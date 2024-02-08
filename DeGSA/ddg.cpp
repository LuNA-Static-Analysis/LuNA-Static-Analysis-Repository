#include "enums.hpp"
#include "vertices.cpp"
#include "ids.cpp"

class DDG {

    private:

        // IMPORTANT NOTE: std::map requires empty constructor; to ignore this requirement you must use insert/find instead of a [] operator

        ast* astobj; // input AST
        
        // list of imports and their positions corresponding to use or def;
        // this is used to determine if argument in a call will be initialized or used;
        // this is initialized in findSubs();
        // positions start with 1;
        std::map<std::string, std::map<int, UseDef>> importAndPositionToUseDef;

        bool mainExists; // for checking if program is correct
        int mainLine; // line in code where main() is called
        block* mainBlock; // needed to start analysis from main

        std::map<std::string, block*> subNameToBlock; // map of structured CF blocks
        std::map<std::string, std::vector<param*>*> subNameToArgsVector; // map of args of subprograms; vector starts from 0

        std::map<int, Vertex*> vertices; // list of all vertices; vertex numeration starts from one
        int vertexCount; // count of vertices

        std::set<std::string> imports; // names of imported functions from C++ (ucodes.cpp)

        std::set<BaseDFName*> baseNameSet; // all names declared after the keyword "df"

        std::vector<std::string> errorReports = {};

        void findSubs(ast* astobj){

            std::cout << "> findSubs called\n\n";

            for (auto subDef: (*(astobj->program_->sub_defs))){

                import* importDecl = dynamic_cast<import*>(subDef);
                luna_sub_def* subDecl = dynamic_cast<luna_sub_def*>(subDef);
                
                if (subDecl != NULL) { // found a structured CF

                    std::cout << "Non-import (structured sub) found: ";
                    std::string subName = *(subDecl->code_id_->get_value());

                    if (subName == "main"){ // main found
                        std::cout << "main() !" << std::endl;
                        this->mainExists = true;
                        this->mainLine = subDecl->line_;
                    } else {
                        std::cout << subName << std::endl;
                    }

                    // save block of this CF to a list in a DDG for later analysis
                    (this->subNameToBlock)[subName] = subDecl->block_;

                    // save args of this CF to use it in enterVF
                    if (subDecl->params_->param_seq_ == nullptr){
                        (this->subNameToArgsVector)[subName] = new std::vector<param*>(); // no args
                    } else {
                        (this->subNameToArgsVector)[subName] = subDecl->params_->param_seq_->params_;
                    }

                } else if (importDecl != NULL) { // found an import

                    std::cout << "Import (atomic sub) found: ";
                    std::string importName = *(importDecl->luna_code_id_->get_value());
                    std::cout << importName << std::endl;
                    (this->imports).insert(importName); // save import for using it later

                    std::vector<code_df_param*>* importArgs = importDecl->params_->seq_->params_;
                    int argPosition = 0;
                    for (auto arg: *importArgs){

                        argPosition++; // importAndPositionToUseDef's positions start with 1
                        std::string argType = arg->type_->to_string();

                        if (argType == "name") { // def

                            std::cout << argPosition << " arg is def-arg\n";
                            importAndPositionToUseDef[importName][argPosition] = def;

                        } else { // use

                            std::cout << argPosition << " arg is use-arg\n";
                            importAndPositionToUseDef[importName][argPosition] = use;

                        }
                        
                    }

                } else {
                    errorReports.push_back("ERROR: unknown CF of name " + subDef->to_string() 
                    + " found.\nFunction: findSubs()\n");
                }
                    
            }

            std::cout << "> findSubs finished\n\n";
        }

        // scanForDFDecls is a function that scans block for DF declarations
        // DFs, according to LuNA rules, must be declared on the very first line, and no other declarations shall follow
        std::map<std::string, Identifier*> scanForDFDecls(block* blockobj){
            
            std::cout << "> scanForDFDecls called\n";
            std::map<std::string, Identifier*> DFDecls = {};

            // short-circuit magic for null pointer checking:
            if ((blockobj->opt_dfdecls_ != NULL) && (blockobj->opt_dfdecls_->dfdecls_ != NULL)) { // found some DF declarations

                // get names of declared DFs
                std::vector<luna_string*> DFNames = *(blockobj->opt_dfdecls_->dfdecls_->name_seq_->names_);
                for (luna_string* currentDFDecl: DFNames){
                    std::string dfName = *(currentDFDecl->value_);
                    if (DFDecls.find(dfName) == DFDecls.end()){
                        DFDecls.insert(std::make_pair(dfName, new BaseDFName(dfName)));
                    } else {
                        std::string report = "ERROR: found duplicate name in df declaration in block " + std::to_string((long long)blockobj) +
                        ": " + dfName + "\n";
                        this->errorReports.push_back(report);
                    }
                }

                std::cout << "DFs in block " << blockobj << ":";
                for (auto currentDFDecl: DFDecls){
                    std::cout << " " << currentDFDecl.second->getName();
                }
                std::cout << std::endl;

            } else {
                std::cout << "No DF declarations found" << std::endl;
            }

            std::cout << "> scanForDFDecls finished\n\n";
            return DFDecls;

        }

        // this function gets an expression, recursively goes through it and
        // returns a set of Ids that are used in this expression
        // nameTable stores information about what Ids are visible currently, and we can
        // find the Id object by its name
        // if there is none, then it's an error
        std::set<Identifier*> getNamesFromExpression(expr* expression, std::map<std::string, Identifier*> nameTable, int line){

            std::cout << "> getNamesFromExpression called\n\n";

            std::set<Identifier*> result = {};

            // ignore all this as we are looking only for DFs
            luna_string* lunaString = dynamic_cast<luna_string*>(expression);
            if (lunaString != NULL){
                std::cout << "> getNamesFromExpression finished (non-DF string found)\n\n";
                return result;
            }
            integer* lunaInteger = dynamic_cast<integer*>(expression);
            if (lunaInteger != NULL) {
                std::cout << "> getNamesFromExpression finished (integer ignored)\n\n";
                return result;
            }
            real* lunaReal = dynamic_cast<real*>(expression);
            if (lunaReal != NULL) {
                std::cout << "> getNamesFromExpression finished (real ignored)\n\n";
                return result;
            }

            // DFs could be inside the cast
            luna_cast* lunaCast = dynamic_cast<luna_cast*>(expression);
            if (lunaCast != NULL){
                expr* nextExpr = lunaCast->expr_;
                std::cout << "> getNamesFromExpression calling recursively for a luna_cast\n\n";
                result = getNamesFromExpression(nextExpr, nameTable, line);
                std::cout << "> getNamesFromExpression finished after luna_cast recursion\n\n";
                return result;
            }

            // DFs could be inside one of the operands
            bin_op* lunaBinOp = dynamic_cast<bin_op*>(expression);
            if (lunaBinOp != NULL){
                std::cout << "> getNamesFromExpression calling recursively for a bin_op (left)\n\n";
                std::set<Identifier*> leftResult = getNamesFromExpression(lunaBinOp->left_, nameTable, line);
                for (auto j : leftResult) result.insert(j);
                std::cout << "> getNamesFromExpression calling recursively for a bin_op (right)\n\n";
                std::set<Identifier*> rightResult = getNamesFromExpression(lunaBinOp->right_, nameTable, line);
                for (auto j : rightResult) result.insert(j);
                std::cout << "> getNamesFromExpression finished after bin_op recursion\n\n";
                return result;
            }

            // name found
            id* df = dynamic_cast<id*>(expression);
            if (df != NULL){

                simple_id* simpleDF = dynamic_cast<simple_id*>(expression);
                if (simpleDF != NULL){
                    std::string simpleDFName = *(simpleDF->value_->value_);
                    auto base = nameTable.find(simpleDFName);
                    if (base != nameTable.end()){
                        result.insert(new IndexedDFName(simpleDFName, base->second, {}));
                    } else {
                        std::string report = "ERROR: no name \"" + simpleDFName + "\" found!" + "\n";
                        errorReports.push_back(report);
                    }

                    std::cout << "> getNamesFromExpression finished (simple DF)\n\n";
                    return result;
                }

                complex_id* complexDF = dynamic_cast<complex_id*>(expression);
                if (complexDF != NULL){

                    // find out how many indices there are
                    int indices = 0;
                    std::string baseName;
                    while(true){
                        indices++;
                        if (dynamic_cast<complex_id*>(complexDF->id_) == nullptr){
                            baseName = complexDF->id_->to_string();
                            break;
                        } else {
                            complexDF = dynamic_cast<complex_id*>(complexDF->id_);
                        }
                    }

                    // now create an IndexedDFName and initialize it
                    std::vector<expr*> expressionsVector(indices);
                    complexDF = dynamic_cast<complex_id*>(expression);
                    for (int i = 0; i < indices; i++){
                        expressionsVector[i] = complexDF->expr_;
                        complexDF = dynamic_cast<complex_id*>(complexDF->id_);
                    }

                    auto base = nameTable.find(baseName);
                    if (base != nameTable.end()){
                        IndexedDFName* temp = new IndexedDFName(baseName, base->second, expressionsVector);
                        result.insert(temp);
                    } else {
                        std::string report = "ERROR: no name \"" + baseName + "\" found at line " + std::to_string(line) + "\n";
                        errorReports.push_back(report);
                    }

                    std::cout << "> getNamesFromExpression finished (indexed DF)\n\n";
                    return result;
                }

                std::string report = "ERROR: dynamic_cast to id at line " + std::to_string(line) + "\n";
                errorReports.push_back(report);
            }

            std::cout << "> getNamesFromExpression finished with errors\n\n";
            return {};

        }

        // this class is used to map arguments of the cf call to the local identifiers inside cf's block
        // it's being done by mapping position of argument in a call to a set of DFs used in the expression
        // in a call at a specified position
        // TODO currently it's used only for declaration checking
        class ParsedArguments {
            private:
                // maps position to set of DFs being on this position
                // set is required as expressions, being a single argument, may contain multiple DFs
                std::map<int, std::set<Identifier*>> positionToIdSet; // starts from 1 //TODO redo to 0
            
            public:
                ParsedArguments(std::vector<expr*> rawCallArgs, std::map<std::string, Identifier*> nameTable, DDG &ddg, int line){
                    positionToIdSet = {};
                    int current = 1;
                    for (auto expression: rawCallArgs){
                        positionToIdSet.insert(std::make_pair(current, ddg.getNamesFromExpression(expression, nameTable, line)));
                        current++;
                    }
                }

                std::map<int, std::set<Identifier*>> getMap(){
                    return positionToIdSet;
                }

        };

        // enterBlock must be called inside enterVF whenever enterVF encounters a block
        // declaredOutsideIdsMap must have no duplicates!
        //
        // this function exists to avoid code duplication while parsing for, while, sub and other operators with blocks
        // however, one-for-all function is still not great (for example, some arguments are used exclusively with "for" block)
        //TODO what to do with this?
        Vertex* enterBlock(VertexType VertexType, block* currentBlock, Vertex* currentVertex, std::map<std::string, Identifier*> declaredOutsideIdsMap,
            int currentDepth, std::vector<expr*> callArgs, std::vector<SubArgName*> declaredArgs, std::string currentCFName,
            /*for ForVertex*/ Identifier* iterator, expr* leftBorder, expr* rightBorder){

            std::cout << "> enterBlock called\n" << std::endl;
            std::cout << "Entering block " << currentBlock << "; name: " + currentCFName << std::endl;

            /* iterate through statements, collect vertices and their use-defs by calling enterVF on each,
            initialize currentVertex' use-defs and return it */
            int statementNumber = 0; // for logging

            /* we have 3 DF containers; checking for duplicates is done using map;
               we can get identifiers [objects] by their name
            1. declaredOutsideIdsMap -- 
                has no duplicates;
                needed for creation of 3., comes as an argument of an enterBlock(), is empty for imports and subs
            2. declaredInsideIdsMap -- 
                has no duplicates;
                needed for creation of 3., comes from checking "df" statement in current block, and also
                has letIds, forId and subArgNames (at least)
                TODO check if it is redundant or not, as we have Both and Outside already
            3. declaredBothIdsMap -- 
                has no duplicates;
                needed for error checking later and for creating bindings properly;
                made as concatenation of 1. and 2.*/

            // copy-assignment (at least it must be and it should be)
            std::map<std::string, Identifier*> declaredBothIdsMap = declaredOutsideIdsMap;

            /* find DF declarations in current block; we can declare DF once in every block!
            scanForDFDecls returns map with no duplicates */
            std::map<std::string, Identifier*> declaredInsideIdsMap = scanForDFDecls(currentBlock);
            // update declaredBothIdsMap with newly declared base names
            for (auto i: declaredInsideIdsMap){
                std::string identifierName = i.first;
                BaseDFName* identifier = dynamic_cast<BaseDFName*>(i.second);
                if (declaredBothIdsMap.find(identifierName) == declaredBothIdsMap.end()){
                    declaredBothIdsMap.insert(std::make_pair(
                        identifierName,
                        identifier)
                    );
                    this->baseNameSet.insert(identifier);
                } else {
                    std::string report = "ERROR: duplicate name declared at " + currentCFName + ": " + identifierName + "\n";
                    errorReports.push_back(report);
                }
            }

            // in case of a "sub": add args as an inside Ids
            if (VertexType == subVF){
                auto temp = subNameToArgsVector.find(currentCFName);
                if (temp != subNameToArgsVector.end()){
                    for (SubArgName* subArgName: declaredArgs){
                        declaredInsideIdsMap.insert(std::make_pair(
                            subArgName->getName(),
                            subArgName)
                        );
                        declaredBothIdsMap.insert(std::make_pair(
                            subArgName->getName(),
                            subArgName)
                        );
                    }
                }
            }

            //TODO DFR bounds of "for" also could have DFs inside
            // in case of a "for": add it as an inside Id
            if (VertexType == forVF){
                std::string iteratorName = iterator->getName();
                if (declaredBothIdsMap.find(iterator->getName()) == declaredBothIdsMap.end()){
                    ForId* forId = new ForId(iteratorName, leftBorder, rightBorder);
                    declaredInsideIdsMap.insert(std::make_pair(
                        iteratorName,
                        forId)
                    );
                    declaredBothIdsMap.insert(std::make_pair(
                        iteratorName,
                        forId)
                    );
                } else {
                    std::string report = "ERROR: duplicate name declared at for: " + iteratorName + "\n";
                    errorReports.push_back(report);
                }
            }
            // same with let variables TODO

            // initialize 3 containers:
            currentVertex->setDeclaredInsideIdsMap(declaredInsideIdsMap);
            currentVertex->setDeclaredOutsideIdsMap(declaredOutsideIdsMap);
            currentVertex->setDeclaredBothIdsMap(declaredBothIdsMap);

            Vertex* tempVertex; // this will be used to store Vertex to return from enterVF()

            // now iterate through current block's statements
            for (statement* innerStatement: *(currentBlock->statement_seq_->statements_)){
                statementNumber++;
                std::cout << "Statement number " << statementNumber << ": " << std::endl;

                // ---- handling cf
                cf_statement* innerStatementFunctionVF = dynamic_cast<cf_statement*>(innerStatement);
                if (innerStatementFunctionVF != NULL){
                    std::string nextCFName = *(innerStatementFunctionVF->code_id_->value_);

                    //  here we parse expressions used as arguments in this CF call
                    //  we check if they are declared, and then pass map (position -> DFs used in this position) to
                    //  enterVF

                    // vector of call args (expressions; need to parse)
                    std::vector<expr*> rawArguments = *(innerStatementFunctionVF->opt_exprs_->exprs_seq_->expr_);
                    auto parsedArguments = ParsedArguments(rawArguments, declaredBothIdsMap, *this, innerStatementFunctionVF->line_).getMap();

                    // check if names inside every argument are declared
                    for (int i = 1; i <= parsedArguments.size(); i++){ // parsed arguments start from 1
                        std::set<Identifier*> identifierSet = parsedArguments.find(i)->second;
                        // we assume that find() always finds here
                        for (Identifier* identifier: identifierSet){
                            std::string identifierName = (identifier->getType() == indexedDFName) ?
                                (dynamic_cast<IndexedDFName*>(identifier))->getBase()->getName() :
                                identifier->getName();
                            if (declaredBothIdsMap.find(identifierName) == declaredBothIdsMap.end()){
                                std::string report = "ERROR: found undeclared name " + identifierName + " in a " + nextCFName + " call\n";
                                errorReports.push_back(report);
                            }
                        }
                    }

                    if (imports.find(nextCFName) != imports.end()){ // import VF
                        std::cout << "import" << std::endl;

                        tempVertex = enterVF(declaredBothIdsMap, rawArguments, nullptr, currentDepth + 1,
                            importVF, nextCFName, innerStatement->line_,
                            nullptr, nullptr, nullptr);
                        
                    } else { // subVF
                        std::cout << "sub" << std::endl;

                        auto m = subNameToArgsVector.find(nextCFName);
                        if (m != subNameToArgsVector.end()){
                            tempVertex = enterVF(declaredBothIdsMap, rawArguments, (this->subNameToBlock)[nextCFName], currentDepth + 1,
                                subVF, nextCFName, innerStatement->line_,
                                nullptr, nullptr, nullptr);
                        } else {
                            std::string report = "ERROR: could not find sub with a name " + nextCFName + "\n";
                            errorReports.push_back(report);
                            currentVertex->addInside(tempVertex);
                            continue;
                        }

                        // remember that every DF inside [] is considered as being "used"
                        for (int i = 0; i < callArgs.size(); i++){
                            std::set<Identifier*> callNames = getNamesFromExpression(callArgs[i], declaredOutsideIdsMap, innerStatementFunctionVF->line_);
                            for (auto callName: callNames){
                                if (callName->getType() == indexedDFName){
                                    IndexedDFName* idfn = dynamic_cast<IndexedDFName*>(callName);
                                    for (expr* expressionInsideIndices: idfn->getExpressionsVector()){
                                        std::set<Identifier*> usedNames = getNamesFromExpression(expressionInsideIndices, declaredOutsideIdsMap, innerStatementFunctionVF->line_);
                                        for (auto usedName: usedNames){
                                            auto roots = usedName->getRoots();
                                            for (auto rootAndSize: roots){
                                                if (rootAndSize.first->getType() == baseDFName){
                                                    BaseDFName* root = dynamic_cast<BaseDFName*>(rootAndSize.first);
                                                    root->addUse(rootAndSize.second, currentVertex);
                                                }
                                            }
                                        }
                                    }
                                }
                            }   
                        }

                    }

                    currentVertex->addInside(tempVertex);
                    continue;
                }

                // ---- handling for
                for_statement* innerStatementForVF = dynamic_cast<for_statement*>(innerStatement);
                if (innerStatementForVF != NULL){
                    
                    ForId* nextIterator = new ForId(innerStatementForVF->name_->to_string(), 
                        innerStatementForVF->expr_1_, innerStatementForVF->expr_2_);
                    tempVertex = enterVF(declaredBothIdsMap, {}, innerStatementForVF->block_, currentDepth + 1, 
                        forVF, "for", innerStatement->line_, nextIterator,
                        innerStatementForVF->expr_1_, innerStatementForVF->expr_2_);

                    currentVertex->addInside(tempVertex);
                    continue;
                }

                //TODO ---- handling other VFs
            }
            std::cout << "> enterBlock finished\n" << std::endl;
            return currentVertex;

        }

        // enterVF must be called on a vertex' block (initially main's vertex). It recursively goes through each operator in the block,
        // creating a corresponding vertex for each and keeping track of what DFs are use and defined and where exactly, and storing
        // this information in Use and Def sets of a vertice (this is later used in bindVertices() to fully create a graph)
        Vertex* enterVF(std::map<std::string, Identifier*> declaredOutsideIdsMap, std::vector<expr*> callArgs,
                        block* currentBlock, int currentDepth, VertexType vertexType, std::string name, int line,
                        /*for ForVertice:*/ Identifier* iterator, expr* leftBorder, expr* rightBorder){

            std::cout << "> enterVF called\n\n";

            Vertex* currentVertex;
            vertexCount++; // we will create a Vertex itself later in a switch

            switch(vertexType){

                // parse imports here instead of calling enterBlock as with every other operator
                case importVF: {

                    vertices.insert(std::make_pair(vertexCount, new CFVertex(currentDepth, vertexCount, line,
                        name, importVF)));
                    currentVertex = vertices.find(vertexCount)->second;

                    std::cout << "Import entered" << std::endl;
                    for (int i = 0; i < callArgs.size(); i++){

                        // callNames -- set of Identifiers used in the current argument of an import call
                        // example: init(a + b[1], b[2]);
                        // callNames of first argument: IndexedDFName* a, IndexedDFName* b[1]
                        std::set<Identifier*> callNames = getNamesFromExpression(callArgs[i], declaredOutsideIdsMap, line);

                        switch(importAndPositionToUseDef[name][i + 1]){ // i + 1 because map's indices start from 1
                            // initialize all BaseDFNames uses and defs that are participating in this import
                            case use:
                                for (auto i: callNames) {
                                    std::set<std::pair<Identifier*, int>> roots = i->getRoots();
                                    for (auto r: roots){
                                        if (r.first->getType() == baseDFName){
                                            std::cout << "added use to baseName " << r.first->getName() << " of size " << r.second << " in a vertex " << currentVertex << std::endl;
                                            (dynamic_cast<BaseDFName*>(r.first))->addUse(r.second, currentVertex);
                                        }
                                    }

                                    // remember that every DF inside [] is considered as being "used"
                                    if (i->getType() == indexedDFName){
                                        IndexedDFName* idfn = dynamic_cast<IndexedDFName*>(i);
                                        for (expr* expressionInsideIndices: idfn->getExpressionsVector()){
                                            std::set<Identifier*> usedNames = getNamesFromExpression(expressionInsideIndices, declaredOutsideIdsMap, line);
                                            for (auto usedName: usedNames){
                                                auto roots = usedName->getRoots();
                                                for (auto rootAndSize: roots){
                                                    if (rootAndSize.first->getType() == baseDFName){
                                                        BaseDFName* root = dynamic_cast<BaseDFName*>(rootAndSize.first);
                                                        root->addUse(rootAndSize.second, currentVertex);
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                                break;
                            case def:
                                for (auto i: callNames) {
                                    std::set<std::pair<Identifier*, int>> roots = i->getRoots();
                                    // check for not attempting to initialize unsuitable expression:
                                    // 1. callNames.size() must be <= 1
                                    // 2. roots.size() must be <= 1
                                    // 3. TODO DFR also must be applied to constants, not only identifiers
                                    if (roots.size() > 1 || callNames.size() > 1){
                                        std::string report = "ERROR: trying to define an unsuitable expression!\n";
                                        errorReports.push_back(report);
                                        continue;
                                    }

                                    for (auto r: roots){
                                        if (r.first->getType() == baseDFName){
                                            std::cout << "added def to baseName " << r.first->getName() << " of size " << r.second << " in a vertex " << currentVertex << std::endl;
                                            (dynamic_cast<BaseDFName*>(r.first))->addDef(r.second, currentVertex);
                                        }
                                    }

                                    // remember that every DF inside [] is considered as being "used"
                                    if (i->getType() == indexedDFName){
                                        IndexedDFName* idfn = dynamic_cast<IndexedDFName*>(i);
                                        for (expr* expressionInsideIndices: idfn->getExpressionsVector()){
                                            std::set<Identifier*> usedNames = getNamesFromExpression(expressionInsideIndices, declaredOutsideIdsMap, line);
                                            for (auto usedName: usedNames){
                                                auto roots = usedName->getRoots();
                                                for (auto rootAndSize: roots){
                                                    if (rootAndSize.first->getType() == baseDFName){
                                                        BaseDFName* root = dynamic_cast<BaseDFName*>(rootAndSize.first);
                                                        root->addUse(rootAndSize.second, currentVertex);
                                                    }
                                                }
                                            }
                                        }
                                    }

                                }
                                break;
                            default:
                                std::cout << "enterVF -- import: found DF with unexpected UseDef!" << std::endl;
                        }
                    }
                    std::cout << "> enterVF finished\n\n";
                    return currentVertex;
                }

                case subVF: {
                    
                    vertices.insert(std::make_pair(vertexCount, new CFVertex(currentDepth, vertexCount, line,
                        name, subVF)));
                    currentVertex = vertices.find(vertexCount)->second;
                    
                    std::cout << "Sub entered" << std::endl;
                    std::vector<SubArgName*> declaredArgs = {};
                    // in case of a "sub": add args as an inside Ids and map them to call args
                    // find if this sub is even declared
                    auto temp = subNameToArgsVector.find(name);
                    if (temp != subNameToArgsVector.end()){
                        // get declared arguments vector
                        auto declaredArgsVector = temp->second;
                        // iterate through this vector and for every arg create a SubArgName object
                        for (int i = 0; i < (*declaredArgsVector).size(); i++){
                            auto declaredArg = (*declaredArgsVector)[i];
                            // name of a declared argument
                            std::string identifierName = *(declaredArg->name_->value_);
                            // check if this name is already declared or not
                            if (declaredOutsideIdsMap.find(identifierName) == declaredOutsideIdsMap.end()){
                                // nameReferenceSet shows, what Identifiers are inside an expression
                                // used in a call of a current sub
                                expr* arg = callArgs[i];
                                // now parse expr and find every its name inside declaredBothIdsMap
                                auto nameReferenceSet = getNamesFromExpression(arg, declaredOutsideIdsMap, line);
                                SubArgName* subArgName = new SubArgName(identifierName, nameReferenceSet);
                                declaredArgs.push_back(subArgName);
                            } else {
                                std::string report = "ERROR: duplicate name of a sub arg at " + name + ": " + identifierName + "\n";
                                errorReports.push_back(report);
                            }
                        }
                    } else {
                        //std::cout << "ERROR: could not find sub with a name " << name << std::endl;
                        // this is actually redundant, as check happens below already
                    }

                    std::cout << "> enterVF finished\n\n";
                    return enterBlock(subVF, currentBlock, currentVertex, {},
                        currentDepth, callArgs, declaredArgs, name,
                        nullptr, nullptr, nullptr);
                }

                case forVF: {

                    vertices.insert(std::make_pair(vertexCount, new ForVertex(currentDepth, vertexCount, line,
                        dynamic_cast<ForId*>(iterator), leftBorder, rightBorder)));
                    currentVertex = vertices.find(vertexCount)->second;

                    std::cout << "For entered" << std::endl;
                    std::cout << "> enterVF finished\n\n";
                    return enterBlock(forVF, currentBlock, currentVertex, declaredOutsideIdsMap,
                        currentDepth, {}, {}, "for",
                        iterator, leftBorder, rightBorder);
                }

                //TODO other VFs

                default: {
                    std::cout << "enterVF: unsupported operator found" << std::endl;
                    std::cout << "> enterVF finished\n\n";
                    return nullptr;
                }

            }

        }

        // this function binds vertices to each other
        // currently function initializes "in" and "out" of imports using baseNameSet
        void bindVertices(Vertex* currentVertex){
            // go through all basenames and bind imports depending on what info baseNameSet has
            for (BaseDFName* baseName: baseNameSet){
                std::map<int, std::pair<std::vector<Vertex*>*, std::vector<Vertex*>*>> map = baseName->getMap();
                for (auto entry: map){
                    for (auto use: *(entry.second.first)){
                        for (auto def: *(entry.second.second)){
                            use->addIn(def, baseName);
                            def->addOut(use, baseName);
                        }
                    }
                }
            }

        }

    public:

        //TODO docs
        //TODO function must return a list of errors in a JSON
        void checkMultipleDFInitialization(){
            for (BaseDFName* bn: baseNameSet){
                auto bnMap = bn->getMap();
                for (auto sizeAndUseDefs: bnMap){
                    int size = sizeAndUseDefs.first;
                    if (size == 0){
                        std::vector<Vertex*> defs = *(sizeAndUseDefs.second.second);
                        if (defs.size() > 1){
                            std::string report = "ERROR: multiple initialization of a DF " + bn->getName() + " in lines:\n";
                            for (auto def: defs){
                                report += (std::to_string(def->getLine()) + " ");
                            }
                            report += "\n";
                            errorReports.push_back(report);
                        }
                    } else {
                        //TODO add warnings?
                    }
                }
            }

        }

        //TODO docs
        void checkUnusedDF(){
            for (BaseDFName* bn: baseNameSet){
                auto bnMap = bn->getMap();
                for (auto sizeAndUseDefs: bnMap){
                    int size = sizeAndUseDefs.first;
                    std::vector<Vertex*> uses = *(sizeAndUseDefs.second.first);
                    if (uses.size() == 0){
                        std::string report = "ERROR: unused DF " + bn->getName() + " with " + std::to_string(size) + " indices\n";
                        errorReports.push_back(report);
                    }
                }
                if (bnMap.size() == 0){
                    std::string report = "ERROR: unused base name DF " + bn->getName() + "\n";
                    errorReports.push_back(report);
                }
            }
        }

        // this function accepts list of errors to find and tries to find them in the created graph
        // list consists of Error enums
        void findErrors(){
            
            checkMultipleDFInitialization();

            checkUnusedDF();

            //TODO attempt to use unitinitialized DF

            //TODO cyclic dependence

        }

        DDG(ast* astObjectIn){
            
            this->vertexCount = 0;
            this->imports = {};
            this->vertices = {};

            this->astobj = astObjectIn;

            this->mainExists = false;

            this->importAndPositionToUseDef = {};

            this->subNameToBlock = {};
            this->subNameToArgsVector = {};

            this->baseNameSet = {};

            //this->mainBlock

            // 1. find use- and def- atomic CFs

            std::cout << "============ Creating DDG ============" << std::endl;

            this->findSubs(astobj);

            if (!(this->mainExists)){
                std::cout << "ERROR: No main found" << std::endl;
                return;
            }

            std::set<std::string> emptySet = {};

            // 2. create all the vertices

            Vertex* mainVertex = enterVF({}, {}, (this->subNameToBlock)["main"], 1, subVF, "main", mainLine, nullptr, nullptr, nullptr);

            std::cout << "Created a [MAIN] vertex number " << this->vertexCount
                      << " with a type " << (this->vertices).find(1)->second->getVertexType()
                      << " and an address of " << &((this->vertices).find(1)->second) << std::endl;

            // 3. bindVertices vertices to eachother
            bindVertices(mainVertex);

            std::cout << "Total vertices: " << vertexCount << std::endl << std::endl; 
            for (int i = 1; i <= vertexCount; i++){

                vertices.find(i)->second->printInfo();
                std::cout << std::endl;

            }

            std::cout << "BaseDFNames:" << std::endl;
            for (BaseDFName* bn: baseNameSet){
                std::cout << std::endl;
                std::cout << "Name: " << bn->getName() << std::endl;
                //std::cout << "Declared: " << bn-> << std::endl; //TODO create declaration stack for BaseDFName
                auto map = bn->getMap(); // 1 = use, 2 = def
                for (auto m: map){
                    std::cout << "Size: " << m.first << std::endl;
                    std::pair<std::vector<Vertex*>*, std::vector<Vertex*>*> pair = m.second;
                    std::cout << "Uses: ";
                    for (auto u: *(pair.first)){
                        std::cout << u << " ";
                    }
                    std::cout << std::endl;
                    std::cout << "Defs: ";
                    for (auto d: *(pair.second)){
                        std::cout << d << " ";
                    }
                    std::cout << std::endl;
                }
            }

            std::cout << "============ Created DDG =============" << std::endl;

            // 4. search for errors

            std::cout << "======== Searching for errors ========" << std::endl;

            findErrors();

            for (auto r: errorReports){
                std::cout << r << std::endl;
            }

        }

};