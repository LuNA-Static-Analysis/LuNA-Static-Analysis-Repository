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
                        (this->subNameToArgsVector)[subName] = nullptr; // no args
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

                } else std::cout << "ERROR: Unknown CF found" << std::endl;
                    
            }

            std::cout << "> findSubs finished\n\n";
        }

        // scanForDFDecls is a function that scans block for DF declarations
        // DFs, according to LuNA rules, must be declared on the very first line, and no other declarations shall follow
        static std::map<std::string, Identifier*> scanForDFDecls(block* blockobj){
            
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
                        std::cout << "ERROR: found duplicate name in df declaration in block " << blockobj <<
                        ": " << dfName << std::endl;
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
        // if there is none, then it's an error TODO DFR 3 report an error here!
        static std::set<Identifier*> getNamesFromExpression(expr* expression, std::map<std::string, Identifier*> nameTable){

            std::cout << "> getNamesFromExpression called\n\n";

            //TODO DFR 1: make a comparator for Id objects to avoid duplications in a set!
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
                result = getNamesFromExpression(nextExpr, nameTable);
                std::cout << "> getNamesFromExpression finished after luna_cast recursion\n\n";
                return result;
            }

            // DFs could be inside one of the operands
            bin_op* lunaBinOp = dynamic_cast<bin_op*>(expression);
            if (lunaBinOp != NULL){
                std::cout << "> getNamesFromExpression calling recursively for a bin_op (left)\n\n";
                std::set<Identifier*> leftResult = getNamesFromExpression(lunaBinOp->left_, nameTable);
                for (auto j : leftResult) result.insert(j);
                std::cout << "> getNamesFromExpression calling recursively for a bin_op (right)\n\n";
                std::set<Identifier*> rightResult = getNamesFromExpression(lunaBinOp->right_, nameTable);
                for (auto j : rightResult) result.insert(j);
                std::cout << "> getNamesFromExpression finished after bin_op recursion\n\n";
                return result;
            }

            // name found
            // TODO check if id class includes every name (i.e. for counters, let variables etc)
            // perhaps it is required to parse luna_string?
            id* df = dynamic_cast<id*>(expression);
            if (df != NULL){

                simple_id* simpleDF = dynamic_cast<simple_id*>(expression);
                if (simpleDF != NULL){
                    std::string simpleDFName = *(simpleDF->value_->value_);
                    auto base = nameTable.find(simpleDFName);
                    if (base != nameTable.end()){
                        result.insert(new IndexedDFName(simpleDFName, base->second, {}));
                    }
                    return result;
                }

                complex_id* complexDF = dynamic_cast<complex_id*>(expression);
                if (complexDF != NULL){

                    // find out how many indices there are
                    int indices = 0;
                    std::string baseName;
                    while(true){
                        if (dynamic_cast<complex_id*>(complexDF->id_) == nullptr){
                            baseName = complexDF->id_->to_string();
                            break;
                        } else {
                            complexDF = dynamic_cast<complex_id*>(complexDF->id_);
                            indices++;
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
                        result.insert(new IndexedDFName(baseName, base->second, expressionsVector));
                    }

                    return result;
                }

                std::cout << "error in dynamic_cast to id" << std::endl;
                //TODO exception

            }

            std::cout << "> getNamesFromExpression finished (default)\n\n";
            return {}; //TODO add exception

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
                ParsedArguments(std::vector<expr*> rawCallArgs, std::map<std::string, Identifier*> nameTable){
                    positionToIdSet = {};
                    int current = 1;
                    for (auto expression: rawCallArgs){
                        positionToIdSet.insert(std::make_pair(current, getNamesFromExpression(expression, nameTable)));
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
            int currentDepth, std::vector<expr*> callArgs, std::string currentCFName,
            /*for ForVertex*/ Identifier* iterator, expr* leftBorder, expr* rightBorder){

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
                    std::cout << "ERROR: duplicate name declared at " << currentCFName << ": " << identifierName << std::endl;
                }
            }

            // in case of a "sub": add args as an inside Ids
            //TODO segfault here
            if (VertexType = subVF){
                // find if this sub is even declared
                auto temp = subNameToArgsVector.find(currentCFName);
                if (temp != subNameToArgsVector.end()){
                    // get declared arguments vector
                    auto declaredArgsVector = temp->second;
                    // iterate through this vector and for every arg create a SubArgName object
                    for (int i = 0; i < (*declaredArgsVector).size(); i++){
                        auto declaredArg = (*declaredArgsVector)[i];
                        // name of a declared argument
                        std::string identifierName = declaredArg->to_string();
                        // check if this name is already declared or not
                        if (declaredBothIdsMap.find(identifierName) == declaredBothIdsMap.end()){
                            // nameReferenceSet shows, what Identifiers are inside an expression
                            // used in a call of a current sub
                            expr* arg = callArgs[i];
                            // now parse expr and find every its name inside declaredBothIdsMap
                            auto nameReferenceSet = getNamesFromExpression(arg, declaredBothIdsMap);
                            SubArgName* subArgName = new SubArgName(identifierName, nameReferenceSet);
                            declaredInsideIdsMap.insert(std::make_pair(
                                identifierName,
                                subArgName)
                            );
                            declaredBothIdsMap.insert(std::make_pair(
                                identifierName,
                                subArgName)
                            );
                        } else {
                            std::cout << "ERROR: duplicate name of a sub arg at " << currentCFName << ": " << identifierName << std::endl;
                        }
                    }
                } else {
                    std::cout << "ERROR: could not find sub with a name " << currentCFName << std::endl;
                    // this is actually redundant, as check happens below already
                }
            }

            //TODO DFR 9 bounds of "for" also could have DFs inside
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
                    std::cout << "ERROR: duplicate name declared at for: " << iteratorName << std::endl;
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
                    auto parsedArguments = ParsedArguments(rawArguments, declaredBothIdsMap).getMap();

                    // check if names inside every argument are declared
                    for (int i = 1; i <= parsedArguments.size(); i++){ // parsed arguments start from 1
                        std::set<Identifier*> identifierSet = parsedArguments.find(i)->second;
                        // we assume that find() always finds here
                        for (Identifier* identifier: identifierSet){
                            std::string identifierName = (identifier->getType() == indexedDFName) ?
                                (dynamic_cast<IndexedDFName*>(identifier))->getBase()->getName() :
                                identifier->getName();
                            if (declaredBothIdsMap.find(identifierName) == declaredBothIdsMap.end())
                                std::cout << "ERROR: found undeclared name " + identifierName + " in a " + nextCFName + " call" << std::endl;
                        }
                    }

                    if (imports.find(nextCFName) != imports.end()){ // import VF
                        std::cout << "import" << std::endl;

                        tempVertex = enterVF({}, rawArguments, nullptr, currentDepth + 1,
                            importVF, nextCFName, innerStatement->line_,
                            nullptr, nullptr, nullptr);
                        
                    } else { // subVF
                        std::cout << "sub" << std::endl;

                        auto m = subNameToArgsVector.find(nextCFName);
                        if (m != subNameToArgsVector.end()){
                            tempVertex = enterVF({}, rawArguments, (this->subNameToBlock)[nextCFName], currentDepth + 1,
                                subVF, nextCFName, innerStatement->line_,
                                nullptr, nullptr, nullptr);
                        } else {
                            std::cout << "ERROR: could not find sub with a name " << nextCFName << std::endl;
                            currentVertex->addInside(tempVertex);
                            continue;
                        }

                        //================== USE DEF ====================== (for sub, not for import; import is initialized in enterVF)
                    
                        /* initialize current Vertex' use and def sets by looking what call
                           DFs were actually used in that vertex that we are currently handling
                           use and def sets only accept BaseDFNames, so every Name must be
                           converted to its full name */

                        //TODO DFR ALL OF THIS IS NOT NEEDED ANYMORE WITH NEW NAME SYSTEM
                        //USEDEF CHECKS WILL BE DONE BY USING BASENAMES
                        //THIS IS NOT ENTIRELY TRUE
                        //AS WE STILL NEED TO LINK IDFS TO BASE NAMES AND CONSIDER EVERY IDF AND
                        //CHECK IS IT IS USED OR DEFINED
                        
                        /*if (subNameToArgsVector[nextCFName] != nullptr){
                            std::cout << "initializing Vertex' use/defs for sub: " + nextCFName << std::endl;
                            std::vector<param*> definedArgs = *subNameToArgsVector[nextCFName];
                            for (int i = 0; i < definedArgs.size(); i++){
                                std::set<BaseDFName*> tempVertexUseSet = tempVertex->getUseSet();
                                // for every name marked as used in tempVertex (which we just analyzed) we
                                // 
                                for (auto n: tempVertexUseSet){
                                    auto temp = declaredBothIdsMap.find(n->getName());
                                    if (temp != declaredBothIdsMap.end()){

                                    }
                                }

                                std::set<BaseDFName*> tempVertexDefSet = tempVertex->getDefSet();
                                //todo
                            }
                        }*/

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

                    //================== USE DEF ======================

                    /*std::cout << "initializing Vertex for \"for\": " + currentCFName << std::endl;
                    
                    auto tempUseSet = tempVertex->getUseSet();
                    for (auto d: tempUseSet){
                        if (declaredBothIdsMap.find(d) != declaredBothIdsMap.end())
                            currentVertex->addUse(d);
                    }
                    auto tempDefSet = tempVertex->getDefSet();
                    for (auto d: tempDefSet){
                        if (declaredBothIdsMap.find(d) != declaredBothIdsMap.end())
                            currentVertex->addDef(d);
                    }*/
                    // other stuff from parsing sub is not needed as we don't have to go though arguments of a call
                    // same thing with while, if and else

                    currentVertex->addInside(tempVertex);
                    continue;
                }

                //TODO ---- handling other VFs
            }
            std::cout << "> enterVF finished\n\n";
            return currentVertex;

        }

        // enterVF must be called on a vertex' block (initially main's vertex). It recursively goes through each operator in the block,
        // creating a corresponding vertex for each and keeping track of what DFs are use and defined and where exactly, and storing
        // this information in Use and Def sets of a vertice (this is later used in bindVertices() to fully create a graph)
        Vertex* enterVF(std::map<std::string, Identifier*> declaredOutsideIdsMap, std::vector<expr*> callArgs,
                        block* currentBlock, int currentDepth, VertexType vertexType, std::string name, int line,
                        /*for ForVertice:*/ Identifier* iterator, expr* leftBorder, expr* rightBorder){

            std::cout << "> enterVF called\n\n";
            std::cout << "Entering block " << currentBlock << "; name: " + name << std::endl;

            Vertex* currentVertex;
            vertexCount++; // we will create a Vertex itself later in a switch

            switch(vertexType){

                // parse imports here instead of calling enterBlock as with every other operator
                case importVF: {

                    vertices.insert(std::make_pair(vertexCount, new CFVertex(currentDepth, vertexCount, line,
                        name, importVF)));
                    currentVertex = vertices.find(vertexCount)->second;

                    std::cout << "Import entered" << std::endl;
                    for (int i = 1; i <= callArgs.size(); i++){
                        // callNames -- set of Identifiers used in an import call
                        std::set<Identifier*> callNames = getNamesFromExpression(callArgs[i], declaredOutsideIdsMap);
                        switch(importAndPositionToUseDef[name][i]){
                            case use:
                                for (auto i: callNames) {
                                    std::map<BaseDFName*, std::set<int>> roots = i->getRoots();
                                    for (auto r: roots){
                                        for (int size: r.second)
                                            // r.first is a BaseDFName, r.second is a set of sizes (amounts of [])
                                            r.first->addUse(size, currentVertex);
                                    }
                                }
                                break;
                            case def:
                                for (auto i: callNames) {
                                    std::map<BaseDFName*, std::set<int>> roots = i->getRoots();

                                    //todo DFR check if roots.size > 1 or (roots[x].size > 1 and roots.size == 1),
                                    // because it is an error

                                    for (auto r: roots){
                                        for (int size: r.second)
                                            // r.first is a BaseDFName, r.second is a set of sizes (amounts of [])
                                            r.first->addDef(size, currentVertex);
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
                    return enterBlock(subVF, currentBlock, currentVertex, {},
                        currentDepth, callArgs, name,
                        nullptr, nullptr, nullptr);
                }

                case forVF: {

                    vertices.insert(std::make_pair(vertexCount, new ForVertex(currentDepth, vertexCount, line,
                        dynamic_cast<ForId*>(iterator), leftBorder, rightBorder)));
                    currentVertex = vertices.find(vertexCount)->second;

                    std::cout << "For entered" << std::endl;
                    return enterBlock(forVF, currentBlock, currentVertex, declaredOutsideIdsMap,
                        currentDepth, {}, "for",
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

        // this function binds vertices to each other; it is initially called for "main" vertex and uses its
        // "inside" field to call itself recursively on new vertices
        // function initializes "in" and "out" of every vertex
        //TODO DFR: use baseNameSet to bind vertices
        void bindVertices(Vertex* currentVertex){

            // old version:
            /*std::cout << "> bindVertices called\n" << std::endl;
            for (Vertex* internalVertex: currentVertex->getInsideSet()){

                for (BaseDFName* DFName: internalVertex->getUseSet()) // adding every use of every DF
                    //todo
                
                if (internalVertex->getVertexType() != importVF) // vertex has a block, use recursion
                    bindVertices(internalVertex);

            }

            for (Vertex* internalVertex: currentVertex->getInsideSet()){

                for (std::string DFName: internalVertex->getDefSet()){ // now find what defined DFs are used, and where exactly
                    std::vector<Vertex*>* maybeUses = coordinates.getDFUses(DFName);
                    if (maybeUses != NULL){ // found used DF
                        for (Vertex* it: *maybeUses){ // bindVertices current Vertex to all that uses its result
                            internalVertex->addOut(it, DFName);
                            it->addIn(internalVertex, DFName);
                        }
                    } // else DF is unused (this will be checked for later anyway)
                }

            }*/

            // new version:
            // go through all basenames and bind imports depending on what info baseNameSet has
            for (BaseDFName* baseName: baseNameSet){
                std::map<int, std::pair<std::vector<Vertex*>*, std::vector<Vertex*>*>> map = baseName->getMap();
                for (auto entry: map){
                    for (auto use: *(entry.second.first)){
                        for (auto def: *(entry.second.second)){
                            use->addOut(def, baseName);
                            def->addIn(use, baseName);
                        }
                    }
                }
            }

        }

    public:

        // this function recursively goes through DDG and on every namespace checks if any of the DFs
        // are initialized more than once
        // map: name of a DF -> line in code
        //TODO function must return a list of errors in a JSON

        //TODO redo all checkers according to DFR
        void checkMultipleDFInitialization(Vertex* vertex, std::map<std::string, std::vector<int>> initializedDFs){

            /*std::vector<Vertex*> stack = {};

            for (auto v: vertex->getInsideSet()){

                if (v->getInsideSet().size() > 0){ // add vertices inside to stack to analyze them recursively later
                    stack.push_back(v);
                }

                for (auto d: v->getDefSet()){
                    auto it = initializedDFs.find(d);
                    if (it != initializedDFs.end()){
                        it->second.push_back(v->getLine());
                    } else {
                        // no error , first initialization of a DF found;
                        // simply update a map of initialized DFs and their positions
                        std::vector<int> lines = {v->getLine()};
                        initializedDFs.insert(std::make_pair(d, lines));
                    }
                }

            }

            for (auto v: stack){
                if (v->getVertexType() == subVF) { // this vertice does not see DFs outside (sub)
                    checkMultipleDFInitialization(v, {});
                } else { // this vertice sees DFs outside (for, if, while, let)
                    checkMultipleDFInitialization(v, initializedDFs);
                }
            }

            // return errors info
            for (auto it: initializedDFs) {
                if (it.second.size() > 1){
                    std::cout << "Multiple DF initialization -- df " << it.first << " initialized:" << std::endl;
                    for (auto d: it.second){
                        std::cout << "at line " << d << std::endl;
                    }
                }
            }*/

        }

        void checkNonDefinedDFUsage(){//todo redo this (using 4 new containers)
        //problem: launch and see -- foo1 is considered undeclared
        //multiple init works however lol

            /*for (auto v: vertices) { // check every vertice
                auto useSet = v.second->getUseSet();
                auto inSet = v.second->getInSet();
                for (auto d: useSet){ // for every vertice every used DF must exist in some binding (inSet)
                    bool found = false;
                    for (auto b: inSet){
                        if (b.getName() == d){
                            found = true;
                            break;
                        }
                    }
                    if (!found){
                        std::cout << "Usage of a non-defined DF -- df " << d << " at line " << v.second->getLine() << std::endl;
                    }
                } 

            }*/

        }

        void checkUnusedDF(Vertex* currentVertex, std::set<std::string>* currentNamespaceUnusedDFSet){

            /*std::vector<Vertex*> stack = {};

            // refresh current unused DFs
            for (auto d: currentVertex->getDeclaredInsideDFsVector()){//TODO iterators are marked as outside :(
                currentNamespaceUnusedDFSet->insert(d);
            }

            for (auto v: currentVertex->getInsideSet()){
                if (v->getInsideSet().size() > 0){ // add vertices inside to stack to analyze them recursively later
                    stack.push_back(v);
                }

                // now check what DFs are actually used; the 
                auto DFset = v->getUseSet();
                for (auto d: DFset){
                    if (currentNamespaceUnusedDFSet->find(d) != currentNamespaceUnusedDFSet->end()){
                        currentNamespaceUnusedDFSet->erase(d);
                    }
                }

            }

            for (auto v: stack){
                if ((v->getVertexType() != subVF) && (v->getVertexType() != importVF)) { // has block
                    checkUnusedDF(v, currentNamespaceUnusedDFSet);
                } else { // has no block
                    std::set<std::string> temp = {};
                    checkUnusedDF(v, &temp);
                }
            }

            // if after all the checks some DFs are unused -- report it
            if (currentNamespaceUnusedDFSet->size() != 0){
                for (auto d: *currentNamespaceUnusedDFSet){
                    std::cout << "Found unused DF " << d << " in vertex number " << currentVertex->getNumber() << " in line " <<
                    currentVertex->getLine() << std::endl;
                }
            }*/

        }

        // this function accepts list of errors to find and tries to find them in the created graph
        // list consists of Error enums
        void findErrors(Vertex* mainVertex){
            
            checkMultipleDFInitialization(mainVertex, {});

            checkNonDefinedDFUsage();

            std::set<std::string> currentNamespaceUnusedDFs = {};
            checkUnusedDF(mainVertex, &currentNamespaceUnusedDFs);

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
                // TODO throw exception/use error reporter/whatever else
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

            std::cout << "============ Created DDG =============" << std::endl;

            // 4. search for errors

            std::cout << "============ Found errors ============" << std::endl;

            findErrors(mainVertex);

        }

};