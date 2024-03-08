#include "enums.hpp"
#include "vertices.cpp"
#include "ids.cpp"
#include "expr.cpp"

#include <chrono>

using ns = std::chrono::nanoseconds;

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
                        std::cout << "main()!" << std::endl;
                        this->mainExists = true;
                        this->mainLine = subDecl->line_;//TODO this is 0 for some reason; perhaps AST failure
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
        std::map<std::string, Identifier*> scanForDFDecls(block* blockobj /* todo int line*/){
            
            std::cout << "> scanForDFDecls called\n";
            std::map<std::string, Identifier*> DFDecls = {};

            // short-circuit magic for null pointer checking:
            if ((blockobj->opt_dfdecls_ != NULL) && (blockobj->opt_dfdecls_->dfdecls_ != NULL)) { // found some DF declarations
        
                // get names of declared DFs
                //TODO check that lines are correct
                std::vector<luna_string*> DFNames = *(blockobj->opt_dfdecls_->dfdecls_->name_seq_->names_);
                for (luna_string* currentDFDecl: DFNames){
                    std::string dfName = *(currentDFDecl->value_);
                    if (DFDecls.find(dfName) == DFDecls.end()){
                        DFDecls.insert(std::make_pair(dfName, new BaseDFName(dfName, blockobj->opt_dfdecls_->dfdecls_->line_)));//TODO :)
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

        // enterBlock must be called inside enterVF whenever enterVF encounters a block
        // declaredOutsideIdsMap must have no duplicates!
        //
        // this function exists to avoid code duplication while parsing for, while, sub and other operators with blocks
        // however, one-for-all function is still not great (for example, some arguments are used exclusively with "for" block)
        //TODO what to do with this?
        Vertex* enterBlock(VertexType VertexType, block* currentBlock, Vertex* currentVertex, std::map<std::string, Identifier*> declaredOutsideIdsMap,
            int currentDepth, std::vector<Expression*> callArgs, std::vector<Identifier*> declaredArgs, std::string currentCFName){

            std::cout << "> enterBlock called\n" << std::endl;
            std::cout << "Entering block " << currentBlock << "; name: " + currentCFName << std::endl;

            /* iterate through statements, collect vertices and their use-defs by calling enterVF on each,
            initialize currentVertex' use-defs and return it */
            int statementNumber = 0; // for logging

            //TODO REDO THESE DOCS
            /* we have 3 DF containers; checking for duplicates is done using map;
               we can get identifiers [objects] by their name
            1. declaredOutsideIdsMap -- 
                has no duplicates;
                needed for creation of 3., comes as an argument of an enterBlock(), is empty for imports and subs
            2. declaredInsideIdsMap -- 
                has no duplicates;
                needed for creation of 3., comes from checking "df" statement in current block, and also
                has letIds, forIteratorName and subArgNames (at least)
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

                    /* here we parse expressions used as arguments in this CF call;
                     we check if they are declared at all (inside Expression constructor), and then
                     pass them to enterVF (as callArgs) */

                    // vector of call args of type expr
                    std::vector<expr*> rawArguments = *(innerStatementFunctionVF->opt_exprs_->exprs_seq_->expr_);

                    // create Expression objects for call args
                    std::vector<Expression*> callArgs = {};
                    for (int i = 0; i < rawArguments.size(); i++){
                        Expression* expression = new Expression(rawArguments[i], declaredBothIdsMap, &errorReports);
                        callArgs.push_back(expression);
                    }

                    if (imports.find(nextCFName) != imports.end()){ // import VF
                        std::cout << "import" << std::endl;

                        tempVertex = enterVF(declaredBothIdsMap, callArgs, nullptr, currentDepth + 1,
                            importVF, nextCFName, innerStatement->line_,
                            /* callerVertex */ currentVertex, innerStatement);
                        
                    } else { // subVF
                        std::cout << "sub" << std::endl;

                        auto m = subNameToArgsVector.find(nextCFName);
                        if (m != subNameToArgsVector.end()){
                            
                            tempVertex = enterVF(declaredBothIdsMap, callArgs, (this->subNameToBlock)[nextCFName], currentDepth + 1,
                                subVF, nextCFName, innerStatement->line_,
                                /* callerVertex */ currentVertex, innerStatement);
                            
                        } else {
                            std::cout << "INTERNAL ERROR: no sub with name " << nextCFName << " found" << std::endl;
                            std::string report = "ERROR: could not find sub with a name " + nextCFName + "\n";
                            errorReports.push_back(report);
                            currentVertex->addInside(tempVertex);
                            continue;
                        }

                    }

                    currentVertex->addInside(tempVertex);
                    continue;
                }

                // ---- handling for
                for_statement* innerStatementForVF = dynamic_cast<for_statement*>(innerStatement);
                if (innerStatementForVF != NULL){

                    tempVertex = enterVF(declaredBothIdsMap, {}, innerStatementForVF->block_, currentDepth + 1, 
                        forVF, "for", innerStatement->line_,
                        /* callerVertex */ currentVertex, innerStatement);

                    currentVertex->addInside(tempVertex);
                    continue;
                }

                // ---- handling while
                // example: while (128 < 129), i = 0 .. out N { /* body */ }
                while_statement* innerStatementWhileVF = dynamic_cast<while_statement*>(innerStatement);
                if (innerStatementWhileVF != NULL){
                    
                    tempVertex = enterVF(declaredBothIdsMap, {}, innerStatementWhileVF->block_, currentDepth + 1, 
                        whileVF, "while", innerStatement->line_,
                        /* callerVertex */ currentVertex, innerStatement);

                    currentVertex->addInside(tempVertex);
                    continue;
                }

                // ---- handling if
                // example: if 128 < 129 { /* body */ }
                if_statement* innerStatementIfVF = dynamic_cast<if_statement*>(innerStatement);
                if (innerStatementIfVF != NULL){

                    tempVertex = enterVF(declaredBothIdsMap, {}, innerStatementIfVF->block_, currentDepth + 1, 
                        ifVF, "if", innerStatement->line_,
                        /* callerVertex */ currentVertex, innerStatement);

                    currentVertex->addInside(tempVertex);
                    continue;
                }

                // ---- handling let
                // example: let b = a[1], message = "Success" { /* body */ }
                let_statement* innerStatementLetVF = dynamic_cast<let_statement*>(innerStatement);
                if (innerStatementLetVF != NULL){

                    tempVertex = enterVF(declaredBothIdsMap, {}, innerStatementLetVF->block_, currentDepth + 1, 
                        letVF, "let", innerStatement->line_,
                        /* callerVertex */ currentVertex, innerStatement);

                    currentVertex->addInside(tempVertex);
                    continue;
                }

                //TODO ---- handling other VFs
            }
            std::cout << "> enterBlock finished\n" << std::endl;
            return currentVertex;

        }

        // enterVF must be called on a vertex (initially main() vertex). It parses imports and saves information about subs (using enterBlock());
        // for other VFs simply calls enterBlock(). enterVF() creats a corresponding vertex for each VF and keeps track of what DFs are use and defined and where exactly, and storing
        // this information in Use and Def sets of a vertice (this is later used in bindVertices() to fully create a graph)
        // Name objects (except for SubArgNames) are being created in enterBlock(), Vertex objects are being created in enterVF()
        Vertex* enterVF(std::map<std::string, Identifier*> declaredOutsideIdsMap, std::vector<Expression*> callArgs,
                        block* currentBlock, int currentDepth, VertexType vertexType,
                        std::string name, int line, Vertex* callerVertex, statement* innerStatement){

            std::cout << "> enterVF called\n\n";

            Vertex* currentVertex;
            vertexCount++; // we will create a Vertex itself later in a switch

            switch(vertexType){

                // parse imports here instead of calling enterBlock as with every other operator
                case importVF: {

                    vertices.insert(std::make_pair(vertexCount, new CFVertex(currentDepth, vertexCount, line,
                        name, importVF, callerVertex, {})));
                    currentVertex = vertices.find(vertexCount)->second;

                    std::cout << "Import entered" << std::endl;
                    for (int i = 0; i < callArgs.size(); i++){

                        switch(importAndPositionToUseDef[name][i + 1]){ // i + 1 because map's indices start from 1
                            case use:
                                callArgs[i]->markAsUse(currentVertex, 0); break;
                            case def:
                                callArgs[i]->markAsDef(currentVertex, 0); break;
                            default:
                                std::cout << "enterVF -- import: found DF with unexpected UseDef!" << std::endl;
                        }
                    }
                    std::cout << "> enterVF finished\n\n";
                    return currentVertex;
                }

                case subVF: {
                    
                    std::cout << "Sub entered" << std::endl;
                    std::vector<Identifier*> declaredNamesVector = {};
                    std::set<std::string> declaredNamesSet = {}; // used to check for duplicate names at declaration

                    // this is a name table used to store declared arguments to use inside enterBlock
                    std::map<std::string, Identifier*> nextDeclaredOutsideIdsMap = {};

                    // in case of a "sub": add args as an inside Ids and map them to call args
                    // IMPORTANT EXCEPTION: no mapping done to args of main(); also these names cannot be initialized and
                    // indexed, so they are pretty special and have a class of their own: MainArgName

                    // find if this sub is even declared
                    auto temp = subNameToArgsVector.find(name);
                    if (temp != subNameToArgsVector.end()){
                        // get declared arguments vector
                        auto declaredArgsVector = temp->second;

                        // iterate through this vector and for every arg create a SubArgName or MainArgName object
                        for (int i = 0; i < (*declaredArgsVector).size(); i++){
                            auto declaredArg = (*declaredArgsVector)[i];

                            // name of a declared argument
                            std::string identifierDeclaredName = *(declaredArg->name_->value_);

                            // check if this name is already declared or not; this is done for every found call of the same sub
                            // this should be done only once TODO
                            if (declaredNamesSet.find(identifierDeclaredName) == declaredNamesSet.end()){

                                declaredNamesSet.insert(identifierDeclaredName);

                                if (name == "main"){
                                    MainArgName* mainArgName = new MainArgName(identifierDeclaredName);
                                    declaredNamesVector.push_back(mainArgName);
                                    nextDeclaredOutsideIdsMap.insert(std::make_pair(identifierDeclaredName, mainArgName));
                                } else {
                                    SubArgName* subArgName = new SubArgName(identifierDeclaredName, callArgs[i], line);
                                    declaredNamesVector.push_back(subArgName);
                                    nextDeclaredOutsideIdsMap.insert(std::make_pair(identifierDeclaredName, subArgName));
                                }
                            } else {
                                declaredNamesVector.push_back(nullptr);//todo beware of this when parsing
                                std::cout << "INTERNAL ERROR: created nullptr sub/main arg name because of name duplication" << std::endl;
                                std::string report = "ERROR: duplicate name of a sub arg at " + name + ": " + identifierDeclaredName + "; line: " + std::to_string(line) + "\n";
                                errorReports.push_back(report);
                            }
                        }

                        vertices.insert(std::make_pair(vertexCount, new CFVertex(currentDepth, vertexCount, line,
                            name, subVF, callerVertex, declaredNamesVector)));
                        currentVertex = vertices.find(vertexCount)->second;
                    } else {
                        //std::cout << "ERROR: could not find sub with a name " << name << std::endl;
                        // this is actually redundant, as check happens below already
                    }

                    std::cout << "> enterVF finished\n\n";
                    return enterBlock(subVF, currentBlock, currentVertex, nextDeclaredOutsideIdsMap,
                        currentDepth, callArgs, declaredNamesVector, name);
                }

                case forVF: {

                    for_statement* innerStatementForVF = dynamic_cast<for_statement*>(innerStatement);

                    // check for duplicate name
                    std::string forIteratorString = innerStatementForVF->name_->to_string();
                    if (declaredOutsideIdsMap.find(forIteratorString) != declaredOutsideIdsMap.end()){
                        errorReports.push_back("ERROR: duplicate name of a \"for\" iterator\n");
                        std::cout << "INTERNAL ERROR: aborted creating for vertex" << std::endl;
                        return nullptr;//todo pretty hefty exception here
                    }

                    ForIteratorName* forIterator = new ForIteratorName(forIteratorString);//todo fails

                    declaredOutsideIdsMap.insert(std::make_pair(forIteratorString, forIterator));

                    // all names inside "for" expressions must be marked as used
                    Expression* leftBorder = new Expression(innerStatementForVF->expr_1_, declaredOutsideIdsMap, &errorReports);
                    leftBorder->markAsUse(currentVertex, 0);
                    Expression* rightBorder = new Expression(innerStatementForVF->expr_2_, declaredOutsideIdsMap, &errorReports);
                    rightBorder->markAsUse(currentVertex, 0);

                    vertices.insert(std::make_pair(vertexCount, new ForVertex(currentDepth, vertexCount, line,
                        forIterator, leftBorder, rightBorder, callerVertex)));
                    currentVertex = vertices.find(vertexCount)->second;
                    forIterator->setVertex(currentVertex);

                    std::cout << "For entered" << std::endl;
                    std::cout << "> enterVF finished\n\n";
                    return enterBlock(forVF, currentBlock, currentVertex, declaredOutsideIdsMap,
                        currentDepth, {}, {}, "for");
                }

                case whileVF: {

                    while_statement* innerStatementWhileVF = dynamic_cast<while_statement*>(innerStatement);

                    // check for duplicate name
                    std::string whileIteratorString = innerStatementWhileVF->left_->to_string();
                    if (declaredOutsideIdsMap.find(whileIteratorString) != declaredOutsideIdsMap.end()){
                        errorReports.push_back("ERROR: duplicate name of a \"while\" iterator\n");
                        std::cout << "INTERNAL ERROR: aborted creating while vertex" << std::endl;
                        return nullptr;//todo pretty hefty exception here
                    }

                    WhileIteratorName* whileIterator = new WhileIteratorName(whileIteratorString);
                    declaredOutsideIdsMap.insert(std::make_pair(whileIteratorString, whileIterator));

                    Expression* whileOutNameExpr = new Expression(innerStatementWhileVF->id_, declaredOutsideIdsMap, &errorReports);
                    Identifier* whileOutName = whileOutNameExpr->getAsIdentifier();
                    if ((whileOutName == nullptr) || 
                        ((whileOutName->getType() != indexedDFNameType) && (whileOutName->getType() != subArgNameType))){//todo is mainArgAllowed?
                        std::cout << "INTERNAL ERROR: unsuitable expression at while out name leads to nullpointer" << std::endl;
                        errorReports.push_back("ERROR: trying to use non-suitable expression as \"while\" out name\n");
                        whileOutName = nullptr;
                    }

                    // "while" out name is an IndexedDF that must be marked as "def" TODO also everything inside indices is "used" as well
                    // everything inside condition and iterator start must be marked as used
                    Expression* conditionExpression = new Expression(innerStatementWhileVF->expr_, declaredOutsideIdsMap, &errorReports);
                    conditionExpression->markAsUse(currentVertex, 0);
                    Expression* startExpression = new Expression(innerStatementWhileVF->right_, declaredOutsideIdsMap, &errorReports);//todo wrong
                    startExpression->markAsUse(currentVertex, 0);

                    vertices.insert(std::make_pair(vertexCount, new WhileVertex(currentDepth, vertexCount, line,
                        whileIterator, whileOutName, conditionExpression, startExpression,
                        callerVertex)));
                    currentVertex = vertices.find(vertexCount)->second;
                    whileIterator->setVertex(currentVertex);

                    std::cout << "While entered" << std::endl;
                    std::cout << "> enterVF finished\n\n";
                    return enterBlock(whileVF, currentBlock, currentVertex, declaredOutsideIdsMap,
                        currentDepth, {}, {}, "for");
                }

                case ifVF: {

                    if_statement* innerStatementIfVF = dynamic_cast<if_statement*>(innerStatement);

                    Expression* conditionExpression = new Expression(innerStatementIfVF->expr_, declaredOutsideIdsMap, &errorReports);

                    vertices.insert(std::make_pair(vertexCount, new IfVertex(currentDepth, vertexCount, line,
                        conditionExpression, callerVertex)));
                    currentVertex = vertices.find(vertexCount)->second;

                    // all identifiers inside "if" expression must be marked as "used"
                    conditionExpression->markAsUse(currentVertex, 0);

                    std::cout << "If entered" << std::endl;
                    std::cout << "> enterVF finished\n\n";
                    return enterBlock(ifVF, currentBlock, currentVertex, declaredOutsideIdsMap,
                        currentDepth, {}, {}, "if");
                }

                case letVF: {

                    let_statement* innerStatementLetVF = dynamic_cast<let_statement*>(innerStatement);

                    // map expressions to new letNames
                    std::vector<assign*>* assignmentsVector = innerStatementLetVF->assign_seq_->assign_seq_;
                    std::vector<LetName*>* letNamesVector = new std::vector<LetName*>();
                    for (auto assignment: *assignmentsVector){

                        // check for duplicate name
                        std::string letString = *(assignment->name_->get_value());
                        if (declaredOutsideIdsMap.find(letString) != declaredOutsideIdsMap.end()){
                            errorReports.push_back("ERROR: duplicate name of a \"let\" identifier\n");
                            std::cout << "INTERNAL ERROR: aborted creating let vertex" << std::endl;
                            return nullptr;//todo pretty hefty exception here
                        }

                        Expression* letExpr = new Expression(assignment->expr_, declaredOutsideIdsMap, &errorReports);
                        LetName* letName = new LetName(*(assignment->name_->get_value()), letExpr);
                        declaredOutsideIdsMap.insert(std::make_pair(letString, letName));
                        letNamesVector->push_back(letName);
                    }

                    vertices.insert(std::make_pair(vertexCount, new LetVertex(currentDepth, vertexCount, line,
                        letNamesVector, callerVertex)));
                    currentVertex = vertices.find(vertexCount)->second;

                    std::cout << "Let entered" << std::endl;
                    std::cout << "> enterVF finished\n\n";
                    return enterBlock(letVF, currentBlock, currentVertex, declaredOutsideIdsMap,
                        currentDepth, {}, {}, "let");
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

        // TODO
        // this function uses breadth search to find cycles in DDG, as this indicates cyclic dependencies
        void checkCyclicDependence(){

        }

        // this function goes through baseNameSet and finds few types of errors:
        // 1. multiple DF initialization
        // 2. using uninitialized DFs
        // 3. unused DF
        void checkBaseNameSet(){

            for (BaseDFName* bn: baseNameSet){
                auto bnMap = bn->getMap();
                for (auto sizeAndUseDefs: bnMap){

                    int size = sizeAndUseDefs.first;
                    std::vector<Vertex*> uses = *(sizeAndUseDefs.second.first);
                    std::vector<Vertex*> defs = *(sizeAndUseDefs.second.second);

                    // multiple initialization of a DF
                    if (size == 0){ // simple DFs
                        if (defs.size() > 1){
                            std::string report = "ERROR: multiple initialization of a DF " + bn->getName() + " in lines:\n";
                            for (auto def: defs){
                                report += (std::to_string(def->getLine()) + " ");
                            }
                            report += "\n";
                            errorReports.push_back(report);
                        }
                    } else { // indexed DFs
                        //TODO add warnings?
                        // proper implementation requires expressions parsing
                    }

                    // unused DF 1
                    if (uses.size() == 0){
                        std::string report = "ERROR: unused DF " + bn->getName() + " with " + std::to_string(size) + " indices\n";
                        errorReports.push_back(report);
                    } else {
                        // using uninitialized DFs
                        if (defs.size() == 0){
                            std::string report = "ERROR: using uninitialized DF " + bn->getName() + " with " + std::to_string(size) +
                            " indices at lines:\n";
                            for (auto use: uses){
                                report += (std::to_string(use->getLine()) + " ");
                            }
                            report += "\n";
                            errorReports.push_back(report);
                        }
                    }

                }
                // unused DF 2
                if (bnMap.size() == 0){
                    std::string report = "ERROR: unused base name DF " + bn->getName() + "\n";
                    errorReports.push_back(report);
                }
            }


        }

        // this function accepts list of errors to find and tries to find them in the created graph and baseNameSet
        void findErrors(){

            checkBaseNameSet();

            //TODO cyclic dependence

            //TODO attempt to initialize unfitting expression

        }

        DDG(ast* astObjectIn){

            auto graphBuildStart = std::chrono::steady_clock::now();
            
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

            std::cout << "\n============ Creating DDG ============" << std::endl;

            this->findSubs(astobj);

            if (!(this->mainExists)){
                std::cout << "ERROR: No main found" << std::endl;
                return;
            }

            std::set<std::string> emptySet = {};

            // 2. create all the vertices

            Vertex* mainVertex = enterVF({}, {}, (this->subNameToBlock)["main"], 1,
                        subVF, "main", mainLine,
                        /* callerVertex */ nullptr, nullptr);

            std::cout << "Created a [MAIN] vertex number " << this->vertexCount
                      << " with a type " << (this->vertices).find(1)->second->getVertexType()
                      << " and an address of " << &((this->vertices).find(1)->second) << std::endl;

            // 3. bindVertices vertices to eachother
            bindVertices(mainVertex);

            auto graphBuildEnd = std::chrono::steady_clock::now();

            auto graphBuildTotal = std::chrono::duration_cast<ns>(graphBuildEnd - graphBuildStart).count();

            // printing out information does not count towards time to use and build graph
            std::cout << "Total vertices: " << vertexCount << std::endl << std::endl; 
            for (int i = 1; i <= vertexCount; i++){

                if (vertices.find(i) == vertices.end()){
                    std::cout << "INTERNAL ERROR: could not find vertex with number ";
                    std::cout << i << std::endl;
                } else {
                    vertices.find(i)->second->printInfo();
                }
                std::cout << std::endl;

            }

            std::cout << "BaseDFNames:" << std::endl;
            for (BaseDFName* bn: baseNameSet){
                std::cout << std::endl;
                std::cout << "Name: " << bn->getName() << std::endl;
                std::cout << "Declared at line: " << bn->getLine() << std::endl;
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

            std::cout << "\n============ Created DDG =============" << std::endl;

            // 4. search for errors

            std::cout << "\n======== Searching for errors ========" << std::endl;

            auto errorsFindStart = std::chrono::steady_clock::now();

            findErrors();

            auto errorsFindEnd = std::chrono::steady_clock::now();
            auto errorsFindTotal = std::chrono::duration_cast<ns>(errorsFindEnd - errorsFindStart).count();

            // printing out information does not count towards time to find errors
            for (auto r: errorReports){
                std::cout << r << std::endl;
            }

            std::cout << "\nTime to find errors: " << (double)errorsFindTotal / 1000000000 << " seconds" << std::endl;

            std::cout << "\nTime to build DDG: " << (double)graphBuildTotal / 1000000000 << " seconds" << std::endl;

        }

};
