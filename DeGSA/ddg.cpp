#include "vertices.cpp"

// this class collects information from checkers and creates JSONs
// TODO
class ErrorCollector {

    void collect(){

    }

};

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

        std::map<std::string, block*> structuredCFBlocks; // list of structured CFs
        std::map<std::string, std::vector<param*>*> structuredCFArgs; // list of args of subprograms; vector starts from 0

        std::map<int, Vertex*> vertices; // list of all vertices; vertex numeration starts from one
        int vertexCount; // count of vertices

        std::set<std::string> imports; // names of imported functions from C++ (ucodes.cpp)

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

                    (this->structuredCFBlocks)[subName] = subDecl->block_; // save block of this CF to a list in a DDG for latery analysis

                    if (subDecl->params_->param_seq_ == nullptr){
                        (this->structuredCFArgs)[subName] = nullptr;
                    } else {
                        (this->structuredCFArgs)[subName] = subDecl->params_->param_seq_->params_; // save args of this CF to use it in enterVF
                    }

                    continue;
                }

                if (importDecl != NULL) { // found an import

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

                    continue;

                }

                std::cout << "ERROR: Unknown CF found" << std::endl;
                    
            }

            std::cout << "> findSubs finished\n\n";
        }

        // scanForDFDecls is a function, that for each CF scans for DF declaration (if CF's nature allows it)
        // DFs, according to LuNA rules, must be declared on the very first line, and no other declarations shall follow
        static std::vector<std::string> scanForDFDecls(block* blockobj){
            
            std::cout << "> scanForDFDecls called\n";
            std::vector<std::string> DFDecls = {};
            std::cout << blockobj << std::endl;
            std::cout << blockobj->opt_dfdecls_ << std::endl;
            std::cout << blockobj->opt_dfdecls_->dfdecls_ << std::endl;

            if ((blockobj->opt_dfdecls_ != NULL) && (blockobj->opt_dfdecls_->dfdecls_ != NULL)) { // found some DF declarations

                std::vector<luna_string*> DFNames = *(blockobj->opt_dfdecls_->dfdecls_->name_seq_->names_); // get names of declared DFs
                for (luna_string* currentDFName: DFNames){
                    DFDecls.push_back(*(currentDFName->value_));
                }

                std::cout << "DFs in block " << blockobj << ":";
                for (std::string currentDFName: DFDecls){
                    std::cout << " " << currentDFName;
                }
                std::cout << std::endl;

            } else {
                std::cout << "No DF declarations found" << std::endl;
            }

            std::cout << "> scanForDFDecls finished\n\n";
            return DFDecls;

        }

        // this function gets an expression, recursively goes through it and returns a set of DFs that are used in this expression
        static std::set<std::string> getDFsFromExpression(expr* expression){

            std::cout << "> getDFsFromExpression called\n\n";

            std::set<std::string> result = {};

            luna_string* lunaString = dynamic_cast<luna_string*>(expression);
            if (lunaString != NULL){
                std::cout << "> getDFsFromExpression finished (non-DF string found)\n\n";
                return result;
            }

            // ignore all this as we are looking only for DFs
            integer* lunaInteger = dynamic_cast<integer*>(expression);
            if (lunaInteger != NULL) {
                std::cout << "> getDFsFromExpression finished (integer ignored)\n\n";
                return result;
            }
            real* lunaReal = dynamic_cast<real*>(expression);
            if (lunaReal != NULL) {
                std::cout << "> getDFsFromExpression finished (real ignored)\n\n";
                return result;
            }

            luna_cast* lunaCast = dynamic_cast<luna_cast*>(expression);
            if (lunaCast != NULL){ // DFs could be inside the cast
                expr* nextExpr = lunaCast->expr_;
                std::cout << "> getDFsFromExpression calling recursively for a luna_cast\n\n";
                result = getDFsFromExpression(nextExpr);
                std::cout << "> getDFsFromExpression finished after luna_cast recursion\n\n";
                return result;
            }

            bin_op* lunaBinOp = dynamic_cast<bin_op*>(expression);
            if (lunaBinOp != NULL){ // DFs could be inside one of the operands
                std::cout << "> getDFsFromExpression calling recursively for a bin_op (left)\n\n";
                std::set<std::string> leftResult = getDFsFromExpression(lunaBinOp->left_);
                for (auto j : leftResult) result.insert(j);
                std::cout << "> getDFsFromExpression calling recursively for a bin_op (right)\n\n";
                std::set<std::string> rightResult = getDFsFromExpression(lunaBinOp->right_);
                for (auto j : rightResult) result.insert(j);
                std::cout << "> getDFsFromExpression finished after bin_op recursion\n\n";
                return result;
            }

            id* df = dynamic_cast<id*>(expression);
            if (df != NULL){

                simple_id* simpleDF = dynamic_cast<simple_id*>(expression);
                if (simpleDF != NULL){
                    result.insert(*(simpleDF->value_->value_));
                    return result;
                }

                complex_id* complexDF = dynamic_cast<complex_id*>(expression);
                if (complexDF != NULL){
                    //TODO exception
                    std::cout << "complex DF found; these are not supported yet" << std::endl;
                    return result;
                }

                std::cout << "error in dynamic_cast to id" << std::endl;
                //TODO exception

            }

            std::cout << "> getDFsFromExpression finished (default)\n\n";
            return {}; //TODO add exception

        }

        class ParsedArguments {
            private:
                // maps position to set of DFs being on this position
                // set is required as expressions, being a single argument, may contain multiple DFs
                std::map<int, std::set<std::string>> map; // starts from 1 //TODO redo to 0
            
            public:
                ParsedArguments(std::vector<expr*> rawCallArgs){
                    map = {};
                    int current = 1;
                    for (auto expression: rawCallArgs){
                        map.insert(std::make_pair(current, getDFsFromExpression(expression)));
                        current++;
                    }
                }

                std::map<int, std::set<std::string>> getMap(){
                    return map;
                }

        };

        // enterBlock must be called inside enterVF whenever enterVF encounters a block
        // this function exists to avoid code duplication while parsing for, while, sub and other operators with blocks
        // however, one-for-all function is still not great (for example, some arguments are used exclusively with "for" block)
        //TODO what to do with this?
        Vertex* enterBlock(VertexType VertexType, block* currentBlock, Vertex* currentVertex, std::vector<std::string> declaredOutsideDFsVector,
            int currentDepth, std::map<int, std::set<std::string>> callArgs, std::string name,
            /*for ForVertex*/ std::string iteratorName, std::string leftBorder, std::string rightBorder){

            /* iterate through statements, collect vertices and their use-defs by calling enterVF on each,
            initialize currentVertex' use-defs and return it */
            int statementNumber = 0; // for logging

            /* find DF declarations in current block; we can declare DF once in every block!
            use vector and check for duplicates (i.e.: df a, b, b;) */
            auto declaredInsideDFsVector = scanForDFDecls(currentBlock);

            std::vector<std::string> declaredBothDFsVector = {};

            std::set<std::string> declaredBothDFsSet = {};

            /* we have 4 DF containers:
            1. declaredOutsideDFsVector -- 
                has duplicates; example: (aa, bb, bb);
                needed for creation of 3., comes as an argument of an enterBlock(), is empty for imports and subs
            2. declaredInsideDFsVector -- 
                has duplicates; example: df a, b, c, c; -> (a, b, c, c);
                needed for creation of 3., comes from checking "df" statement in current block
            3. declaredBothDFsVector -- 
                has duplicates; example: (aa, bb, bb, a, b, c, c);
                needed for error checking later; made as concatenation of 1. and 2.
            4. declaredBothDFsSet --
                set of 3.: example: (aa, bb, a, b, c);
                needed for creating bindings properly*/
            //TODO what to do with iterator? what DF it is?
            //TODO maybe use term "name" instead of a DF?

            // firstly go through outside DFs
            for (auto d: declaredOutsideDFsVector){
                declaredBothDFsVector.push_back(d);
                if (declaredBothDFsSet.find(d) != declaredBothDFsSet.end())
                    std::cout << "ERROR: found DF duplicate in an outside vector -- " + d << std::endl;
                else
                    declaredBothDFsSet.insert(d);
            }

            // secondly go through inside DFs
            for (auto d: declaredInsideDFsVector){
                declaredBothDFsVector.push_back(d);
                if (declaredBothDFsSet.find(d) != declaredBothDFsSet.end())
                    std::cout << "ERROR: found DF duplicate in an inside vector -- " + d << std::endl;
                else
                    declaredBothDFsSet.insert(d);
            }

            // initialize 4 containers:
            currentVertex->setDeclaredInsideDFsVector(declaredInsideDFsVector);
            currentVertex->setDeclaredOutsideDFsVector(declaredOutsideDFsVector);
            currentVertex->setDeclaredBothDFsVector(declaredBothDFsVector);
            currentVertex->setDeclaredBothDFsSet(declaredBothDFsSet);

            Vertex* tempVertex; // this will be used to store Vertex to return from enterVF()

            // now iterate through current block's statements
            for (statement* innerStatement: *(currentBlock->statement_seq_->statements_)){
                statementNumber++;
                std::cout << "Statement number " << statementNumber << ": " << std::endl;

                // ---- handling cf
                cf_statement* innerStatementFunctionVF = dynamic_cast<cf_statement*>(innerStatement);
                if (innerStatementFunctionVF != NULL){
                    std::string subName = *(innerStatementFunctionVF->code_id_->value_);

                    // vector of call args (expressions; need to parse)
                    std::vector<expr*> DFExpressions = *(innerStatementFunctionVF->opt_exprs_->exprs_seq_->expr_);
                    auto parsedArguments = ParsedArguments(DFExpressions).getMap();

                    // check if this DFs are declared at all
                    for (auto i: parsedArguments)
                        for (auto j: i.second)
                            if (declaredBothDFsSet.find(j) == declaredBothDFsSet.end())
                                std::cout << "ERROR: found undeclared DF: " + j << std::endl;

                    if (imports.find(subName) != imports.end()){ // import VF
                        std::cout << "import" << std::endl;

                        tempVertex = enterVF({}, parsedArguments, nullptr, currentDepth + 1,
                            importVF, subName, innerStatement->line_,
                            "", "", "");
                        
                    } else { // subVF TODO what if no such function exists?
                        std::cout << "sub" << std::endl;

                        /* find what DFs to send as "current namespace DFs" to enterVF
                        these DFs are just arguments visible from inside this new sub*/
                        std::vector<std::string> nextNamespaceDFs = {};
                        auto m = structuredCFArgs.find(subName);
                        if (m != structuredCFArgs.end()){
                            for (auto s: *(m->second)){
                                nextNamespaceDFs.push_back(*(s->name_->value_));
                            }
                        }

                        for (auto t: nextNamespaceDFs){
                            std::cout << "AHAHAHAHA " + t << std::endl;
                        }
                        tempVertex = enterVF(nextNamespaceDFs, parsedArguments, (this->structuredCFBlocks)[subName], currentDepth + 1,
                            subVF, subName, innerStatement->line_,
                            "", "", "");
                    }

                    //================== USE DEF ====================== (for sub, not for import; import is initizliaed in enterVF)
                    
                    /* for (every df in definedArgs) 
                    if (this df is used/defined in tempVertex, 
                    then add call arg of current operator that matches this defined arg)
                    i.e. initialize current Vertex' use and def sets by looking what call
                    DFs were actually used in that vertex that we are currently handling*/
                    if (structuredCFArgs[name] != nullptr){
                        std::cout << "initializing Vertex for sub: " + name << std::endl;
                        auto definedArgs = *structuredCFArgs[name];
                        for (int i = 0; i < definedArgs.size(); i++){ // defined args
                            std::string definedArg = *(definedArgs[i]->name_->get_value());

                            auto tempUseSet = tempVertex->getUseSet();
                            if (tempUseSet.find(definedArg) != tempUseSet.end())
                                for (auto j: callArgs[i + 1]) currentVertex->addUse(j);

                            auto tempDefSet = tempVertex->getDefSet();
                            if (tempDefSet.find(definedArg) != tempDefSet.end())
                                for (auto j: callArgs[i + 1]) currentVertex->addDef(j);
                        }
                    }

                    currentVertex->addInside(tempVertex);
                    continue;
                }

                // ---- handling for
                for_statement* innerStatementForVF = dynamic_cast<for_statement*>(innerStatement);
                if (innerStatementForVF != NULL){

                    std::cout << "KEKW: ";
                    std::cout << declaredOutsideDFsVector.size() << std::endl;
                    
                    tempVertex = enterVF(declaredBothDFsVector, {}, innerStatementForVF->block_, currentDepth + 1, 
                        forVF, "for", innerStatement->line_, innerStatementForVF->name_->to_string(),
                        innerStatementForVF->expr_1_->to_string(), innerStatementForVF->expr_2_->to_string());

                    //================== USE DEF ======================

                    std::cout << "initializing Vertex for \"for\": " + name << std::endl;
                    
                    auto tempUseSet = tempVertex->getUseSet();
                    for (auto d: tempUseSet){
                        if (declaredBothDFsSet.find(d) != declaredBothDFsSet.end())
                            currentVertex->addUse(d);
                    }
                    auto tempDefSet = tempVertex->getDefSet();
                    for (auto d: tempDefSet){
                        if (declaredBothDFsSet.find(d) != declaredBothDFsSet.end())
                            currentVertex->addDef(d);
                    }
                    // other stuff from parsing sub is not needed as we don't have to go though arguments of a call
                    // same thing with while, if and else

                    currentVertex->addInside(tempVertex);
                    continue;
                }

                //TODO --- handling other VFs
            }
            std::cout << "> enterVF finished\n\n";
            return currentVertex;

        }

        // enterVF must be called on a vertex' block (initially main's vertex). It recursively goes through each operator in the block,
        // creating a corresponding vertex for each and keeping track of what DFs are use and defined and where exactly, and storing
        // this information in Use and Def sets of a vertice (this is later used in bindVertices() to fully create a graph)
        Vertex* enterVF(std::vector<std::string> declaredOutsideDFsVector, std::map<int, std::set<std::string>> callArgs,
                        block* currentBlock, int currentDepth, VertexType vertexType, std::string name, int line,
                        /*for ForVertice:*/ std::string iteratorName, std::string leftBorder, std::string rightBorder){

            std::cout << "> enterVF called\n\n";
            std::cout << "Entering block " << currentBlock << "; name: " + name << std::endl;

            Vertex* currentVertex;
            vertexCount++; // we will create a Vertex itself later in a switch

            switch(vertexType){

                case importVF: {

                    vertices.insert(std::make_pair(vertexCount, new CFVertex(currentDepth, vertexCount, line,
                        name, importVF)));
                    currentVertex = vertices.find(vertexCount)->second;

                    std::cout << "Import entered" << std::endl;
                    for (int i = 1; i <= callArgs.size(); i++){
                        switch(importAndPositionToUseDef[name][i]){
                            case use:
                                for (auto df: callArgs[i]) currentVertex->addUse(df); break;
                            case def:
                                for (auto df: callArgs[i]) currentVertex->addDef(df); break;
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
                    return enterBlock(subVF, currentBlock, currentVertex, declaredOutsideDFsVector,
                        currentDepth, callArgs, name,
                        "", "", "");
                }

                case forVF: {

                    vertices.insert(std::make_pair(vertexCount, new ForVertex(currentDepth, vertexCount, line,
                        iteratorName, leftBorder, rightBorder)));
                    currentVertex = vertices.find(vertexCount)->second;

                    std::cout << "For entered" << std::endl;
                    return enterBlock(forVF, currentBlock, currentVertex, declaredOutsideDFsVector,
                        currentDepth, {}, "for",
                        iteratorName, leftBorder, rightBorder);
                }

                //TODO other VFs

                default: {
                    std::cout << "enterVF: unsupported operator found" << std::endl;
                    std::cout << "> enterVF finished\n\n";
                    return nullptr;
                }

            }

        }

        // this function binds vertices to each other; it is initially called for "main" vertex and uses its "inside" field to call itself recursively on new vertices
        // function initializes "in" and "out" of every vertex
        //TODO add depth check to avoid ignoring namespace
        void bindVertices(Vertex* currentVertex){

            class DFCoordinates { //TODO convert to singleton

                private:
                std::map<std::string, std::vector<Vertex*>> map;
                
                public:
                DFCoordinates(){
                    map = {};
                }

                void addDFUse(std::string DFName, Vertex* currentVertex){
                    auto finding = map.find(DFName);
                    if (finding != map.end()){ // found this DF being used already, append to existing vector
                        finding->second.push_back(currentVertex);
                    } else { // did not found this DF, create new vector
                        std::vector<Vertex*> temp = {currentVertex};
                        map.insert(std::make_pair(DFName, temp));
                    }
                }

                std::vector<Vertex*>* getDFUses(std::string DFName){
                    auto finding = map.find(DFName);
                    if (finding != map.end()) return &(finding->second);
                    else return NULL;
                }

            };

            std::cout << "> bindVertices called\n" << std::endl;

            DFCoordinates coordinates = DFCoordinates();
            for (Vertex* internalVertex: currentVertex->getInsideSet()){

                for (std::string DFName: internalVertex->getUseSet()){ // adding every use of every DF
                    coordinates.addDFUse(DFName, internalVertex);
                    std::cout << "bindVertices: added " + DFName + " to use-coordinates" << std::endl;
                }

                if (internalVertex->getVertexType() != importVF) { // vertex has a block, use recursion
                    bindVertices(internalVertex);
                }

            }

            for (Vertex* internalVertex: currentVertex->getInsideSet()){

                for (std::string DFName: internalVertex->getDefSet()){ // now find what defined DFs are used, and where exactly
                    std::vector<Vertex*>* maybeUses = coordinates.getDFUses(DFName);
                    if (maybeUses != NULL){
                        std::cout << "bindVertices: found used DF: " + DFName << std::endl;
                        for (Vertex* it: *maybeUses){ // bindVertices current Vertex to all that uses its result
                            internalVertex->addOut(it, DFName);
                            it->addIn(internalVertex, DFName);
                        }
                    } else {
                        //TODO throw exception
                        std::cout << "bindVertices: found unused DF: " + DFName << std::endl;
                    }
                }

            }

        }

    public:

        // this function recursively goes through DDG and on every namespace checks if any of the DFs
        // are initialized more than once
        // map: name of a DF -> line in code
        //TODO function must return a list of errors in a JSON or whatever

        //todo redo this (using 4 new containers) maybe
        void checkMultipleDFInitialization(Vertex* vertex, std::map<std::string, std::vector<int>> initializedDFs){

            std::vector<Vertex*> stack = {};

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
            }

        }

        void checkNonDefinedDFUsage(){//todo redo this (using 4 new containers)
        //problem: launch and see -- foo1 is considered undeclared
        //multiple init works however lol

            for (auto v: vertices) { // check every vertice
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

            }

        }

        void checkUnusedDF(Vertex* currentVertex, std::set<std::string>* currentNamespaceUnusedDFs){

            std::vector<Vertex*> stack = {};

            // refresh current unused DFs
            for (auto d: currentVertex->getDeclaredInsideDFsVector()){
                currentNamespaceUnusedDFs->insert(d);
            }

            for (auto v: currentVertex->getInsideSet()){
                if (v->getInsideSet().size() > 0){ // add vertices inside to stack to analyze them recursively later
                    stack.push_back(v);
                }

                // now check what DFs are actually used; the 
                auto DFset = v->getUseSet();
                for (auto d: DFset){
                    if (currentNamespaceUnusedDFs->find(d) != currentNamespaceUnusedDFs->end()){
                        currentNamespaceUnusedDFs->erase(d);
                    }
                }

            }

            for (auto v: stack){
                if ((v->getVertexType() != subVF) && (v->getVertexType() != importVF)) { // has block
                    checkUnusedDF(v, currentNamespaceUnusedDFs);
                } else { // has no block
                    std::set<std::string> temp = {};
                    checkUnusedDF(v, &temp);
                }
            }

            // if after all the checks some DFs are unused -- report it
            if (currentNamespaceUnusedDFs->size() != 0){
                for (auto d: *currentNamespaceUnusedDFs){
                    std::cout << "Found unused DF " << d << " in vertex number " << currentVertex->getNumber() << " in line " <<
                    currentVertex->getLine() << std::endl;
                }
            }

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

            this->structuredCFBlocks = {};
            this->structuredCFArgs = {};

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

            Vertex* mainVertex = enterVF({}, {}, (this->structuredCFBlocks)["main"], 1, subVF, "main", mainLine, "", "", "");

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