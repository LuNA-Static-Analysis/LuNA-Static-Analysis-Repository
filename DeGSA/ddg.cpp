#include "../parser/ast.hpp"
#include <map>
#include <set>

enum VertexType {

    forVF = 1,
    ifVF = 2,
    whileVF = 3,
    letVF = 4,
    importVF = 5,
    subVF = 6

};

enum UseDef {

    use = 1,
    def = 2,
    useAndDef = 3

};

// this class represents a vertex in a DDG
// vertex is a computational fragment (VF), so it could require some (or none) VFs to be ran before, and also allow other VFs to run
// also vertex can contain VFs inside, if its operator allows to have blocks (i.e subprogram, but not an import)
class Vertex {

    // this struct serves as a binding between vertices of the same level
    // it has a pointer to a vertice and a name of a DF used or defined
    struct Binding {

    private:

        std::string name;
        Vertex* pointerTo;

    public:

        Binding(Vertex* pointerTo, std::string name){
            this->name = name;
            this->pointerTo = pointerTo;
        }

        std::string getName(){
            return this->name;
        }

        Vertex* getPointerTo(){
            return this->pointerTo;
        }

        bool operator<(const Binding b) const {//TODO optimize + check (this is utterly retarded)
            if ((long)(this->pointerTo) < (long)b.pointerTo){
                return true;
            } else if (this->name.compare(b.name) > 0){
                return true;
            } else {
                return false;
            }
        }
    };

    private:

        std::set<Binding> in; // vertices that must be ran directly before current
        std::set<Binding> out; // vertices that require directly current vertex to be ran
        std::set<Vertex*> inside; // vertices that are inside the body of a current vertex

        std::vector<std::string> currentNamespaceDFs; // DFs that are visible inside current block (i.e. "for" can see what's outside)
        std::vector<std::string> declaredDFs; // DFs that are declared in current block; they must be included in currentNamespaceDFs
        // also both vectors allow for duplicates, which indicates for errors; checking for errors happens later

        std::set<std::string> use; // list of DFs that are used in this vertex
        std::set<std::string> def; // list of DFs that are defined in this vertex

        VertexType vertexType; // type of a vertex (VF type)
        std::string name; // name of an import/sub
        int depth; // amount of blocks that this vertex is in
        int number; // unique number of a vertice
        int line; // line in code that this operator is in

    public:

        Vertex(VertexType vertexType, std::set<std::string> useDFs, std::set<std::string> defDFs, std::set<Vertex*> inside, std::string name, int depth, int number,
        std::vector<std::string> currentNamespaceDFs, std::vector<std::string> declaredDFs, int line){

            this->in = {};
            this->out = {};
            this->inside = inside;

            this->currentNamespaceDFs = currentNamespaceDFs;
            this->declaredDFs = declaredDFs;

            this->use = useDFs;
            this->def = defDFs;

            this->vertexType = vertexType;
            this->name = name;
            this->depth = depth;
            this->number = number;
            this->line = line;

        }

        // copy constructor //TODO does it work properly?
        Vertex(Vertex* vertex){

            std::cout << "> Copy constructor for Vertex is called" << std::endl;

            this->in = vertex->in; //TODO this does not work perhaps
            this->out = vertex->out; //TODO this also does not work perhaps
            this->inside = vertex->inside;

            //TODO vectors of current and declared

            this->use = vertex->use;
            this->def = vertex->def;

            this->vertexType = vertex->vertexType;

            this->name = vertex->name;

            this->depth = vertex->depth;

            this->number = vertex->number;

        }

        VertexType getVertexType(){
            return vertexType;
        }

        std::set<std::string> getUseSet(){
            return use;
        }

        std::set<std::string> getDefSet(){
            return def;
        }

        std::set<Vertex*> getInsideSet(){
            return inside;
        }

        std::set<Binding> getInSet(){
            return in;
        }

        std::set<Binding> getOutSet(){
            return out;
        }

        std::vector<std::string> getDeclaredDFs(){
            return declaredDFs;
        }

        std::vector<std::string> getCurrentNamespaceDFs(){
            return currentNamespaceDFs;
        }

        std::string getName(){
            return name;
        }

        int getDepth(){
            return depth;
        }

        int getNumber(){
            return number;
        }

        int getLine(){
            return line;
        }

        void addIn(Vertex* vertex, std::string dfName){
            this->in.insert(Binding(vertex, dfName)); //TODO this should not allow duplicates; does it allow it here?
        }

        void addOut(Vertex* vertex, std::string dfName){
            this->out.insert(Binding(vertex, dfName)); //TODO this should not allow duplicates; does it allow it here?
        }

        void addInside(Vertex* vertex){
            auto temp = this->inside.find(vertex);
            if (temp == this->inside.end()){
                this->inside.insert(vertex);
            }
        }

        void addUse(std::string name){
            auto temp = this->use.find(name);
            if (temp == this->use.end()){
                this->use.insert(name);
            }
        }

        void addDef(std::string name){
            auto temp = this->def.find(name);
            if (temp == this->def.end()){
                this->def.insert(name);
            }
        }

        void setDeclaredDFs(std::vector<std::string> declaredDFs){
            this->declaredDFs = declaredDFs;
            for (auto d: declaredDFs){
                this->currentNamespaceDFs.push_back(d);
            }
        }

        void printInfo(){

            std::cout << "Vertex number: " << this->getNumber() << std::endl;
            std::cout << "Vertex address: " << this << std::endl;
            std::cout << "Vertex type: ";
            std::cout << this->getVertexType() << " ";
            switch (this->getVertexType()){
                case forVF:
                    std::cout << "(for)" << std::endl;
                    break;
                case ifVF:
                    std::cout << "(if)" << std::endl;
                    break;
                case whileVF:
                    std::cout << "(while)" << std::endl;
                    break;
                case letVF:
                    std::cout << "(let)" << std::endl;
                    break;
                case importVF:
                    std::cout << "(atomic CF); name: " << this->getName() << std::endl;
                    break;
                case subVF:
                    std::cout << "(structured CF);  name: " << this->getName() << std::endl;
                    break;
                default:
                    std::cout << "(unknown)" << std::endl;
                    break;
            }
            std::cout << "Vertex line: " << this->getLine() << std::endl;
            std::cout << "Vertex depth: " << this->getDepth() << std::endl;

            std::cout << "Current namespace DFs:";
            for (std::string i: this->getCurrentNamespaceDFs()){
                std::cout << " " << i;
            }
            std::cout << std::endl;

            std::cout << "Declared DFs:";
            for (std::string i: this->getDeclaredDFs()){
                std::cout << " " << i;
            }
            std::cout << std::endl;

            std::cout << "Use DFs:";
            for (std::string i: this->getUseSet()){
                std::cout << " " << i;
            }
            std::cout << std::endl;

            std::cout << "Def DFs:";
            for (std::string i: this->getDefSet()){
                std::cout << " " << i;
            }
            std::cout << std::endl;

            std::cout << "Vertices inside:";
            for (auto i: this->getInsideSet()){
                std::cout << " " << i;
            }
            std::cout << std::endl;

            std::cout << "Vertices before (\"in\"):";
            for (auto i: this->getInSet()){
                std::cout << " " << i.getPointerTo() << " [" << i.getPointerTo()->getNumber() << "] (" << i.getName() << ")";
            }
            std::cout << std::endl;

            std::cout << "Vertices after (\"out\"):";
            for (auto i: this->getOutSet()){
                std::cout << " " << i.getPointerTo() << " [" << i.getPointerTo()->getNumber() << "] (" << i.getName() << ")";
            }
            std::cout << std::endl;

        }
        
};

class ErrorCollector {



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

        std::map<int, Vertex> vertices; // list of all vertices; vertex numeration starts from one
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

            if (blockobj->opt_dfdecls_->dfdecls_ != NULL) { // found some DF declarations

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

        // enterVF must be called on a vertex' block (initially main's vertex). It recursively goes through each operator in the block,
        // creating a corresponding vertex for each and keeping track of what DFs are use and defined and where exactly, and storing
        // this information in Use and Def sets of a vertice (this is later used in bindVertices() to fully create a graph)
        //TODO create a function that checks for using undeclared DFs instead of checking here?
        Vertex* enterVF(std::vector<std::string> currentNamespaceDFs, std::map<int, std::set<std::string>> callArgs,
                        block* currentBlock, int currentDepth, VertexType vertexType, std::string name, int line){
            std::cout << "> enterVF called\n\n";
            std::cout << "Entering block " << currentBlock << "; name: " + name << std::endl;

            vertexCount++;
            vertices.insert(std::make_pair(vertexCount, Vertex(vertexType, {}, {}, {}, name, currentDepth, vertexCount, 
            currentNamespaceDFs, {}, line)));
            Vertex* currentVertex = &(vertices.find(vertexCount)->second);

            switch(vertexType){
                case importVF: {
                    std::cout << "Import entered" << std::endl;
                    for (int i = 1; i <= callArgs.size(); i++){
                        switch(importAndPositionToUseDef[name][i]){
                            case use:
                                for (auto df: callArgs[i]){
                                    currentVertex->addUse(df);
                                }
                                break;
                            case def:
                                for (auto df: callArgs[i]){
                                    currentVertex->addDef(df);
                                }
                                break;
                            case useAndDef:
                                std::cout << "enterVF -- import: found useAndDef DF!" << std::endl;
                                break;
                            default:
                                std::cout << "enterVF -- import: found DF with unknown UseDef!" << std::endl;
                        }
                    }
                    std::cout << "> enterVF finished\n\n";
                    return currentVertex;
                }
                case subVF: {
                    std::cout << "Sub entered" << std::endl;
                    //iterate through statements, collect vertices and their use-defs, initialize currentVertex' use-defs and return it
                    int statementNumber = 0;

                    //find DF declarations: use vector and check for duplicates (i.e.: df a, b, b;)
                    auto declaredDFsVector = scanForDFDecls(currentBlock);
                    currentVertex->setDeclaredDFs(declaredDFsVector);
                    std::set<std::string> declaredDFsSet = {};
                    for (auto s: currentNamespaceDFs){ // assuming that currentNamespaceDFs has no duplicates (TODO but what if we do?)
                        if (declaredDFsSet.find(s) != declaredDFsSet.end()) {
                            //std::cout << "ERROR: found DF duplicate!"
                        } else {
                            declaredDFsSet.insert(s);
                        }
                    }
                    // now add new-declared DFs to already existing DFs from current namespace
                    for (auto s: declaredDFsVector){
                        if (declaredDFsSet.find(s) != declaredDFsSet.end()) {
                            std::cout << "ERROR: declared DF duplicate: " + s << std::endl;
                        } else {
                            declaredDFsSet.insert(s);
                        }
                    }
                    
                    for (statement* innerStatement: *(currentBlock->statement_seq_->statements_)){
                        statementNumber++;
                        std::cout << "Statement number " << statementNumber << ": ";
                        cf_statement* innerStatementFunctionVF = dynamic_cast<cf_statement*>(innerStatement);
                        if (innerStatementFunctionVF != NULL){
                            std::string subName = *(innerStatementFunctionVF->code_id_->value_);
                            std::vector<expr*> DFExpressions = *(innerStatementFunctionVF->opt_exprs_->exprs_seq_->expr_); // vector of call args (expressions; need to parse)
                            Vertex* tempVertex;

                            // check if this DFs are declared at all
                            auto parsedArguments = ParsedArguments(DFExpressions).getMap();
                            for (auto i: parsedArguments){
                                for (auto j: i.second){
                                    if (declaredDFsSet.find(j) == declaredDFsSet.end()){
                                        std::cout << "ERROR: found undeclared DF: " + j << std::endl;
                                    }
                                }
                            }

                            if (imports.find(subName) != imports.end()){ // import VF
                                std::cout << "import" << std::endl;
                                tempVertex = enterVF({}, parsedArguments, nullptr, currentDepth + 1, importVF, subName, innerStatement->line_);
                            } else { // subVF
                                std::cout << "sub" << std::endl;
                                // find what DFs to send as "current namespace DFs" to enterVF
                                std::vector<std::string> nextNamespaceDFs = {};
                                auto m = structuredCFArgs.find(subName);
                                if (m != structuredCFArgs.end()){
                                    for (auto s: *(m->second)){
                                        nextNamespaceDFs.push_back(*(s->name_->value_));
                                    }
                                }
                                tempVertex = enterVF(nextNamespaceDFs, parsedArguments, (this->structuredCFBlocks)[subName], currentDepth + 1, subVF, subName, innerStatement->line_);
                            }
                            //for (every df in definedArgs) if (this df is used/defined in tempVertex, then add call arg of current operator that matches this defined arg)
                            if (structuredCFArgs[name] != nullptr){
                                std::cout << "initializing Vertex for sub: " + name << std::endl;
                                auto definedArgs = *structuredCFArgs[name];
                                for (int i = 0; i < definedArgs.size(); i++){ // defined args
                                    std::string definedArg = *(definedArgs[i]->name_->get_value());
                                    auto tempUseSet = tempVertex->getUseSet();
                                    if (tempUseSet.find(definedArg) != tempUseSet.end()){
                                        for (auto j: callArgs[i + 1]) currentVertex->addUse(j);
                                    }
                                    auto tempDefSet = tempVertex->getDefSet();
                                    if (tempDefSet.find(definedArg) != tempDefSet.end()){
                                        for (auto j: callArgs[i + 1]) currentVertex->addDef(j);
                                    }
                                }
                            }

                            currentVertex->addInside(tempVertex);
                        }
                        //TODO other VFs
                    }
                    std::cout << "> enterVF finished\n\n";
                    return currentVertex;

                }
                default: {
                    std::cout << "enterVF: unsupported operator found" << std::endl;
                    std::cout << "> enterVF finished\n\n";
                    return nullptr;
                }

            }

        }

        // this function binds vertices to each other; it is initially called for "main" vertex and uses its "inside" field to call itself recursively on new vertices
        // function initializes "in" and "out" of every vertex
        //TODO modify Vertex so that the link will contain what DF is used/initialized; this is needed for analysis
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

        void walkThrough(){ //TODO

        }

    public:

        // this function recursively goes through DDG and on every namespace checks if any of the DFs
        // are initialized more than once
        // map: name of a DF -> line in code
        //TODO perhaps create a "walkthrough" function to recursively go through a graph and return required information
        //TODO function must return a list of errors in a JSON or whatever
        void checkMultipleDFInitialization(Vertex* vertex, std::map<std::string, int> initializedDFs){

            std::vector<Vertex*> stack = {};

            for (auto v: vertex->getInsideSet()){

                if (v->getInsideSet().size() > 0){ 
                    // add to stack to analyze them recursively later
                    stack.push_back(v);
                }

                for (auto d: v->getDefSet()){
                    if (initializedDFs.find(d) != initializedDFs.end()){
                        //TODO error found, throw exception
                        //ErrorCollector.collect(vertex, v, );
                        std::cout << "Multiple DF initialization: " << d << std::endl;
                    } else { // update a map of initialized DFs and their positions
                        initializedDFs.insert(std::make_pair(d, -1));
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

        }

        // this function accepts list of errors to find and tries to find them in the created graph
        // list consists of Error enums
        void findErrors(Vertex* mainVertex){
            
            checkMultipleDFInitialization(mainVertex, {});

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

            Vertex* mainVertex = enterVF({}, {}, (this->structuredCFBlocks)["main"], 1, subVF, "main", mainLine);

            std::cout << "Created a [MAIN] vertex number " << this->vertexCount
                      << " with a type " << (this->vertices).find(1)->second.getVertexType()
                      << " and an address of " << &((this->vertices).find(1)->second) << std::endl;

            // 3. bindVertices vertices to eachother
            bindVertices(mainVertex);

            std::cout << "Total vertices: " << vertexCount << std::endl << std::endl; 
            for (int i = 1; i <= vertexCount; i++){

                vertices.find(i)->second.printInfo();
                std::cout << std::endl;

            }

            std::cout << "============ Created DDG =============" << std::endl;

            // 4. search for errors

            findErrors(mainVertex);

        }

};