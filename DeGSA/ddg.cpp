#include "../parser/ast.hpp"
//#include <vector> // already included in ast.hpp
#include <map>
#include <set>
#include <regex>

//TODO use "parser" folder!

//std::cout << blockobj << std::endl;
//std::cout << blockobj->opt_dfdecls_ << std::endl;
//std::cout << blockobj->opt_dfdecls_->dfdecls_ << std::endl;
//std::cout << blockobj->opt_dfdecls_->dfdecls_->name_seq_ << std::endl;
//std::cout << blockobj->opt_dfdecls_->dfdecls_->name_seq_->names_ << std::endl; // all of this just to fix null pointer error

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

enum Error {

    multipleDFInitialization = 1

};

// this class represents a vertex in a DDG
// vertex is a computational fragment (VF), so it could require some (or none) VFs to be ran before, and also allow other VFs to run
// also vertex can contain VFs inside; that is, vertex is basically a set of it's child vertices and nothing
class Vertex {

    private:

        std::set<Vertex*> in; // vertices that must be ran directly before current
        std::set<Vertex*> out; // vertices that require directly current vertex to be ran
        std::set<Vertex*> inside; // vertices that are inside the body of a current vertex

        std::set<std::string> use; // list of DFs that are used in this vertex
        std::set<std::string> def; // list of DFs that are defined in this vertex

        VertexType vertexType; // type of a vertex (VF type)

        std::string name; // name of an import/sub

        int depth; // amount of blocks that this vertex is in

        int line; //TODO line in code that this operator is in

    public:

        Vertex(VertexType vertexType, std::set<std::string> useDFs, std::set<std::string> defDFs, std::set<Vertex*> inside, std::string name, int depth){

            this->in = {};
            this->out = {};
            this->inside = inside;

            this->use = useDFs;
            this->def = defDFs;

            this->vertexType = vertexType;

            this->name = name;

            this->depth = depth;

            //this->line = line; // TODO

        }

        // copy constructor //TODO does it work properly?
        Vertex(Vertex* vertex){

            std::cout << "Copy constructor for Vertex is called" << std::endl;

            this->in = vertex->in;
            this->out = vertex->out;
            this->inside = vertex->inside;

            this->use = vertex->use;
            this->def = vertex->def;

            this->vertexType = vertex->vertexType;

            this->name = vertex->name;

            this->depth = vertex->depth;

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

        std::set<Vertex*> getInSet(){
            return in;
        }

        std::set<Vertex*> getOutSet(){
            return out;
        }

        std::string getName(){
            return name;
        }

        int getDepth(){
            return depth;
        }

        void addIn(Vertex* vertex){
            auto temp = this->in.find(vertex);
            if (temp == this->in.end()){
                this->in.insert(vertex);
            }
        }

        void addOut(Vertex* vertex){
            auto temp = this->out.find(vertex);
            if (temp == this->out.end()){
                this->out.insert(vertex);
            }
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

        void printInfo(){

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

            std::cout << "Vertex depth: " << this->getDepth() << std::endl;

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
                std::cout << " " << i;
            }
            std::cout << std::endl;

            std::cout << "Vertices after (\"out\"):";
            for (auto i: this->getOutSet()){
                std::cout << " " << i;
            }
            std::cout << std::endl;

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
        block* mainBlock; // needed to start analysis from main

        std::map<std::string, block*> structuredCFBlocks; // list of structured CFs
        std::map<std::string, std::vector<param*>*> structuredCFArgs; // list of args of subprograms; vector starts from 0

        std::map<int, Vertex> layerOne; // list of vertices that require no DFs to be started -- bindVertices
        std::map<int, Vertex> vertices; // list of all vertices; vertex numeration starts from one
        int vertexCount; // count of vertices

        std::set<std::string> imports; // names of imported functions from C++ (ucodes.cpp)

    //public:

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

        // TODO: uncertain: use already existing block->opt_dfdecls_->dfdecls_->name_seq_, or scan manually?
        // scanForDFDecls is a function, that for each CF scans for DF declaration (if CF's nature allows it)
        // DFs, according to LuNA rules, must be declared on the very first line, and no other declarations shall follow
        static std::vector<std::string> scanForDFDecls(block* blockobj){
            
            std::cout << "> scanForDFDecls called\n\n";
            std::vector<std::string> DFDecls = {};

            if (blockobj->opt_dfdecls_->dfdecls_ != NULL) { // found some DF declarations

                std::vector<luna_string*> DFNames = *(blockobj->opt_dfdecls_->dfdecls_->name_seq_->names_);
                for (luna_string* currentDFName: DFNames){
                    DFDecls.push_back(*(currentDFName->value_));
                }

                if (DFDecls.size() != DFNames.size()){//TODO
                    std::cout << "ERROR: Duplicates in DF declaration" << std::endl;
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
        static std::set<std::string> getDFsFromExpression(expr* expression){ //TODO fails? see id, complex_id, simple_id; regex does not work anymore perhaps

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
                        block* currentBlock, int currentDepth, VertexType vertexType, std::string name){
            std::cout << "> enterVF called\n\n";
            std::cout << "Entering block " << currentBlock << "; name: " + name << std::endl;

            vertexCount++;
            vertices.insert(std::make_pair(vertexCount, Vertex(vertexType, {}, {}, {}, name, currentDepth)));
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
                    std::vector<std::string> declaredDFsVector = scanForDFDecls(currentBlock);
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
                                tempVertex = enterVF({}, parsedArguments, nullptr, currentDepth + 1, importVF, subName);
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
                                tempVertex = enterVF(nextNamespaceDFs, parsedArguments, (this->structuredCFBlocks)[subName], currentDepth + 1, subVF, subName);
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
                            internalVertex->addOut(it);
                            it->addIn(internalVertex);
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

        // this function accepts list of errors to find and tries to find them in the created graph
        // list consists of Error enums
        void findErrors(){
            


        }

        // this function recursively goes through DDG and on every namespace checks if any of the DFs
        // are initialized more than once
        //TODO perhaps create a "walkthrough" function to recursively go through a graph and return required information
        //TODO function must return a list of errors in a JSON or whatever
        //TODO modify bindVertices so it also saves what DF exactly is required/created for a vertex
        void checkMultipleDFInitialization(){

            

        }

        DDG(ast* astObjectIn){
            
            this->vertexCount = 0;
            //imports = new std::set<std::string>; //TODO
            this->imports = {};
            this->vertices = {};

            this->astobj = astObjectIn;

            this->mainExists = false;

            this->layerOne = {};

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

            Vertex* mainVertex = enterVF({}, {}, (this->structuredCFBlocks)["main"], 1, subVF, "main");

            std::cout << "Created a [MAIN] vertex number " << this->vertexCount
                      << " with a type " << (this->vertices).find(1)->second.getVertexType()
                      << " and an address of " << &((this->vertices).find(1)->second) << std::endl;

            // 3. bindVertices vertices to eachother
            bindVertices(mainVertex);

            std::cout << "Total vertices: " << vertexCount << std::endl << std::endl; 
            for (int i = 1; i <= vertexCount; i++){

                //Vertex currentVertex = Vertex(vertices.find(i)->second); //TODO fails; copy constructor is not finished
                std::cout << "Vertex number: " << i << std::endl;
                vertices.find(i)->second.printInfo();
                std::cout << std::endl;

            }

            std::cout << "============ Created DDG =============" << std::endl;

            // 4. search for errors
            //TODO search for errors

            //checkMultipleDFInitialization();

        }

};