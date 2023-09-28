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

        // copy constructor //TODO do I need it?
        /*Vertex(Vertex* vertex){

            std::cout << "Copy constructor for Vertex is called" << std::endl;

            this->vertexType = vertex->vertexType;

            this->use = vertex->use;
            this->def = vertex->def;
            this->inside = vertex->inside;

            this->in = vertex->in;
            this->out = vertex->out;

            this->name = vertex->name;

            this->depth = vertex->depth;

        }*/

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

// this class is being used as output for enterVF function and helps to initialize Vertex
// std::set<Vertex*> inside;
// std::map<int, UseDef> DFPositionToUseDef;
// TODO refactor this
struct VertexTransferObject {

    std::set<Vertex*> inside;

    //std::set<int> usePosition;
    //std::set<int> defPosition;

    std::map<int, UseDef> DFPositionToUseDef;

};

struct ParsedArguments {
    std::map<std::string, UseDef> DFNameToUseDef;
    std::map<int, UseDef> DFPositionToUseDef;
};

class DDG {

    private:

        // IMPORTANT NOTE: std::map required empty constructor; to ignore this requirement you must use insert/find instead of a [] operator

        ast* astobj; // input AST
        
        // list of imports and their positions corresponding to use or def;
        // this is used to determine if argument in a call will be initialized or used;
        // this is initialized in findSubs();
        // positions start with 1;
        std::map<std::string, std::map<int, UseDef>> importAndPositionToUseDef;


        bool mainExists; // for checking if program is correct
        block* mainBlock; // needed to start analysis from main

        std::map<std::string, block*> structuredCFBlocks; // list of structured CFs

        std::map<int, Vertex> layerOne; // list of vertices that require no DFs to be started -- bind
        std::map<int, Vertex> vertices; // list of all vertices; vertex numeration starts from one
        int vertexCount; // count of vertices

        std::set<std::string> imports; // names of imported functions from C++ (ucodes.cpp)

    public:

        // test function, can be deleted
        static void printTokens(ast* astobj){
            for (auto i: *(astobj->tokens_)){
                std::cout << *i << std::endl;
            }
        }

        void findSubs(ast* astobj){

            std::cout << "Searching for subs\n\n";

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

            std::cout << "Finished searching for subs!" << std::endl;
        }

        // TODO: uncertain: use already existing block->opt_dfdecls_->dfdecls_->name_seq_, or scan manually?
        // scanForDFDecls is a function, that for each CF scans for DF declaration (if CF's nature allows it)
        // DFs, according to LuNA rules, must be declared on the very first line, and no other declarations shall follow
        static std::vector<std::string> scanForDFDecls(block* blockobj){
            
            std::cout << "Scanning block for DF declarations" << std::endl;
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

            return DFDecls;

        }

        // this is for proper arg parsing; "regex" header required
        static bool isInt(std::string s) {
            return std::regex_match(s, std::regex("[0-9]+"));
        }

        static bool isReal(std::string s) {
            return std::regex_match(s, std::regex("[0-9]+.[0-9]+"));
        }

        static bool isString(std::string s) { //TODO rename to isDF
            //return std::regex_match(s, std::regex("\"[^\"]*\""));
            return std::regex_match(s, std::regex("[a-zA-Z][a-zA-Z0-9]*"));
        }

        // this function gets an expression, recursively goes through it and returns a set of DFs that are used in this expression
        static std::set<std::string> getDFsFromExpression(expr* expression){ //TODO fails

            std::set<std::string> result = {};

            luna_string* lunaString = dynamic_cast<luna_string*>(expression);
            if (lunaString != NULL){

                //std::cout << "found luna string: " << *(lunaString->value_) << std::endl;

                if (isString(*(lunaString->value_))){ // DF is a luna_string, just as integer or real (for some reason); 
                    // this regex demands luna_string to be a DF name
                    result.insert(*(lunaString->value_));
                    //std::cout << "and it is a DF" << std::endl;
                } else {
                    //std::cout << "but it is not a DF" << std::endl;
                }
                return result;

            }

            integer* lunaInteger = dynamic_cast<integer*>(expression);
            if (lunaInteger != NULL){ // ignore this as we are looking only for DFs

                return result;
                    
            }

            real* lunaReal = dynamic_cast<real*>(expression);
            if (lunaReal != NULL){ // ignore this as we are looking only for DFs

                return result;
                    
            }

            luna_cast* lunaCast = dynamic_cast<luna_cast*>(expression);
            if (lunaCast != NULL){ // DFs could be inside the cast

                expr* nextExpr = lunaCast->expr_;
                result = getDFsFromExpression(nextExpr);
                return result;

            }

            bin_op* lunaBinOp = dynamic_cast<bin_op*>(expression);
            if (lunaBinOp != NULL){ // DFs could be inside one of the operands

                std::set<std::string> leftResult = getDFsFromExpression(lunaBinOp->left_);
                for (auto j : leftResult){
                    result.insert(j);
                }
                std::set<std::string> rightResult = getDFsFromExpression(lunaBinOp->right_);
                for (auto j : rightResult){
                    result.insert(j);
                }
                return result;

            }

        }

        // this function gets an import name and its args when it's called, and returns a map of DF names and whether DF is used or initialized, and DFs position in an args list
        // TODO refactor (ParsedArguments)
        std::map<std::string, UseDef> parseImportVFArguments(std::vector<expr*> expressions, std::string importName){

            std::map<std::string, UseDef> result = {};

            std::cout << "Parsing " << expressions.size() << " import arguments" << std::endl;

            for (int i = 1; i <= expressions.size(); i++){

                if (importAndPositionToUseDef[importName][i] == use) { // expression is used

                    std::cout << "Arg " << i << " is used" << std::endl;

                    std::set<std::string> tempResult = getDFsFromExpression(expressions[i - 1]);
                    for (auto j: tempResult){
                        result[j] = use, i;
                    }

                } else if (importAndPositionToUseDef[importName][i] == def) { // expression is defined //TODO expressions other than simply a DF can not be initialized

                    std::cout << "Arg " << i << " is defined" << std::endl;

                    std::set<std::string> tempResult = getDFsFromExpression(expressions[i - 1]);
                    for (auto j: tempResult){
                        result[j] = def;
                    }

                } else if (importAndPositionToUseDef[importName][i] == useAndDef){ // both used and defined

                    std::cout << "Arg " << i << " is both used and defined" << std::endl;

                    std::set<std::string> tempResult = getDFsFromExpression(expressions[i - 1]);
                    for (auto j: tempResult){
                        result[j] = useAndDef;
                    }
                } else {

                    std::cout << "Arg " << i << " has undefined usage or/and definition!" << std::endl;

                }

            }

            return result;

        }

        // this function gets a subprogram name and its args (expressions) when it's called, and returns a map of DF names and whether DF is used or initialized
        // TODO refactor (ParsedArguments)
        ParsedArguments parseStructuredCFVFArguments(std::vector<expr*> expressions, std::string subName, std::map<int, UseDef> DFPositionsToUseDef){ 

            ParsedArguments pa;

            std::map<std::string, UseDef> result = {};

            std::cout << "Parsing " << expressions.size() << " structured CF arguments (" << subName << ")" <<std::endl;

            for (int i = 1; i <= expressions.size(); i++){

                if (DFPositionsToUseDef[i] == use) { // expression is used

                    std::cout << "Arg " << i << " is used" << std::endl;

                    std::set<std::string> tempResult = getDFsFromExpression(expressions[i - 1]);
                    for (auto j: tempResult){
                        result[j] = use;
                    }

                } else if (DFPositionsToUseDef[i] == def) { // expression is defined //TODO can be anything other than DF as luna_string be initialized?

                    std::cout << "Arg " << i << " is defined" << std::endl;

                    std::set<std::string> tempResult = getDFsFromExpression(expressions[i - 1]);
                    for (auto j: tempResult){
                        result[j] = def;
                    }

                } else if (DFPositionsToUseDef[i] == useAndDef){ // both used and defined //TODO can be anything other than DF as luna_string be initialized? i.e. we can't initialize a + b!

                    std::cout << "Arg " << i << " is both used and defined" << std::endl;

                    std::set<std::string> tempResult = getDFsFromExpression(expressions[i - 1]);
                    for (auto j: tempResult){
                        result[j] = useAndDef;
                    }

                } else {

                    std::cout << "Arg " << i << " has undefined usage or/and definition!" << std::endl;

                }

            }

            pa.DFNameToUseDef = result;
            return pa;

        }

        // TODO: swap "if" cascade for a switch(enumerated)
        // TODO: create proper return of a VTO: split this giant function for smaller ones, etc
        // enterVF is a recursive function, which starts scanning at main(). It returns map of which DFs are used in current VF, and which are defined in it
        // It takes a set of current declared DFs, so in the case of a (for example) "for" or "while" operators,
        // we can detect name duplicates
        VertexTransferObject enterVF(std::vector<std::string> currentNamespaceDFs, block* currentBlock, int currentDepth){
            
            std::cout << "Entering block " << currentBlock << std::endl;

            // currentSubDFs = currentNamespaceDFs + DFs declared in this block
            // enterVF map of DFs that are visible in the scope where enterVF was called (i.e. only from currentNamespaceDFs)!
            // DFs declared in this block will not be returned, but will be analysed locally
            std::vector<std::string> currentSubDFs = scanForDFDecls(currentBlock);

            for (std::string currentDFName: currentNamespaceDFs){ // all DFs must be sent into recursive enterVFs
                currentSubDFs.push_back(currentDFName);
            }
            //TODO check for duplicates in DFs and possibly report an error

            //std::set<std::string> currentNamespaceUseDFs = {};
            //std::set<std::string> currentNamespaceDefDFs = {};
            //std::set<std::string> currentSubUseDFs = {};
            //std::set<std::string> currentSubDefDFs = {};
            
            VertexTransferObject vto;
            vto.inside = {};
            //vto.usePosition = {};
            //vto.defPosition = {};
            vto.DFPositionToUseDef = {};
            

            for (statement* currentCF: *(currentBlock->statement_seq_->statements_)){

                std::cout << std::endl;
                
                /*for_statement* forCF = dynamic_cast<for_statement*>(currentCF);
                if (forCF != NULL){ //TODO "for" is a weird thing

                    // "for" allows DF declarations

                    std::cout << "for statement:" << std::endl;
                    VertexTransferObject vto = enterVF(currentSubDFs, forCF->block_); //TODO perhaps send struct/union that has info about header (for i in range ...)?
                    //std::map<int, UseDef> positionToUseDef = enterVF(currentSubDFs, forCF->block_);

                    std::set<std::string> useDFs = {};
                    std::set<std::string> defDFs = {};
                    for (int j = 1; j <= positionToUseDef.size(); j++){

                    }

                    (this->vertexCount)++; // first increment, then create a vertex! vertex numeration starts from
                    (this->vertex)[this->vertexCount] = Vertex(forVF, vto.use, vto.def, vto.inside); //create a vertex

                    continue;

                }*/

                /*if_statement* ifCF = dynamic_cast<if_statement*>(currentCF);
                if (ifCF != NULL){ //TODO "if" is a weird thing
                    
                    // "if" allows DF declarations

                    std::cout << "if statement:" << std::endl;
                    VertexTransferObject vto = enterVF(currentSubDFs, ifCF->block_); //TODO perhaps send struct/union that has info about header (if a != b ..)?

                    Vertiex vertex = Vertex(ifVF, vto.use, vto.def, vto.inside); //create a vertex
                    (this->vertexCount)++;
                    (this->vertices)[this->vertexCount] = vertex;

                    continue;

                }*/
                
                /*while_statement* whileCF = dynamic_cast<while_statement*>(currentCF);
                if (whileCF != NULL){ //TODO "while" is a weird thing
                    // "while" allows DF declarations

                    std::cout << "while statement:" << std::endl;
                    VertexTransferObject vto = enterVF(currentSubDFs, whileCF->block_); //TODO perhaps send struct/union that has info about header (while a != b ..)?

                    Vertex vertex = Vertex(whileVF, vto.use, vto.def, vto.inside); //create a vertex
                    (this->vertexCount)++;
                    (this->vertices)[this->vertexCount] = vertex;

                    continue;
                }*/

                /*let_statement* letCF = dynamic_cast<let_statement*>(currentCF); //TODO not sure how; need more examples of "let" usage
                if (letCF != NULL){
                    // TODO build graph
                    // let does not allow DF declarations?
                    std::cout << "let statement:" << std::endl;
                    enterVF(currentSubDFs, letCF->block_);
                    continue;
                }*/

                /*
                VertexTransferObject vto;
                vto.inside = {};
                vto.DFPositionToUseDef = {};*/

                cf_statement* cfCF = dynamic_cast<cf_statement*>(currentCF); // found a code fragment call
                if (cfCF != NULL){
                    // import does not allow DF declarations
                    // but structured CF does

                    std::cout << "CF statement:";
                    std::string subName = *(cfCF->code_id_->value_);

                    std::vector<expr*> DFExpressions = *(cfCF->opt_exprs_->exprs_seq_->expr_); // get a list of expressions that are used as args in this CF call

                    auto search = (this->imports).find(subName);
                    if (search != (this->imports).end()) { // import

                        std::cout << " import" << std::endl;
                        
                        std::map<std::string, UseDef> DFNameToUseDef = parseImportVFArguments(DFExpressions, subName); // parse those expressions to get actual DFs from them
                        
                        std::cout << "DFNameToUseDef size: " << DFNameToUseDef.size() << std::endl;

                        std::set<std::string> useDFs = {}; // for creating a vertex for current cf operator
                        std::set<std::string> defDFs = {};
                        for (auto i: DFNameToUseDef){
                            if (i.second == use) {
                                useDFs.insert(i.first);
                                std::cout << i.first << " is used" << std::endl;
                            } else if (i.second == def){
                                defDFs.insert(i.first);
                                std::cout << i.first << " is defined" << std::endl;
                            } else if (i.second == useAndDef) { // both
                                useDFs.insert(i.first);
                                defDFs.insert(i.first);
                                std::cout << i.first << " is used and defined" << std::endl;
                            } else {
                                std::cout << i.first << " has undefined usage or/and definition!" << std::endl;
                            }
                        }

                        (this->vertexCount)++; // first increment, then create a vertex! vertex numeration starts from 1
                        (this->vertices).insert(std::make_pair(this->vertexCount, Vertex(importVF, useDFs, defDFs, {}, subName, currentDepth)));;
                        vto.inside.insert(&(this->vertices).find(this->vertexCount)->second); // add it to the list so it could be sent into inside of another Vertex
                        std::cout << "Created a vertex number " << this->vertexCount
                                  << " with a type " << (this->vertices).find(this->vertexCount)->second.getVertexType()
                                  << " and an address of " << &((this->vertices).find(this->vertexCount)->second) << std::endl;

                        // update vto //TODO wrong! we're in a loop here, check for previous vto containments
                        for (int i = 1; i <= DFExpressions.size(); i++){

                            if (importAndPositionToUseDef.find(subName)->second.find(i)->second == use){
                                vto.DFPositionToUseDef.insert(std::make_pair(i, use));
                            } else if (importAndPositionToUseDef.find(subName)->second.find(i)->second == def) {
                                vto.DFPositionToUseDef.insert(std::make_pair(i, def));
                            } else if (importAndPositionToUseDef.find(subName)->second.find(i)->second == useAndDef){
                                vto.DFPositionToUseDef.insert(std::make_pair(i, useAndDef));
                            } else {
                                std::cout << "ERROR: can't update import VTO!" << std::endl;
                            }
                        }

                    } else { // structured CF

                        std::cout << " structured CF" << std::endl;

                        block* cfBlock = (this->structuredCFBlocks)[subName];

                        //localVto is used in parsing arguments
                        //TODO currentSubDFs must be args of this sub (but not expressions in a call) (what?)
                        VertexTransferObject localVto = enterVF(currentSubDFs, cfBlock, currentDepth + 1);

                        std::cout << "DEBUG: localVto check" << std::endl;
                        std::cout << subName << std::endl;
                        for (auto i: localVto.DFPositionToUseDef){
                            std::cout << i.first << " " << i.second << std::endl;
                        }
                        std::cout << "END" << std::endl;
                        std::cout << std::endl;

                        // using localVto here
                        //std::map<std::string, UseDef> DFNameToUseDef = parseStructuredCFVFArguments(DFExpressions, subName, localVto.DFPositionToUseDef);
                        ParsedArguments pa = parseStructuredCFVFArguments(DFExpressions, subName, localVto.DFPositionToUseDef);
                        std::cout << "PARSED ARGUMENTS" << std::endl;
                        for (auto i: pa.DFNameToUseDef){
                            std::cout << i.first << " " << i.second << std::endl;
                        }
                        std::cout << std::endl;

                        std::set<std::string> useDFs = {};
                        std::set<std::string> defDFs = {};
                        for (auto i: pa.DFNameToUseDef){
                            if (i.second == use) {
                                useDFs.insert(i.first);
                                std::cout << i.first << " is used" << std::endl;
                            } else if (i.second == def) {
                                defDFs.insert(i.first);
                                std::cout << i.first << " is defined" << std::endl;
                            } else if (i.second == useAndDef) { // both
                                useDFs.insert(i.first);
                                defDFs.insert(i.first);
                                std::cout << i.first << " is used and defined" << std::endl;
                            } else {
                                std::cout << i.first << " has undefined usage or/and definition!" << std::endl;
                            }
                        }

                        (this->vertexCount)++; // first increment, then create a vertex! vertex numeration starts from 1
                        (this->vertices).insert(std::make_pair(this->vertexCount, Vertex(subVF, useDFs, defDFs, localVto.inside, subName, currentDepth)));
                        vto.inside.insert(&(this->vertices).find(this->vertexCount)->second); // add it to the list so it could be sent into inside of another Vertex
                        std::cout << "Created a vertex number " << this->vertexCount
                                  << " with a type " << (this->vertices).find(this->vertexCount)->second.getVertexType()
                                  << " and an address of " << &((this->vertices).find(this->vertexCount)->second) << std::endl;

                        // update vto //TODO
                        /*for (int i = 1; i <= DFExpressions.size(); i++){
                            if (pa. == use){
                                vto.DFPositionToUseDef.insert(std::make_pair(i, use));
                            } else if ( == def) {
                                vto.DFPositionToUseDef.insert(std::make_pair(i, def));
                            } else if ( == useAndDef){
                                vto.DFPositionToUseDef.insert(std::make_pair(i, useAndDef));
                            } else {
                                std::cout << "ERROR: can't update import VTO!" << std::endl;
                            }
                        }*/

                    }

                    continue;
                }

                std::cout << "Unknown statement" << std::endl;
            }

            std::cout << std::endl;

            return vto;

        }

        // this function binds vertices to each other; it is initially called for "main" vertex and uses its "inside" field to call itself recursively on new vertices
        // function initializes "in" and "out" of every vertex
        //TODO modify Vertex so that the link will contain what DF is used/initialized; this is needed for analysis
        void bind(Vertex* currentVertex){

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
                    if (finding != map.end()){
                        return &(finding->second);
                    } else {
                        return NULL;
                    }
                }

            };

            DFCoordinates coordinates = DFCoordinates();
            for (Vertex* internalVertex: currentVertex->getInsideSet()){

                for (std::string DFName: internalVertex->getUseSet()){ // adding every use of every DF
                    coordinates.addDFUse(DFName, internalVertex);
                    std::cout << "bind: added " + DFName + " to use-coordinates" << std::endl;
                }

                if (internalVertex->getVertexType() != importVF) { // vertex has a block, use recursion
                    bind(internalVertex);
                }

            }

            for (Vertex* internalVertex: currentVertex->getInsideSet()){

                for (std::string DFName: internalVertex->getDefSet()){ // now find what defined DFs are used, and where exactly
                    std::vector<Vertex*>* maybeUses = coordinates.getDFUses(DFName);
                    if (maybeUses != NULL){
                        std::cout << "bind: found used DF: " + DFName << std::endl;
                        for (Vertex* it: *maybeUses){ // bind current Vertex to all that uses its result
                            internalVertex->addOut(it);
                            it->addIn(internalVertex);
                        }
                    } else {
                        //TODO throw exception
                        std::cout << "bind: found unused DF: " + DFName << std::endl;
                    }
                }

            }

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

            VertexTransferObject vto = enterVF({}, (this->structuredCFBlocks)["main"], 1);

            (this->vertexCount)++; // first increment, then create a vertex! vertex numeration starts from ?
            (this->vertices).insert(std::make_pair(this->vertexCount, Vertex(subVF, {}, {}, vto.inside, "main", 0)));
            //vto.inside.insert(&(this->vertices).find(this->vertexCount)->second); // add it to the list so it could be sent into inside of another Vertex TODO why here?

            std::cout << "Created a [MAIN] vertex number " << this->vertexCount
                      << " with a type " << (this->vertices).find(this->vertexCount)->second.getVertexType()
                      << " and an address of " << &((this->vertices).find(this->vertexCount)->second) << std::endl;

            Vertex mainVertex = (this->vertices).find(this->vertexCount)->second;

            // 3. bind vertices to eachother
            bind(&mainVertex);

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

        }

};