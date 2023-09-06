#include "ast1.hpp"
//#include <vector> // already included in ast.hpp
#include <map>
#include <set>
#include <regex>

enum VerticeType {

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

// this class represents a vertice in a DDG
// vertice is a computational fragment (VF), so it could require some (or none) VFs to be ran before, and also allow other VFs to run
// also vertice can contain 
class Vertice {

    private:

        std::set<Vertice*> in; // vertices that must be ran directly before current
        std::set<Vertice*> out; // vertices that require directly current vertice to be ran
        std::set<Vertice*> inside; // vertices that are inside the body of a current vertice

        std::set<std::string> use; // list of DFs that are used in this vertice
        std::set<std::string> def; // list of DFs that are defined in this vertice

        VerticeType verticeType; // type of a vertice

        std::string name; // name of an import/sub

        int depth; // amount of blocks that this vertice is in

        int line; //TODO line in code that this operator is in

    public:

        Vertice(VerticeType verticeType, std::set<std::string> useDFs, std::set<std::string> defDFs, std::set<Vertice*> inside, std::string name, int depth){

            this->verticeType = verticeType;
            this->use = useDFs;
            this->def = defDFs;
            this->inside = inside;

            this->in = {};
            this->out = {};

            this->name = name;

            this->depth = depth;

        }

        // copy constructor //TODO do I need it?
        /*Vertice(Vertice* vertice){

            std::cout << "Copy constructor for Vertice is called" << std::endl;

            this->verticeType = vertice->verticeType;

            this->use = vertice->use;
            this->def = vertice->def;
            this->inside = vertice->inside;

            this->in = vertice->in;
            this->out = vertice->out;

            this->name = vertice->name;

            this->depth = vertice->depth;

        }*/

        VerticeType getVerticeType(){
            return verticeType;
        }

        std::set<std::string> getUseSet(){
            return use;
        }

        std::set<std::string> getDefSet(){
            return def;
        }

        std::set<Vertice*> getInsideSet(){
            return inside;
        }

        std::set<Vertice*> getInSet(){
            return in;
        }

        std::set<Vertice*> getOutSet(){
            return out;
        }

        std::string getName(){
            return name;
        }

        int getDepth(){
            return depth;
        }

        void addIn(Vertice* vertice){
            auto temp = this->in.find(vertice);
            if (temp == this->in.end()){
                this->in.insert(vertice);
            }
        }

        void addOut(Vertice* vertice){
            auto temp = this->out.find(vertice);
            if (temp == this->out.end()){
                this->out.insert(vertice);
            }
        }

        void printInfo(){

            std::cout << "Vertice address: " << this << std::endl;
            std::cout << "Vertice type: ";
            std::cout << this->getVerticeType() << " ";
            switch (this->getVerticeType()){
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

            std::cout << "Vertice depth: " << this->getDepth() << std::endl;

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

// this class is being used as output for enterVF function and helps to initialize Vertice
struct VerticeTransferObject {

    std::set<Vertice*> inside;

    //std::set<int> usePosition;
    //std::set<int> defPosition;

    std::map<int, UseDef> DFPositionToUseDef;

};

class DDG {

    private:

        // IMPORTANT NOTE: std::map required empty constructor; to ignore this requirement you must use insert/find instead of a [] operator

        ast* astobj; // input AST

        // these are initialized in findSubs()
        // positions start with 1
        std::map<std::string, std::set<int>> importDefCFArgs; // list of imports that initialize DFs, and position of args that are declared
        std::map<std::string, std::set<int>> importUseCFArgs; // list of imports that uses DFs, and position of args that are used
        
        // duplicate of previous two fields, but convenient
        // positions start with 1
        std::map<std::string, std::map<int, UseDef>> importAndPositionToUseDef; // this is used to determine if argument in a call will be initialized or used


        bool mainExists; // for checking if program is correct
        block* mainBlock; // needed to start analysis from main

        std::map<std::string, block*> structuredCFBlocks; // list of structured CFs

        std::map<int, Vertice> layerOne; // list of vertices that require no DFs to be started -- bind
        std::map<int, Vertice> vertices; // list of all vertices; vertice numeration starts from one
        int verticeCount; // count of vertices

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
                
                if (subDecl != NULL) {

                    std::cout << "Non-import (structured sub) found: ";
                    std::string subName = *(subDecl->code_id_->get_value());
                    std::cout << subName << std::endl;

                    if (subName == "main"){ // main
                        std::cout << "main() found" << std::endl;
                        this->mainExists = true;
                    }

                    (this->structuredCFBlocks)[subName] = subDecl->block_;

                    continue;
                }

                if (importDecl != NULL) {

                    std::cout << "Import (atomic sub) found: ";
                    std::string importName = *(importDecl->luna_code_id_->get_value());
                    std::cout << importName << std::endl;
                    // save import for using it later
                    (this->imports).insert(importName);

                    std::vector<code_df_param*>* subDefArgs = importDecl->params_->seq_->params_;
                    int argPosition = 0;
                    for (auto arg: *subDefArgs){

                        argPosition++;
                        std::string argType = arg->type_->to_string();

                        if (argType == "name") { // def

                            std::cout << argPosition << " arg is def-arg\n";
                            importDefCFArgs[importName].insert(argPosition);
                            importAndPositionToUseDef[importName][argPosition] = def;

                        } else { // use

                            std::cout << argPosition << " arg is use-arg\n";
                            importUseCFArgs[importName].insert(argPosition);
                            importAndPositionToUseDef[importName][argPosition] = use;

                        }
                        
                    }

                    continue;

                }

                std::cout << "ERROR: Unknown CF found" << std::endl;
                    
            }

            std::cout << "Finished!" << std::endl;
        }

        // TODO: uncertain: use already existing block->opt_dfdecls_->dfdecls_->name_seq_, or scan manually?
        // scanForDFDecls is a function, that for each CF scans for DF declaration (if CF's nature allows it)
        // DFs, according to LuNA rules, must be declared on the very first line, and no other declarations shall follow
        static std::vector<std::string> scanForDFDecls(block* blockobj){
            
            std::cout << "Scanning block for DF declarations" << std::endl;
            std::vector<std::string> DFDecls = {};
            //std::cout << blockobj << std::endl;
            //std::cout << blockobj->opt_dfdecls_ << std::endl;
            //std::cout << blockobj->opt_dfdecls_->dfdecls_ << std::endl;
            //std::cout << blockobj->opt_dfdecls_->dfdecls_->name_seq_ << std::endl;
            //std::cout << blockobj->opt_dfdecls_->dfdecls_->name_seq_->names_ << std::endl; // all of this to fix null pointer error
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

                } else { // both used and defined

                    std::cout << "Arg " << i << " is both used and defined" << std::endl;

                    std::set<std::string> tempResult = getDFsFromExpression(expressions[i - 1]);
                    for (auto j: tempResult){
                        result[j] = useAndDef;
                    }

                }

            }

            return result;

        }

        // this function gets a subprogram name and its args when it's called, and returns a map of DF names and whether DF is used or initialized
        std::map<std::string, UseDef> parseStructuredCFVFArguments(std::vector<expr*> expressions, std::string subName, std::map<int, UseDef> DFPositionsToUseDef){ 

            std::map<std::string, UseDef> result = {};

            std::cout << "Parsing " << expressions.size() << " structured CF arguments" << std::endl;

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

                } else { // both used and defined //TODO can be anything other than DF as luna_string be initialized? i.e. we can't initialize a + b!

                    std::cout << "Arg " << i << " is both used and defined" << std::endl;

                    std::set<std::string> tempResult = getDFsFromExpression(expressions[i - 1]);
                    for (auto j: tempResult){
                        result[j] = useAndDef;
                    }

                }

            }

            return result;

        }

        // TODO: swap "if" cascade for a switch(enumerated)
        // enterVF is a recursive function, which starts scanning at main(). It returns map of which DFs are used, and which are defined
        // It takes a set of current declared DFs, so in the case of a (for example) "for" or "while" operators,
        // we can detect name duplicates
        VerticeTransferObject enterVF(std::vector<std::string> currentNamespaceDFs, block* currentBlock, int currentDepth){
            
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
            
            VerticeTransferObject vto;
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
                    VerticeTransferObject vto = enterVF(currentSubDFs, forCF->block_); //TODO perhaps send struct/union that has info about header (for i in range ...)?
                    //std::map<int, UseDef> positionToUseDef = enterVF(currentSubDFs, forCF->block_);

                    std::set<std::string> useDFs = {};
                    std::set<std::string> defDFs = {};
                    for (int j = 1; j <= positionToUseDef.size(); j++){

                    }

                    (this->verticeCount)++; // first increment, then create a vertice! vertice numeration starts from
                    (this->vertices)[this->verticeCount] = Vertice(forVF, vto.use, vto.def, vto.inside); //create a vertice

                    continue;

                }*/

                /*if_statement* ifCF = dynamic_cast<if_statement*>(currentCF);
                if (ifCF != NULL){ //TODO "if" is a weird thing
                    
                    // "if" allows DF declarations

                    std::cout << "if statement:" << std::endl;
                    VerticeTransferObject vto = enterVF(currentSubDFs, ifCF->block_); //TODO perhaps send struct/union that has info about header (if a != b ..)?

                    Vertice vertice = Vertice(ifVF, vto.use, vto.def, vto.inside); //create a vertice
                    (this->verticeCount)++;
                    (this->vertices)[this->verticeCount] = vertice;

                    continue;

                }*/
                
                /*while_statement* whileCF = dynamic_cast<while_statement*>(currentCF);
                if (whileCF != NULL){ //TODO "while" is a weird thing
                    // "while" allows DF declarations

                    std::cout << "while statement:" << std::endl;
                    VerticeTransferObject vto = enterVF(currentSubDFs, whileCF->block_); //TODO perhaps send struct/union that has info about header (while a != b ..)?

                    Vertice vertice = Vertice(whileVF, vto.use, vto.def, vto.inside); //create a vertice
                    (this->verticeCount)++;
                    (this->vertices)[this->verticeCount] = vertice;

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
                VerticeTransferObject vto;
                vto.inside = {};
                vto.DFPositionToUseDef = {};*/

                cf_statement* cfCF = dynamic_cast<cf_statement*>(currentCF);
                if (cfCF != NULL){
                    // import does not allow DF declarations
                    // but structured CF does

                    std::cout << "CF statement:";
                    std::string subName = *(cfCF->code_id_->value_);
                    //std::map<int, std::string> importPositionsToName; 

                    std::vector<expr*> DFExpressions = *(cfCF->opt_exprs_->exprs_seq_->expr_);

                    auto search = (this->imports).find(subName);
                    if (search != (this->imports).end()) { // import

                        std::cout << " import" << std::endl;
                        
                        std::map<std::string, UseDef> DFNameToUseDef = parseImportVFArguments(DFExpressions, subName);
                        
                        std::cout << "DFNameToUseDef size: " << DFNameToUseDef.size() << std::endl;

                        std::set<std::string> useDFs = {}; // for creating a vertice for current cf operator
                        std::set<std::string> defDFs = {};
                        for (auto i: DFNameToUseDef){
                            if (i.second == use) {
                                useDFs.insert(i.first);
                                std::cout << i.first << " is used" << std::endl;
                            } else if (i.second == def){
                                defDFs.insert(i.first);
                                std::cout << i.first << " is defined" << std::endl;
                            } else { // both
                                useDFs.insert(i.first);
                                defDFs.insert(i.first);
                                std::cout << i.first << " is used and defined" << std::endl;
                            }
                        }

                        (this->verticeCount)++; // first increment, then create a vertice! vertice numeration starts from
                        (this->vertices).insert(std::make_pair(this->verticeCount, Vertice(importVF, useDFs, defDFs, {}, subName, currentDepth)));;
                        vto.inside.insert(&(this->vertices).find(this->verticeCount)->second); // add it to the list so it could be sent into inside of another Vertice
                        std::cout << "Created a vertice number " << this->verticeCount
                                  << " with a type " << (this->vertices).find(this->verticeCount)->second.getVerticeType()
                                  << " and an address of " << &((this->vertices).find(this->verticeCount)->second) << std::endl;

                        // update vto //TODO //how tf this works with out this?
                        /*for (auto i: DFNameToUseDef){
                            vto.DFPositionToUseDef[findDFPosition(subName, i.first)] = i.second;
                        }*/

                    } else { // structured CF

                        std::cout << " structured CF" << std::endl;

                        block* cfBlock = (this->structuredCFBlocks)[subName];

                        VerticeTransferObject localVto = enterVF(currentSubDFs, cfBlock, currentDepth + 1); //TODO currentSubDFs must be args of this sub (but not expressions in a call)
                        std::cout << std::endl;

                        std::map<std::string, UseDef> DFNameToUseDef = parseStructuredCFVFArguments(DFExpressions, subName, localVto.DFPositionToUseDef);

                        std::set<std::string> useDFs = {};
                        std::set<std::string> defDFs = {};
                        for (auto i: DFNameToUseDef){
                            if (i.second == use) {
                                useDFs.insert(i.first);
                                std::cout << i.first << " is used" << std::endl;
                            } else if (i.second == def) {
                                defDFs.insert(i.first);
                                std::cout << i.first << " is defined" << std::endl;
                            } else { // both
                                useDFs.insert(i.first);
                                defDFs.insert(i.first);
                                std::cout << i.first << " is used and defined" << std::endl;
                            }
                        }

                        (this->verticeCount)++; // first increment, then create a vertice! vertice numeration starts from
                        (this->vertices).insert(std::make_pair(this->verticeCount, Vertice(subVF, useDFs, defDFs, localVto.inside, subName, currentDepth)));
                        vto.inside.insert(&(this->vertices).find(this->verticeCount)->second); // add it to the list so it could be sent into inside of another Vertice
                        std::cout << "Created a vertice number " << this->verticeCount
                                  << " with a type " << (this->vertices).find(this->verticeCount)->second.getVerticeType()
                                  << " and an address of " << &((this->vertices).find(this->verticeCount)->second) << std::endl;

                        // update vto //TODO //how tf this works without this?
                        /*for (auto i: DFNameToUseDef){
                            vto.DFPositionToUseDef[findDFPosition(subName, i.first)] = i.second;
                        }*/

                    }

                    continue;
                }

                std::cout << "Unknown statement" << std::endl;
            }

            std::cout << std::endl;

            return vto;

        }

        // this function binds vertices to each other; it is initially called for "main" vertice and uses its "inside" field to call itself recursively on new vertices
        // function initializes "in" and "out" of every vertice
        //TODO modify Vertice so that the link will contain what DF is used/initialized; this is needed for analysis
        //TODO also this does not fucking work
        void bind(Vertice* currentVertice){

            std::map<std::string, std::set<std::pair<UseDef, Vertice*>>*> DFNameToUseDefAndVerticeSet = {};

            for (Vertice* i: currentVertice->getInsideSet()){ // analyze every vertice in this block and initialize DFNameToUseDefAndVertice (and use it later)

                for (std::string DFName: i->getUseSet()){ // analyze DFs that are used in this block

                    auto foundMappings = DFNameToUseDefAndVerticeSet.find(DFName);
                    if (foundMappings != DFNameToUseDefAndVerticeSet.end()) { // found -> update old mapping and also initialize in and out

                        foundMappings->second->insert(std::make_pair(use, i));

                        // find what vertices define it and connect them to current
                        for (auto j: *(foundMappings->second)){ //TODO INFINITE LOOP???????????????????
                            std::cout << j.first << " " << j.second << " " << foundMappings->second->size() << std::endl;
                            if ((j.first == def) || (j.first == useAndDef)){
                                j.second->addOut(i);
                                i->addIn(j.second);
                            }
                        }

                    } else { // not found -> make new mapping
                        
                        std::set<std::pair<UseDef, Vertice*>> newMapping = {};
                        newMapping.insert(std::make_pair(use, i));
                        DFNameToUseDefAndVerticeSet.insert(std::make_pair(DFName, &newMapping));

                    }

                }

                for (std::string DFName: i->getDefSet()){ // analyze DFs that are defined in this block

                    auto foundMappings = DFNameToUseDefAndVerticeSet.find(DFName);
                    if (foundMappings != DFNameToUseDefAndVerticeSet.end()) { // found -> update old mapping and also initialize in and out

                        foundMappings->second->insert(std::make_pair(use, i));

                        // find what vertices define it and connect them to current
                        for (auto j: *(foundMappings->second)){ //TODO INFINITE LOOP???????????????????
                            std::cout << j.first << " " << j.second << " " << foundMappings->second->size() << std::endl;
                            if ((j.first == use) || (j.first == useAndDef)){
                                j.second->addOut(i);
                                i->addIn(j.second);
                            }
                        }

                    } else { // not found -> make new mapping
                        
                        std::set<std::pair<UseDef, Vertice*>> newMapping = {};
                        newMapping.insert(std::make_pair(def, i));
                        DFNameToUseDefAndVerticeSet.insert(std::make_pair(DFName, &newMapping));

                    }

                }

                if (i->getVerticeType() != importVF) { // vertice has a block, use recursion
                    bind(i);
                }

            }

            std::cout << "ended" << std::endl;

        }

        DDG(ast* astObjectIn){
            
            this->verticeCount = 0;
            //imports = new std::set<std::string>; //TODO
            this->imports = {};
            this->vertices = {};

            this->astobj = astObjectIn;

            this->importDefCFArgs = {};
            this->importUseCFArgs = {};

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

            //std::cout << (this->importUseCFArgs["printIndexedValue"]).size() << std::endl;

            std::set<std::string> emptySet = {};

            //std::cout << mainBlock->statement_seq_->to_string() << std::endl;
            //std::cout << mainBlock << std::endl;
            //std::cout << mainBlock->statement_seq_ << std::endl;

            // 2. create all the vertices

            VerticeTransferObject vto = enterVF({}, (this->structuredCFBlocks)["main"], 1);

            (this->verticeCount)++; // first increment, then create a vertice! vertice numeration starts from
            (this->vertices).insert(std::make_pair(this->verticeCount, Vertice(subVF, {}, {}, vto.inside, "main", 0)));
            vto.inside.insert(&(this->vertices).find(this->verticeCount)->second); // add it to the list so it could be sent into inside of another Vertice
            std::cout << "Created a [MAIN] vertice number " << this->verticeCount
                      << " with a type " << (this->vertices).find(this->verticeCount)->second.getVerticeType()
                      << " and an address of " << &((this->vertices).find(this->verticeCount)->second) << std::endl;

            Vertice mainVertice = (this->vertices).find(this->verticeCount)->second;
            // 3. bind vertices to eachother
            bind(&mainVertice); //TODO does not work

            std::cout << "Total vertices: " << verticeCount << std::endl << std::endl; 
            for (int i = 1; i <= verticeCount; i++){

                //Vertice currentVertice = Vertice(vertices.find(i)->second); //TODO fails; copy constructor is not finished
                std::cout << "Vertice number: " << i << std::endl;
                vertices.find(i)->second.printInfo();
                std::cout << std::endl;

            }

            std::cout << "============ Created DDG =============" << std::endl;

            // 4. search for errors
            //TODO search for errors

        }

};