#include "enums.hpp"
#include "vertices.cpp"
#include "ids.cpp"
#include "expr.cpp"
#include "json_reporter.cpp"

#include <chrono>
#include <fstream>
#include <queue>

using ns = std::chrono::nanoseconds;

class DDG {

    public:

        ast* astobj; // input AST

        std::string fileName;

        void findSubs(ast* astobj){

            for (auto subDef: (*(astobj->program_->sub_defs))){

                import* importDecl = dynamic_cast<import*>(subDef);
                luna_sub_def* subDecl = dynamic_cast<luna_sub_def*>(subDef);
                
                if (subDecl != NULL) { // found a structured CF

                    std::cout << "Non-import (structured sub) found: ";
                    std::string subName = *(subDecl->code_id_->get_value());

                    if (subName == "main"){ // main found
                        std::cout << "main()!" << std::endl;
                    } else {
                        std::cout << subName << std::endl;
                    }

                    // save args of this CF to use it in initializeVertex
                    std::vector<DeclaredArg> declaredArgs = {};
                    if (subDecl->params_->param_seq_ != nullptr){
                        for (auto arg : *(subDecl->params_->param_seq_->params_)) {
                            ValueType type;
                            std::string lunaType = *(arg->type_->value_);
                            if (lunaType == "int") { //todo check all this
                                type = intType;
                            } else if (lunaType == "real") {
                                type = realType;
                            } else if (lunaType == "string") {
                                type = stringType;
                            } else if (lunaType == "value") {
                                type = valueType;
                            } else if (lunaType == "name") {
                                type = nameType;
                            } else {
                                type = noneType;
                            }
                            declaredArgs.push_back( { *(arg->name_->value_), type } );
                        }
                    }

                    auto subCFDecl = CFDECLARATIONS.find(subName);
                    CFDeclaration* newDeclaration = new CFDeclaration(subName, subCF, declaredArgs, subDecl->block_, subDecl->line_, fileName);
                    if (subCFDecl != CFDECLARATIONS.end()) {

                        REPORTS.push_back(JsonReporter::createSYN6_2(subCFDecl->second, newDeclaration));
                        CFDECLARATIONS.erase(subName); // LuNA overwrites declarations with same names and uses the last one
                    }
                    CFDECLARATIONS.insert( { subName, newDeclaration } );

                } else if (importDecl != NULL) { // found an import

                    std::cout << "Import (atomic sub) found: ";
                    std::string importName = *(importDecl->luna_code_id_->get_value());
                    std::cout << importName << std::endl;

                    std::vector<DeclaredArg> declaredArgs = {};
                    if (importDecl->params_->seq_ != nullptr){
                        for (auto arg : *(importDecl->params_->seq_->params_)) {
                            ValueType type;
                            std::string lunaType = *(arg->type_->value_);
                            if (lunaType == "int") { //todo check all this
                                type = intType;
                            } else if (lunaType == "real") {
                                type = realType;
                            } else if (lunaType == "string") {
                                type = stringType;
                            } else if (lunaType == "value") {
                                type = valueType;
                            } else if (lunaType == "name") {
                                type = nameType;
                            } else {
                                type = noneType;
                            }
                            declaredArgs.push_back( { "", type } );
                        }
                    }

                    auto importCFDecl = CFDECLARATIONS.find(importName);
                    CFDeclaration* newDeclaration = new CFDeclaration(importName, importCF, declaredArgs, nullptr, importDecl->line_, fileName);
                    if (importCFDecl != CFDECLARATIONS.end()) {
                        REPORTS.push_back(JsonReporter::createSYN6_1(importCFDecl->second, newDeclaration));
                        CFDECLARATIONS.erase(importName); // LuNA overwrites declarations with same names and uses the last one
                    }
                    CFDECLARATIONS.insert( { importName, newDeclaration } );

                } else {
                    std::cout << "INTERNAL ERROR: unknown CF of name " + subDef->to_string() 
                    + " found.\nFunction: findSubs()" << std::endl;
                }
                    
            }

        }

        // this function binds vertices to each other
        // currently function initializes "in" and "out" of imports using BASENAMES
        void bindVertices(){
            std::cout << "\nbindVertices started\n" << std::endl;
            // go through all basenames and bind imports depending on what info BASENAMES has
            for (BaseDFName* baseName: BASENAMES){
                std::cout << "checking \"" + baseName->getName() << "\":" << std::endl;
                std::map<int, std::pair<std::vector<Vertex*>*, std::vector<Vertex*>*>> map = baseName->getMap();
                for (auto entry: map){
                    for (auto useVertex: *(entry.second.first)){
                        for (auto defVertex: *(entry.second.second))
                            defVertex->bindTo(useVertex, baseName);
                    }
                }
            }
            std::cout << "\nbindVertices finished\n" << std::endl;
        }

    public:

        // SYN5.2, 3, 6, 7, 8 if both use and def sets are empty
        // SEM4 is def set is not empty, but use is empty
        void checkUnusedIdentifiers(){
            for (auto vertex: VERTICES){
                for (auto nameIdPair: vertex->getDeclaredInsideIdsMap()){
                    Identifier* identifier = nameIdPair.second;
                    if (identifier->getUseSet().size() == 0) {
                        if (identifier->getDefSet().size() == 0) { // SYN5
                            switch (identifier->getClass()) {
                                case letNameClass: {
                                    REPORTS.push_back(JsonReporter::createSYN5_2(
                                        identifier
                                    ));
                                    break;
                                }
                                case baseDFNameClass: {
                                    REPORTS.push_back(JsonReporter::createSYN5_3(
                                        identifier
                                    ));
                                    break;
                                }
                                case mutableArgNameClass:
                                case immutableArgNameClass:
                                {
                                    REPORTS.push_back(JsonReporter::createSYN5_6(
                                        identifier
                                    ));
                                    break;
                                }
                                case forIteratorNameClass: {
                                    REPORTS.push_back(JsonReporter::createSYN5_7(
                                        identifier
                                    ));
                                    break;
                                }
                                case whileIteratorNameClass: {
                                    REPORTS.push_back(JsonReporter::createSYN5_8(
                                        identifier
                                    ));
                                    break;
                                }
                            }
                        } else { // SEM4
                            REPORTS.push_back(JsonReporter::createSEM4(
                                identifier
                            ));
                        }
                    }
                }
            }
        }

        // SYN5.4, SYN5.5
        void checkUnusedCFs(){
            for (auto cfDeclaration : CFDECLARATIONS){
                if (!cfDeclaration.second->isUsed) {
                    if (cfDeclaration.second->type == importCF)
                        REPORTS.push_back(JsonReporter::createSYN5_4(
                            cfDeclaration.second
                        ));
                    else
                        REPORTS.push_back(JsonReporter::createSYN5_5(
                            cfDeclaration.second
                        ));
                }
            }
        }

        // this function uses breadth search to find cycles in DDG, as this indicates cyclic dependencies
        // SEM3.2
        void checkCyclicDependence(){

            class VertexCycleDependence {
            
            public:

                std::vector<Vertex*> callstack;

                VertexCycleDependence(std::vector<Vertex*> iCallstack) : callstack(iCallstack) {};

                VertexCycleDependence(const VertexCycleDependence& iVCD) : callstack(iVCD.callstack) {};

                bool operator<(const VertexCycleDependence& other) const { // todo check

                    if (this->callstack.size() < other.callstack.size()) {
                        return true;
                    } else if (this->callstack.size() > other.callstack.size()) {
                        return false;
                    } else { // check this stuff
                        int biggerTimes = 0;
                        for (auto thisElement : this->callstack) {
                            bool found = false;
                            for (auto otherElement : other.callstack) {
                                if (thisElement == otherElement) {
                                    break;
                                }
                            }
                            if (!found) {
                                return false;
                            }
                        }
                        return false; //todo why
                    }
                }

                static std::set<VertexCycleDependence> DFS(Vertex* vertex, std::set<Vertex*> marked, std::vector<Vertex*> callstack) {
                    std::set<VertexCycleDependence> foundCycles = {};
                    if (marked.find(vertex) != marked.end()) {
                        // already marked -> cycle found, i.e. find this vertex in a callstack and create a cycle
                        VertexCycleDependence newFoundCycle({});
                        // find last same vertex, i.e. beginning of the cycle
                        int lastSameVertex = -1;
                        for (int i = 0; i < callstack.size(); i++) {
                            if (vertex == callstack[i])
                                lastSameVertex = i;
                        }
                        // now create a cycle beginning from lastSameVertex up until the callstack end
                        for (int i = lastSameVertex; i < callstack.size(); i++) {
                            newFoundCycle.callstack.push_back(callstack[i]);
                        }
                        foundCycles.insert(newFoundCycle);
                        return foundCycles;
                    }
                    callstack.push_back(vertex);
                    marked.insert(vertex);
                    for (auto nextBinding : vertex->getOutSet()) {
                        if (nextBinding->getId()->getClass() == indexedDFNameClass && dynamic_cast<IndexedDFName*>(nextBinding->getId())->getExpressionsVector().size() > 0) {
                            continue; // indexed DF, ingore it for now TODO do cool things after DFTS
                        } else {
                            // found simple DF or iterator
                            for (auto newFoundCycle : DFS(nextBinding->getPointerTo(), marked, callstack)) {
                                if (foundCycles.find(newFoundCycle) == foundCycles.end())
                                    foundCycles.insert(newFoundCycle);
                            }
                        }
                    }
                    return foundCycles;
                }

            };

            std::set<VertexCycleDependence> foundCycles = {};

            // now use DFS for every Vertex
            for (auto vertex : VERTICES) {
                for (auto additionalFoundCycle : VertexCycleDependence::DFS(vertex, {}, {})) {
                    if (foundCycles.find(additionalFoundCycle) == foundCycles.end()) {
                        foundCycles.insert(additionalFoundCycle);
                    }
                }
            }

            for (auto cycle : foundCycles) {
                REPORTS.push_back(JsonReporter::createSEM3_2(cycle.callstack));
            }
        }

        //todo rename codes
        // this function goes through BASENAMES and finds few types of errors:
        // 1. multiple DF initialization (03)
        // 2. using uninitialized DFs (05)
        // 3. unused DF (10)
        void checkBaseNameSet(){

            for (BaseDFName* bn: BASENAMES){
                auto bnMap = bn->getMap();
                for (auto sizeAndUseDefs: bnMap){

                    int size = sizeAndUseDefs.first;
                    std::vector<Vertex*> uses = *(sizeAndUseDefs.second.first);
                    std::vector<Vertex*> defs = *(sizeAndUseDefs.second.second);

                    // multiple initialization of a DF
                    if (size == 0){ // simple DFs
                        if (defs.size() > 1){
                            /*std::string report = "ERROR: multiple initialization of a DF " + bn->getName() + " in lines:\n";
                            for (auto def: defs){
                                report += (std::to_string(def->getLine()) + " ");
                            }
                            report += "\n";
                            errorReports.push_back(report);*/
                            // error code: 03
                            // details: df
                            //todo callstacks
                            REPORTS.push_back(JsonReporter::createSEM2_1(
                                bn
                            ));
                        }
                    } else { // indexed DFs
                        //TODO add warnings?
                        // proper implementation requires expressions parsing
                    }

                    // unused DF 1
                    if (uses.size() == 0){
                        /*std::string report = "ERROR: unused DF " + bn->getName() + " with " + std::to_string(size) + " indices\n";
                        errorReports.push_back(report);*/
                        // error code: 10
                        // details: df
                        //todo callstacks
                        REPORTS.push_back(JsonReporter::createSEM4(
                            bn
                        ));
                    } else {
                        // using uninitialized DFs
                        if (defs.size() == 0){
                            /*std::string report = "ERROR: using uninitialized DF " + bn->getName() + " with " + std::to_string(size) +
                            " indices at lines:\n";
                            for (auto use: uses){
                                report += (std::to_string(use->getLine()) + " ");
                            }
                            report += "\n";
                            errorReports.push_back(report);*/
                            // error code: 05
                            // details: df
                            //todo callstacks
                            REPORTS.push_back(JsonReporter::createSEM3_1(
                                bn
                            ));
                        }
                    }

                }
                // unused DF 2
                if (bnMap.size() == 0){
                    // error code: 10
                    // details: df
                    //todo callstacks
                    REPORTS.push_back(JsonReporter::createSEM4(
                        bn
                    ));
                }
            }

        }

        void checkConstantConditions(){
            for (auto vertex: VERTICES){
                if (vertex->getVertexType() == ifVF){
                    IfVertex* ifVertex = dynamic_cast<IfVertex*>(vertex);
                    Expression conditionConstant = ifVertex->getConditionExpr()->getAsConstant();
                    if (conditionConstant.getExpressionType() != noneNode){
                        switch(conditionConstant.getExpressionType()){
                            case realNode: {
                                if (std::stod(conditionConstant.getConstant()) == 0){
                                    REPORTS.push_back(JsonReporter::createSEM5(
                                        false,
                                        ifVertex->getConditionExpr()->getASTExpr()->to_string(),
                                        fileName,
                                        ifVertex->getLine(),
                                        "if"
                                    ));
                                } else {
                                    REPORTS.push_back(JsonReporter::createSEM5(
                                        true,
                                        ifVertex->getConditionExpr()->getASTExpr()->to_string(),
                                        fileName,
                                        ifVertex->getLine(),
                                        "if"
                                    ));
                                }
                                continue;
                            }
                            case intNode: {
                                std::string conditionConstantString = conditionConstant.getConstant();
                                if (std::stoi(conditionConstantString) == 0) {
                                    REPORTS.push_back(JsonReporter::createSEM5(
                                        false,
                                        ifVertex->getConditionExpr()->getASTExpr()->to_string(),
                                        fileName,
                                        ifVertex->getLine(),
                                        "if"
                                    ));
                                } else {
                                    REPORTS.push_back(JsonReporter::createSEM5(
                                        true,
                                        ifVertex->getConditionExpr()->getASTExpr()->to_string(),
                                        fileName,
                                        ifVertex->getLine(),
                                        "if"
                                    ));
                                }
                                continue;
                            }
                            default:
                                std::cout << "INTERNAL ERROR: checkConstantConditions reached default at if" << std::endl;
                        }
                    }
                }

                if (vertex->getVertexType() == whileVF){
                    WhileVertex* whileVertex = dynamic_cast<WhileVertex*>(vertex);
                    Expression conditionConstant = whileVertex->getConditionExpr()->getAsConstant();
                    if (conditionConstant.getExpressionType() != noneNode){
                        switch(conditionConstant.getExpressionType()){
                            case realNode:
                                if (std::stod(conditionConstant.getConstant()) == 0) {
                                    REPORTS.push_back(JsonReporter::createSEM5(
                                        false,
                                        whileVertex->getConditionExpr()->getASTExpr()->to_string(),
                                        fileName,
                                        whileVertex->getLine(),
                                        "while"
                                    ));
                                } else {
                                    REPORTS.push_back(JsonReporter::createSEM5(
                                        true,
                                        whileVertex->getConditionExpr()->getASTExpr()->to_string(),
                                        fileName,
                                        whileVertex->getLine(),
                                        "while"
                                    ));
                                }
                                continue;
                            case intNode: {
                                std::string conditionConstantString = conditionConstant.getConstant();
                                if (std::stoi(conditionConstantString) == 0) {
                                    REPORTS.push_back(JsonReporter::createSEM5(
                                        false,
                                        whileVertex->getConditionExpr()->getASTExpr()->to_string(),
                                        fileName,
                                        whileVertex->getLine(),
                                        "while"
                                    ));
                                } else {
                                    REPORTS.push_back(JsonReporter::createSEM5(
                                        true,
                                        whileVertex->getConditionExpr()->getASTExpr()->to_string(),
                                        fileName,
                                        whileVertex->getLine(),
                                        "while"
                                    ));
                                }
                                continue;
                            }
                            default:
                                std::cout << "INTERNAL ERROR: checkConstantConditions reached default at while" << std::endl;
                        }
                    }
                }

            }
        }

        // this function accepts list of errors to find and tries to find them in the created graph and BASENAMES
        void findErrors(){

            std::cout << "\ncheckBaseNameSet started\n" << std::endl;
            checkBaseNameSet();
            std::cout << "\ncheckBaseNameSet finished\n" << std::endl;

            std::cout << "\ncheckConstantConditions started\n" << std::endl;
            checkConstantConditions();
            std::cout << "\ncheckConstantConditions finished\n" << std::endl;

            std::cout << "\ncheckUnusedIdentifiers started\n" << std::endl;
            checkUnusedIdentifiers();
            std::cout << "\ncheckUnusedIdentifiers finished\n" << std::endl;

            std::cout << "\ncheckUnusedCFs started\n" << std::endl;
            checkUnusedCFs();
            std::cout << "\ncheckUnusedCFs finished\n" << std::endl;

            std::cout << "\ncheckCyclicDependence started\n" << std::endl;
            checkCyclicDependence();
            std::cout << "\ncheckCyclicDependence finished\n" << std::endl;

        }

        DDG(ast* astObjectIn, std::ostream* outputTarget, std::string fileName){

            auto graphBuildStart = std::chrono::steady_clock::now();
            auto graphBuildStartSystem = std::chrono::system_clock::now();

            this->fileName = fileName;

            this->astobj = astObjectIn;

            // 1. find use- and def- atomic CFs

            std::cout << "\n============ Creating DDG ============" << std::endl;

            this->findSubs(astobj);

            // 2. create all the vertices
            auto mainDeclaration = CFDECLARATIONS.find("main");
            if (mainDeclaration != CFDECLARATIONS.end()) {
                CFDECLARATIONS.find("main")->second->isUsed = true;
                SubVertex* mainVertex = new SubVertex("main", nullptr, subVF, 1, mainDeclaration->second->line, fileName, mainDeclaration->second->cfBlock, nullptr, mainDeclaration->second, {}, {}, {}, mainDeclaration->second->declaredArgs);
                //VERTICES.push_back(mainVertex);
                std::cout << "Created a [MAIN] vertex: " << mainVertex << std::endl;
                std::cout << "Initializing [MAIN] vertex" << std::endl;
                if (mainVertex->initializeVertex()) {
                    VERTICES.push_back(mainVertex);
                } else {
                    std::cout << "INTERNAL ERROR: not able to initialize [MAIN] vertex" << std::endl;
                }
                
            } else {
                REPORTS.push_back(JsonReporter::createSYN7());
                std::cout << "INTERNAL ERROR: No main found" << std::endl;
                for (auto r: REPORTS){
                    *outputTarget << r << std::endl;//todo does not work as a part of the ADAPT
                }
                return;
            }

            // 3. bind vertices to eachother
            bindVertices();

            auto graphBuildEnd = std::chrono::steady_clock::now();
            auto graphBuildEndSystem = std::chrono::system_clock::now();

            auto graphBuildTotal = std::chrono::duration_cast<ns>(graphBuildEnd - graphBuildStart).count();
            auto graphBuildTotalSystem = std::chrono::duration_cast<ns>(graphBuildEndSystem - graphBuildStartSystem).count();

            *outputTarget << "Total vertices: " << VERTICES.size() << std::endl << std::endl; 
            for (Vertex* vertex : VERTICES){
                vertex->printInfo(outputTarget);
                *outputTarget << std::endl;
            }

            *outputTarget << "BaseDFNames:" << std::endl;
            for (BaseDFName* bn: BASENAMES){
                *outputTarget << std::endl;
                *outputTarget << "Name: " << bn->getName() << std::endl;
                *outputTarget << "Declared in function call at line: " << bn->getLine() << std::endl;
                auto map = bn->getMap(); // 1 = use, 2 = def
                for (auto m: map){
                    *outputTarget << "Size: " << m.first << std::endl;
                    std::pair<std::vector<Vertex*>*, std::vector<Vertex*>*> pair = m.second;
                    *outputTarget << "Uses: ";
                    for (auto u: *(pair.first)){
                        *outputTarget << u << " ";
                    }
                    *outputTarget << std::endl;
                    *outputTarget << "Defs: ";
                    for (auto d: *(pair.second)){
                        *outputTarget << d << " ";
                    }
                    *outputTarget << std::endl;
                }
            }

            *outputTarget << "\n============ Created DDG =============" << std::endl;

            // 4. search for errors

            *outputTarget << "\n======== Searching for errors ========\n" << std::endl;

            auto errorsFindStart = std::chrono::steady_clock::now();
            auto errorsFindStartSystem = std::chrono::system_clock::now();

            findErrors();

            auto errorsFindEnd = std::chrono::steady_clock::now();
            auto errorsFindEndSystem = std::chrono::system_clock::now();
            auto errorsFindTotal = std::chrono::duration_cast<ns>(errorsFindEnd - errorsFindStart).count();
            auto errorsFindTotalSystem = std::chrono::duration_cast<ns>(errorsFindEndSystem - errorsFindStartSystem).count();

            if (REPORTS.size() == 0){
                *outputTarget << "\nNo errors found\n" << std::endl;
            } else {
                for (auto r: REPORTS){
                    *outputTarget << r << std::endl;
                }
            }

            *outputTarget << "\nTime to find errors: " << (double)errorsFindTotal / 1000000000 << " seconds" << std::endl;
            *outputTarget << "\nTime to find errors (system): " << (double)errorsFindTotalSystem / 1000000000 << " seconds" << std::endl;

            *outputTarget << "\nTime to build DDG: " << (double)graphBuildTotal / 1000000000 << " seconds" << std::endl;
            *outputTarget << "\nTime to build DDG (system): " << (double)graphBuildTotalSystem / 1000000000 << " seconds" << std::endl;

            // launch.sh (local)
            //std::string jsonOutputPath = "../reporter/found_errors.json";
            // run.sh (adapt)
            std::string jsonOutputPath = "./reporter/found_errors.json";

            std::ifstream inFile(jsonOutputPath);

            if (inFile.good()){ // file already exists

                if (REPORTS.size() != 0){ // DeGSA found some errors
                    std::ifstream t(jsonOutputPath);
                    std::string existingReports(
                        (std::istreambuf_iterator<char>(t)),
                        std::istreambuf_iterator<char>()
                    );

                    std::ofstream outFile(jsonOutputPath);
                    std::string newReports = JsonReporter::createArray(REPORTS);
                    if (existingReports.size() < 10){ // no objects present; todo change this
                        outFile << newReports;
                    } else {
                        existingReports.pop_back();
                        newReports[0] = ',';
                        outFile << existingReports << newReports;
                    }
                    outFile.close();
                } // else we don't change existing file
                
            } else { // file does not exist
                inFile.close();
                std::ofstream outFile(jsonOutputPath);
                outFile << JsonReporter::createArray(REPORTS);
                outFile.close();
            }

        }

};
