#include "enums.hpp"
#include "vertices.cpp"
#include "ids.cpp"
#include "expr.cpp"
#include "json_reporter.cpp"

#include <chrono>
#include <fstream>

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
                            LunaType type;
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

                    if (CFDECLARATIONS.find(subName) == CFDECLARATIONS.end()) {
                        CFDECLARATIONS.insert( { subName, CFDeclaration(subName, subCF, declaredArgs, subDecl->block_, subDecl->line_) } );
                    } else {
                        //todo duplicate sub decl report
                    }

                } else if (importDecl != NULL) { // found an import

                    std::cout << "Import (atomic sub) found: ";
                    std::string importName = *(importDecl->luna_code_id_->get_value());
                    std::cout << importName << std::endl;

                    std::vector<DeclaredArg> declaredArgs = {};
                    if (importDecl->params_->seq_ != nullptr){
                        for (auto arg : *(importDecl->params_->seq_->params_)) {
                            LunaType type;
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

                    if (CFDECLARATIONS.find(importName) == CFDECLARATIONS.end()) {
                        CFDECLARATIONS.insert( { importName, CFDeclaration(importName, importCF, declaredArgs, nullptr, importDecl->line_) } );
                    } else {
                        //todo duplicate import decl report
                    }

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

        // TODO
        // this function uses breadth search to find cycles in DDG, as this indicates cyclic dependencies
        void checkCyclicDependence(){

        }

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
                            REPORTS.push_back(JsonReporter::createSEM2(
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
                    if (conditionConstant.getType() != noneNode){
                        switch(conditionConstant.getType()){
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
                    if (conditionConstant.getType() != noneNode){
                        switch(conditionConstant.getType()){
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

            //TODO cyclic dependence

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
                SubVertex* mainVertex = new SubVertex("main", nullptr, subVF, 1, mainDeclaration->second.line, fileName, mainDeclaration->second.cfBlock, nullptr, {}, {}, mainDeclaration->second.declaredArgs);
                VERTICES.push_back(mainVertex);
                mainVertex->initializeVertex();
                std::cout << "Created a [MAIN] vertex: " << mainVertex << std::endl;
            } else {
                std::cout << "INTERNAL ERROR: No main found" << std::endl;
                return;
            }

            // 3. bind vertices to eachother
            bindVertices();

            auto graphBuildEnd = std::chrono::steady_clock::now();
            auto graphBuildEndSystem = std::chrono::system_clock::now();

            auto graphBuildTotal = std::chrono::duration_cast<ns>(graphBuildEnd - graphBuildStart).count();
            auto graphBuildTotalSystem = std::chrono::duration_cast<ns>(graphBuildEndSystem - graphBuildStartSystem).count();

            // printing out information does not count towards time to use and build graph
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

            // printing out information does not count towards time to find errors
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
