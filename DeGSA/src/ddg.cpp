#include "enums.hpp"
#include "vertices.cpp"
#include "ids.cpp"
#include "expr.cpp"
#include "parameter_usage.hpp"
#include "json_reporter.cpp"

#include <chrono>
#include <fstream>
#include <queue>

//todo apt-get install libz3-dev for this to work
#include <z3++.h>

using ns = std::chrono::nanoseconds;

class DDG {

public:

    ast* _astobj; // input AST
    std::string _fileName;
    std::ostream* _outputTarget;

    void declareName(std::set<std::string>& visible, const std::string& name,
                     const char* duplicateCode, int line, const std::string& subName) {
        if (!visible.insert(name).second) {
            JsonReporter::reportNameAt(duplicateCode, name, _fileName, line, subName);
        }
    }

    void checkBlockDeclarations(const block& body, const std::string& subName, std::set<std::string> visible) {
        if (auto dfs = body.opt_dfdecls_->dfdecls_) {
            for (auto name : *dfs->name_seq_->names_) visible.insert(name->to_string());
        }
        for (auto stmt : *body.statement_seq_->statements_) {
            // Only compound statements own a nested block.
            if (!stmt || dynamic_cast<cf_statement*>(stmt)) continue;
            auto nested = visible;
            if (auto loop = dynamic_cast<for_statement*>(stmt)) {
                const auto& name = *loop->name_->value_;
                declareName(nested, name, "SYN8.2", loop->line_, subName);
                if (!NameUsage(name).referencedIn(*loop)) {
                    JsonReporter::reportUnusedDeclaration("SYN5.7", name, _fileName, loop->line_, subName);
                }
            } else if (auto loop = dynamic_cast<while_statement*>(stmt)) {
                const auto& name = *loop->left_->value_;
                declareName(nested, name, "SYN8.3", loop->line_, subName);
                if (!NameUsage(name).referencedIn(*loop)) {
                    JsonReporter::reportUnusedDeclaration("SYN5.8", name, _fileName, loop->line_, subName);
                }
            } else if (auto let = dynamic_cast<let_statement*>(stmt)) {
                for (auto assignment : *let->assign_seq_->assign_seq_) {
                    declareName(nested, assignment->name_->to_string(), "SYN8.4", let->line_, subName);
                }
            }
            checkBlockDeclarations(*stmt->block_, subName, std::move(nested));
        }
    }

    void findSubs(){
        for (auto subDef: (*(_astobj->program_->sub_defs))){
            if (luna_sub_def* subDecl = dynamic_cast<luna_sub_def*>(subDef); subDecl != NULL) { // found a structured CF
                std::cout << "Non-import (structured sub) found: ";
                std::string subName = *(subDecl->code_id_->get_value());
                std::cout << subName << std::endl;

                std::set<std::string> parameters;

                // save args of this CF to use it in initializeVertex
                std::vector<DeclaredArg> declaredArgs = {};
                if (subDecl->params_->param_seq_ != nullptr){
                    for (auto arg : *(subDecl->params_->param_seq_->params_)) {
                        ValueType type;
                        std::string lunaType = *(arg->type_->value_);
                        if (lunaType == "int") {
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
                        const auto& parameterName = *arg->name_->value_;
                        declareName(parameters, parameterName, "SYN8.5", arg->line_, subName);
                        declaredArgs.push_back({parameterName, type});
                        if (!NameUsage(parameterName).referencedIn(*subDecl)) {
                            JsonReporter::reportUnusedDeclaration("SYN5.6", parameterName, _fileName, arg->line_, subName);
                        }
                    }
                }

                checkBlockDeclarations(*subDecl->block_, subName, std::move(parameters));

                auto subCFDecl = CFDECLARATIONS.find(subName);
                CFDeclaration* newDeclaration = new CFDeclaration(subName, subCF, declaredArgs, subDecl->block_, subDecl->line_, _fileName);
                if (subCFDecl != CFDECLARATIONS.end()) {
                    /* LuNA overwrites declarations with same names and uses the last one, so we overwrite previous one
                       and create an error report */
                    JsonReporter::createSYN6_2(subCFDecl->second, newDeclaration);
                    CFDECLARATIONS.erase(subName);
                }
                CFDECLARATIONS.insert( { subName, newDeclaration } );
            } else if (import* importDecl = dynamic_cast<import*>(subDef); importDecl != NULL) { // found an import
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
                CFDeclaration* newDeclaration = new CFDeclaration(importName, importCF, declaredArgs, nullptr, importDecl->line_, _fileName);
                if (importCFDecl != CFDECLARATIONS.end()) {
                    /* LuNA overwrites declarations with same names and uses the last one, so we overwrite previous one
                       and create an error report */
                    JsonReporter::createSYN6_1(importCFDecl->second, newDeclaration);
                    CFDECLARATIONS.erase(importName);
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

            // this map has information about what indexed/simple DFs there is and with how many indices
            // they are used and defined in what vertices
            // basically: map(int size, pair<vector use, vector def>);
            //std::map<int, std::pair<std::vector<Vertex*>*, std::vector<Vertex*>*>> _sizeToUseDefVectors = {};
            auto pData = baseName->getData();
            std::vector<int> aSizes = pData->getAllSizes();
            for (auto size : aSizes) {
                auto pUseVector = pData->getUseVectorByIndicesSize(size);
                auto pDefVector = pData->getDefVectorByIndicesSize(size);
                if (!pUseVector || !pDefVector)
                    continue;
                for (auto useVertexPair : *pUseVector) {
                    for (auto defVertexPair : *pDefVector) {
                        int pointerToIndex = useVertexPair.second;
                        int pointerFromIndex = defVertexPair.second;
                        defVertexPair.first->bindTo(useVertexPair.first, pointerToIndex, pointerFromIndex, baseName);//todo check if this works correctly
                    }
                }
            }
        }
        std::cout << "\nbindVertices finished\n" << std::endl;
    }

    // this function compiles all information related to DF usage by CFs
    // for each atomic CF it tells where exactly its input DFs were initialized
    void dumpAtomicCFDF()
    {
        *_outputTarget << "\n========================\nDF usage information dump\n========================\n";

        std::ofstream dfDump("DFDump.json");
        dfDump << JsonReporter::DFDcreateDFDump(VERTICES) << std::endl;

        /*std::vector<std::string> functionEntries = {};
        for (Vertex* pVertex : VERTICES) {
            if (!pVertex)
                continue;

            switch(pVertex->getVertexType()) {
            case VertexType::importVF: {
                ImportVertex* pImportVertex = static_cast<ImportVertex*>(pVertex);
                *_outputTarget << "\n***\nImport " << pImportVertex->getName() << " on line " << pImportVertex->getLine() << std::endl;
                int ind = -1;
                for (auto& declaredArg : pImportVertex->getDeclaration()->declaredArgs) {
                    ind++;
                    if (declaredArg.type == ValueType::nameType) {
                        *_outputTarget << "\nArgument " << ind << " (" << pImportVertex->getCallArgs()[ind]->getASTExpr()->to_string() << ") is initialized\n";
                        continue;
                    }

                    std::set<Identifier*> apIdentifiers = pImportVertex->getCallArgs()[ind]->getAllIdentifiers();
                    *_outputTarget << "\nArgument " << ind << " (" << pImportVertex->getCallArgs()[ind]->getASTExpr()->to_string() << ") is used, " << apIdentifiers.size() << " participants:\n";
                    for (Identifier* pIdentifier : apIdentifiers) {
                        *_outputTarget << pIdentifier->getIdentifierTypeAsString() << " " << pIdentifier->getName() << "\n";
                        if (pIdentifier->getClass() == IdentifierClass::indexedDFNameClass) {
                            IndexedDFName* pDFName = static_cast<IndexedDFName*>(pIdentifier);
                            *_outputTarget << "Amount of indices: " << pDFName->getExpressionsVector().size() << ", expressions inside:";
                            for (Expression* pExp : pDFName->getExpressionsVector()) {
                                *_outputTarget << "\n" << pExp->getASTExpr()->to_string();
                            }
                            *_outputTarget << "\n\n";
                        }
                    }
                }
                *_outputTarget << "***\n";
                break;
            }

            case VertexType::forVF: {
                ForVertex* pForVertex = static_cast<ForVertex*>(pVertex);
                *_outputTarget << "\n***\nFor operator on line " << pForVertex->getLine() << std::endl;

                std::set<Identifier*> apIdentifiersLeft = pForVertex->getLeftBorder()->getAllIdentifiers();
                *_outputTarget << "\nLeft border expression: " << pForVertex->getLeftBorder()->getASTExpr()->to_string() << ", participants:\n";
                for (Identifier* pIdentifier : apIdentifiersLeft) {
                    *_outputTarget << pIdentifier->getIdentifierTypeAsString() << " " << pIdentifier->getName() << "\n";
                    if (pIdentifier->getClass() == IdentifierClass::indexedDFNameClass) {
                        IndexedDFName* pDFName = static_cast<IndexedDFName*>(pIdentifier);
                        *_outputTarget << "Amount of indices: " << pDFName->getExpressionsVector().size() << ", expressions inside:";
                        for (Expression* pExp : pDFName->getExpressionsVector()) {
                            *_outputTarget << "\n" << pExp->getASTExpr()->to_string();
                        }
                        *_outputTarget << "\n\n";
                    }
                }

                std::set<Identifier*> apIdentifiersRight = pForVertex->getRightBorder()->getAllIdentifiers();
                *_outputTarget << "\nRight border expression: " << pForVertex->getRightBorder()->getASTExpr()->to_string() << ", participants:\n";
                for (Identifier* pIdentifier : apIdentifiersRight) {
                    *_outputTarget << pIdentifier->getIdentifierTypeAsString() << " " << pIdentifier->getName() << "\n";
                    if (pIdentifier->getClass() == IdentifierClass::indexedDFNameClass) {
                        IndexedDFName* pDFName = static_cast<IndexedDFName*>(pIdentifier);
                        *_outputTarget << "Amount of indices: " << pDFName->getExpressionsVector().size() << ", expressions inside:";
                        for (Expression* pExp : pDFName->getExpressionsVector()) {
                            *_outputTarget << "\n" << pExp->getASTExpr()->to_string();
                        }
                        *_outputTarget << "\n\n";
                    }
                }
                break;
            }

            case VertexType::whileVF: {
                WhileVertex* pWhileVertex = static_cast<WhileVertex*>(pVertex);
                *_outputTarget << "\n***\nWhile operator on line " << pWhileVertex->getLine() << std::endl;
                std::set<Identifier*> apIdentifiersCond = pWhileVertex->getConditionExpr()->getAllIdentifiers();
                *_outputTarget << "\nCondition expression: " << pWhileVertex->getConditionExpr()->getASTExpr()->to_string() << ", participants:\n";
                for (Identifier* pIdentifier : apIdentifiersCond) {
                    *_outputTarget << pIdentifier->getIdentifierTypeAsString() << " " << pIdentifier->getName() << "\n";
                    if (pIdentifier->getClass() == IdentifierClass::indexedDFNameClass) {
                        IndexedDFName* pDFName = static_cast<IndexedDFName*>(pIdentifier);
                        *_outputTarget << "Amount of indices: " << pDFName->getExpressionsVector().size() << ", expressions inside:";
                        for (Expression* pExp : pDFName->getExpressionsVector()) {
                            *_outputTarget << "\n" << pExp->getASTExpr()->to_string();
                        }
                        *_outputTarget << "\n\n";
                    }
                }

                std::set<Identifier*> apIdentifiersStart = pWhileVertex->getStartExpr()->getAllIdentifiers();
                *_outputTarget << "\nStart expression: " << pWhileVertex->getStartExpr()->getASTExpr()->to_string() << ", participants:\n";
                for (Identifier* pIdentifier : apIdentifiersStart) {
                    *_outputTarget << pIdentifier->getIdentifierTypeAsString() << " " << pIdentifier->getName() << "\n";
                    if (pIdentifier->getClass() == IdentifierClass::indexedDFNameClass) {
                        IndexedDFName* pDFName = static_cast<IndexedDFName*>(pIdentifier);
                        *_outputTarget << "Amount of indices: " << pDFName->getExpressionsVector().size() << ", expressions inside:";
                        for (Expression* pExp : pDFName->getExpressionsVector()) {
                            *_outputTarget << "\n" << pExp->getASTExpr()->to_string();
                        }
                        *_outputTarget << "\n\n";
                    }
                }

                break;
            }
            }
        }*/

        *_outputTarget << "\n========================\nDF usage information dump end\n========================\n";
    }


public:

    // Check identifiers declared inside graph vertices.
    void checkUnusedIdentifiers(){
        for (auto vertex: VERTICES){
            for (auto nameIdPair: vertex->getDeclaredInsideIdsMap()){
                Identifier* identifier = nameIdPair.second;
                if (auto baseName = dynamic_cast<BaseDFName*>(identifier)) {
                    if (baseName->getData()->empty()) {
                        JsonReporter::createSYN5_3(baseName);
                    }
                    continue;
                }
                if (auto alias = dynamic_cast<LetName*>(identifier)) {
                    if (!alias->isReferenced()) {
                        JsonReporter::createSYN5_2(alias);
                    }
                    continue;
                }
            }
        }
    }


    // SYN5.4, SYN5.5
    void checkUnusedCFs(){
        for (auto cfDeclaration : CFDECLARATIONS){
            if (!cfDeclaration.second->isUsed) {
                if (cfDeclaration.second->type == importCF)
                    JsonReporter::createSYN5_4(cfDeclaration.second);
                else
                    JsonReporter::createSYN5_5(cfDeclaration.second);
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
            JsonReporter::createSEM3_2(cycle.callstack);
        }
    }


    //todo rename codes
    // this function goes through BASENAMES and finds few types of errors:
    // 1. multiple DF initialization (03)
    // 2. using uninitialized DFs (05)
    // 3. unused DF (10)
    void checkBaseNameSet(){

        for (BaseDFName* bn: BASENAMES){
            auto baseNameData = bn->getData();
            for (auto baseNameSize : baseNameData->getAllSizes()){

                std::vector<std::pair<Vertex*, int>>* pUses = baseNameData->getUseVectorByIndicesSize(baseNameSize);
                std::vector<std::pair<Vertex*, int>>* pDefs = baseNameData->getDefVectorByIndicesSize(baseNameSize);

                if (baseNameSize == 0 && pDefs && pDefs->size() > 1) {
                    JsonReporter::createSEM2_1(bn, *pDefs);
                }

                if (!pUses || pUses->empty()) {
                    if (pDefs && !pDefs->empty()) {
                        JsonReporter::createSEM4(bn, baseNameSize);
                    }
                } else if (baseNameSize == 0 && (!pDefs || pDefs->empty())) {
                    for (const auto& use : *pUses) {
                        JsonReporter::createSEM3_1(bn, use.first);
                    }
                }

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
                                JsonReporter::createSEM5(
                                    false,
                                    ifVertex->getConditionExpr()->getASTExpr()->to_string(),
                                    _fileName,
                                    ifVertex->getLine(),
                                    "if"
                                );
                            } else {
                                JsonReporter::createSEM5(
                                    true,
                                    ifVertex->getConditionExpr()->getASTExpr()->to_string(),
                                    _fileName,
                                    ifVertex->getLine(),
                                    "if"
                                );
                            }
                            continue;
                        }
                        case intNode: {
                            std::string conditionConstantString = conditionConstant.getConstant();
                            JsonReporter::createSEM5(
                                std::stoi(conditionConstantString) != 0,
                                ifVertex->getConditionExpr()->getASTExpr()->to_string(),
                                _fileName,
                                ifVertex->getLine(),
                                "if"
                            );
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
                        case realNode: {
                            JsonReporter::createSEM5(
                                std::stod(conditionConstant.getConstant()) != 0,
                                whileVertex->getConditionExpr()->getASTExpr()->to_string(),
                                _fileName,
                                whileVertex->getLine(),
                                "while"
                            );
                            continue;
                        }
                        case intNode: {
                            std::string conditionConstantString = conditionConstant.getConstant();
                            JsonReporter::createSEM5(
                                std::stoi(conditionConstantString) != 0,
                                whileVertex->getConditionExpr()->getASTExpr()->to_string(),
                                _fileName,
                                whileVertex->getLine(),
                                "while"
                            );
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


    void writeReports() {
        const auto& reports = JsonReporter::REPORTS;
        if (reports.empty()) {
            *_outputTarget << "\nNo errors found\n" << std::endl;
        }
        for (const auto& report : reports) {
            *_outputTarget << report << std::endl;
        }

        const char* path = "./reporter/found_errors.json";
        std::ifstream input(path);
        if (input && reports.empty()) return;
        std::string existing((std::istreambuf_iterator<char>(input)), {});
        input.close();

        // Other ADAPT analyzers may already have written a JSON array.
        const auto last = existing.find_last_not_of(" \t\n\r");
        std::string output = JsonReporter::createArray(reports);
        if (last != std::string::npos && existing[last] == ']') {
            existing.resize(last);
            const auto previous = existing.find_last_not_of(" \t\n\r");
            if (previous != std::string::npos && existing[previous] != '[') {
                output = existing + "," + output.substr(1);
            }
        }
        std::ofstream(path) << output;
    }

    DDG(ast* astObjectIn, std::ostream* outputTarget, std::string fileName){
        auto graphBuildStart = std::chrono::steady_clock::now();
        auto graphBuildStartSystem = std::chrono::system_clock::now();

        _fileName = fileName;
        _outputTarget = outputTarget;
        _astobj = astObjectIn;

        std::cout << "\n============ Creating DDG ============" << std::endl;

        // 1. find use- and def- atomic CFs
        findSubs();

        // 2. create all the vertices
        auto mainDeclaration = CFDECLARATIONS.find("main");
        if (mainDeclaration != CFDECLARATIONS.end() && mainDeclaration->second->type == subCF) {
            mainDeclaration->second->isUsed = true;
            SubVertex* mainVertex = new SubVertex("main", nullptr, subVF, 1, mainDeclaration->second->line, fileName, mainDeclaration->second->cfBlock, nullptr, mainDeclaration->second, {}, {}, {}, mainDeclaration->second->declaredArgs);
            std::cout << "Created a [MAIN] vertex: " << mainVertex << std::endl;
            std::cout << "Initializing [MAIN] vertex" << std::endl;
            if (mainVertex->initializeVertex()) {
                VERTICES.push_back(mainVertex);
            } else {
                std::cout << "INTERNAL ERROR: not able to initialize [MAIN] vertex" << std::endl;
            }
        } else {
            JsonReporter::createSYN7();
            dumpAtomicCFDF();
            writeReports();
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
            auto baseNameData = bn->getData();
            for (auto baseNameSize : baseNameData->getAllSizes()){

                *outputTarget << "Size: " << baseNameSize << std::endl;

                std::vector<std::pair<Vertex*, int>>* pUses = baseNameData->getUseVectorByIndicesSize(baseNameSize);
                *outputTarget << "Uses: ";
                if (pUses)
                    for (auto u: *pUses){
                        *outputTarget << u.first << " at pos " << u.second << " ";
                    }
                *outputTarget << std::endl;

                std::vector<std::pair<Vertex*, int>>* pDefs = baseNameData->getDefVectorByIndicesSize(baseNameSize);
                *outputTarget << "Defs: ";
                if (pDefs)
                    for (auto d: *pDefs){
                        *outputTarget << d.first << " at pos " << d.second << " ";
                    }
                *outputTarget << std::endl;
            }
        }

        dumpAtomicCFDF();

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

        writeReports();

        *outputTarget << "\nTime to find errors: " << (double)errorsFindTotal / 1000000000 << " seconds" << std::endl;
        *outputTarget << "\nTime to find errors (system): " << (double)errorsFindTotalSystem / 1000000000 << " seconds" << std::endl;

        *outputTarget << "\nTime to build DDG: " << (double)graphBuildTotal / 1000000000 << " seconds" << std::endl;
        *outputTarget << "\nTime to build DDG (system): " << (double)graphBuildTotalSystem / 1000000000 << " seconds" << std::endl;

        //===================================
        //z3 proto
        z3::context ctx;
        z3::solver s(ctx);

        // Define integer variables
        z3::expr x = ctx.int_const("x");
        z3::expr y = ctx.int_const("y");

        // Add constraints: x > 10 AND y > 10 AND x + y == 42
        s.add(x > 10);
        s.add(y > 10);
        s.add(x + y == 42);

        *outputTarget << "Z3 testing:\n\n";
        // Check satisfiability
        if (s.check() == z3::sat) {
            z3::model m = s.get_model();
            *outputTarget << "Satisfiable!\n";
            *outputTarget << "x = " << m.eval(x) << "\n";
            *outputTarget << "y = " << m.eval(y) << "\n";
        } else {
            *outputTarget << "Unsatisfiable.\n";
        }
        //===================================


    }
};
