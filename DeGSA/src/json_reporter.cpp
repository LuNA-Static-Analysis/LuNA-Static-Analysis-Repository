#pragma once

#include "enums.hpp"
#include "vertices.hpp"

#include <vector>
#include <map>
#include <string>
#include <format>

class JsonReporter {

public:

static std::string DFDcreateDFDump(std::vector<Vertex*> apVertices)//todo differentiate variables -- add address
{
    std::vector<std::string> functionEntries = {};

    for (Vertex* pVertex : apVertices) {
        if (!pVertex)
            continue;

        switch(pVertex->getVertexType()) {
            case VertexType::importVF: {
                ImportVertex* pImportVertex = static_cast<ImportVertex*>(pVertex);
                
                std::vector<std::string> argEntries = {};
                int ind = -1;
                for (auto& declaredArg : pImportVertex->getDeclaration()->declaredArgs) {
                    ind++;
                    
                    std::vector<std::string> participants = {};
                    for (auto pIdentifier : pImportVertex->getCallArgs()[ind]->getAllIdentifiers())
                        participants.push_back(DFDcreateParticipant(pIdentifier));

                    std::string argType = declaredArg.type == ValueType::nameType ? "initialized" : "used";

                    argEntries.push_back(DFDcreateArgEntry(
                        pImportVertex->getCallArgs()[ind]->getASTExpr()->to_string(),
                        participants,
                        argType
                    ));
                }

                functionEntries.push_back(DFDcreateFunctionEntry(pVertex, argEntries));

                break;
            }

            case VertexType::forVF: {
                ForVertex* pForVertex = static_cast<ForVertex*>(pVertex);
                
                std::vector<std::string> argEntries = {};

                std::vector<std::string> iteratorParticipants = {};
                iteratorParticipants.push_back(DFDcreateParticipant(pForVertex->getIterator()));
                argEntries.push_back(DFDcreateArgEntry(
                    pForVertex->getIterator()->getName(),
                    iteratorParticipants,
                    "for_iterator"
                ));

                std::vector<std::string> leftBorderParticipants = {};
                for (auto pLeftBorderIdentifier : pForVertex->getLeftBorder()->getAllIdentifiers())
                    leftBorderParticipants.push_back(DFDcreateParticipant(pLeftBorderIdentifier));
                argEntries.push_back(DFDcreateArgEntry(
                    pForVertex->getLeftBorder()->getASTExpr()->to_string(),
                    leftBorderParticipants,
                    "df"
                ));

                std::vector<std::string> rightBorderParticipants = {};
                for (auto pRightBorderIdentifier : pForVertex->getRightBorder()->getAllIdentifiers())
                    rightBorderParticipants.push_back(DFDcreateParticipant(pRightBorderIdentifier));
                argEntries.push_back(DFDcreateArgEntry(
                    pForVertex->getRightBorder()->getASTExpr()->to_string(),
                    rightBorderParticipants,
                    "df"
                ));

                functionEntries.push_back(DFDcreateFunctionEntry(pVertex, argEntries));

                break;
            }

            case VertexType::whileVF: {
                WhileVertex* pWhileVertex = static_cast<WhileVertex*>(pVertex);

                std::vector<std::string> argEntries = {};

                std::vector<std::string> conditionParticipants = {};
                for (auto pConditionIdentifier : pWhileVertex->getConditionExpr()->getAllIdentifiers())
                    conditionParticipants.push_back(DFDcreateParticipant(pConditionIdentifier));
                argEntries.push_back(DFDcreateArgEntry(
                    pWhileVertex->getConditionExpr()->getASTExpr()->to_string(),
                    conditionParticipants,
                    "df"
                ));

                std::vector<std::string> iteratorParticipants = {};
                iteratorParticipants.push_back(DFDcreateParticipant(pWhileVertex->getIterator()));
                argEntries.push_back(DFDcreateArgEntry(
                    pWhileVertex->getIterator()->getName(),
                    iteratorParticipants,
                    "while_iterator"
                ));

                std::vector<std::string> startExprParticipants = {};
                for (auto pStartExprIdentifier : pWhileVertex->getStartExpr()->getAllIdentifiers())
                    startExprParticipants.push_back(DFDcreateParticipant(pStartExprIdentifier));
                argEntries.push_back(DFDcreateArgEntry(
                    pWhileVertex->getStartExpr()->getASTExpr()->to_string(),
                    startExprParticipants,
                    "df"
                ));

                if (auto out = pWhileVertex->getOutName()) {
                    std::vector<std::string> outParticipants;
                    for (auto identifier : out->getAllIdentifiers())
                        outParticipants.push_back(DFDcreateParticipant(identifier));
                    argEntries.push_back(DFDcreateArgEntry(out->getName(), outParticipants, "df"));
                } else {
                    argEntries.push_back(DFDcreateArgEntry("", {}, "df"));
                }

                functionEntries.push_back(DFDcreateFunctionEntry(pVertex, argEntries));

                break;
            }

            case VertexType::ifVF: {
                IfVertex* pIfVertex = static_cast<IfVertex*>(pVertex);

                std::vector<std::string> argEntries = {};

                std::vector<std::string> conditionParticipants = {};
                for (auto pConditionIdentifier : pIfVertex->getConditionExpr()->getAllIdentifiers())
                    conditionParticipants.push_back(DFDcreateParticipant(pConditionIdentifier));
                argEntries.push_back(DFDcreateArgEntry(
                    pIfVertex->getConditionExpr()->getASTExpr()->to_string(),
                    conditionParticipants,
                    "df"
                ));

                functionEntries.push_back(DFDcreateFunctionEntry(pVertex, argEntries));

                break;
            }
        }
    }

    return createArray(functionEntries);
}

static std::string DFDcreateFunctionEntry(
    Vertex* pVertex,
    std::vector<std::string> argEntries
)
{
    std::map<std::string, std::string> params = {};

    params["cf"] = pVertex->getName() + "@" + std::format("{}", static_cast<const void*>(pVertex));

    params["args"] = createArray(argEntries);

    params["callstack"] = createCallstackFromVertex(pVertex);

    return createJson(params);
}

static std::string DFDcreateArgEntry(
    std::string argExpression,
    std::vector<std::string> participants,
    std::string argType
){
    std::map<std::string, std::string> params = {};

    params["arg_expression"] = argExpression;

    params["participants"] = createArray(participants);

    params["arg_type"] = argType;

    return createJson(params);
}

static std::string DFDcreateParticipant(
    Identifier* pIdentifier
){
    std::map<std::string, std::string> params = {};

    params["name"] = pIdentifier->getName() + "@" + std::format("{}", static_cast<const void*>(pIdentifier));//todo how does it work with indexed dfs?
    
    switch(pIdentifier->getClass()) {
        case IdentifierClass::indexedDFNameClass:{
            params["identifier_type"] = "df";
            break;
        }
        case IdentifierClass::whileIteratorNameClass:{
            params["identifier_type"] = "while_iterator";
            break;
        }
        case IdentifierClass::forIteratorNameClass:{
            params["identifier_type"] = "for_iterator";
            break;
        }
        default:{
            params["identifier_type"] = pIdentifier->getIdentifierTypeAsString();
            break;
        }
    }

    std::vector<std::string> initialized;
    std::vector<std::string> used;
    auto appendAccesses = [](auto& entries, const auto& accesses) {
        for (const auto& [vertex, position] : accesses) {
            entries.push_back(DFDcreateIndexCallstack(vertex, position));
        }
    };

    // The graph groups DF accesses by base declaration and index depth.
    int indexCount = 0;
    if (auto base = DFDresolveBase(pIdentifier, indexCount)) {
        const auto data = base->getData();
        if (auto defs = data->getDefVectorByIndicesSize(indexCount)) {
            appendAccesses(initialized, *defs);
        }
        if (auto uses = data->getUseVectorByIndicesSize(indexCount)) {
            appendAccesses(used, *uses);
        }
    } else if (auto data = dynamic_cast<DataIdentifier*>(pIdentifier)) {
        appendAccesses(initialized, data->getDefVector());
        appendAccesses(used, data->getUseVector());
    }

    params["initialized"] = createArray(initialized);
    params["used"] = createArray(used);
    return createJson(params);
}

static BaseDFName* DFDresolveBase(Identifier* identifier, int& indexCount) {
    if (auto indexed = dynamic_cast<IndexedDFName*>(identifier)) {
        indexCount += indexed->getExpressionsVector().size();
        return DFDresolveBase(indexed->getBase(), indexCount);
    }
    if (identifier && identifier->getReference()) {
        return DFDresolveBase(identifier->getReference()->getAsIdentifier(), indexCount);
    }
    return dynamic_cast<BaseDFName*>(identifier);
}

static std::string DFDcreateIndexCallstack(Vertex* vertex, int position) {
    return "{\"callstack\": " + createCallstackFromVertex(vertex)
         + ", \"index\": " + std::to_string(position) + "}";
}

//=====================

static std::string createJson(
    std::map<std::string, std::string> jsonObjects
){
    if (jsonObjects.size() == 0)
        return "{}";
    std::string result = "{";
    for (auto l: jsonObjects){
        if (l.second[0] == '{' || l.second[0] == '['){ // complex object
            result += 
                "\"" + l.first + "\": " + l.second + ",";
        } else { // simple object
            result += 
                "\"" + l.first + "\": \"" + l.second + "\",";
        }  
    }
    return result.substr(0, result.size() - 1) + "}";
}

static std::string createArray(
    std::vector<std::string> jsonObjects
){
    std::string result = "[";
    for (auto jo: jsonObjects){
        result += (jo + ",");
    }
    if (jsonObjects.size() != 0)
        result[result.size() - 1] = ']';
    else
        result += "]";
    return result;
}

static void createReport(std::string lunaCode, std::string details){
    std::map<std::string, std::string> map = {};
    map.insert( { "error_code", lunaCode } );
    map.insert( { "details", details } );
    REPORTS.push_back(createJson(map));
}

static std::string createCallStackEntry(
    std::string fileName,
    std::string line,
    std::string cfName
)
{
    std::map<std::string, std::string> map = {};
    map.insert( { "file", fileName } );
    map.insert( { "line", line } );
    map.insert( { "name", cfName } );
    return createJson(map);
}

static std::string createCallStack(
        std::vector<std::string> callStackEntries
){
    return createArray(callStackEntries);
}

static std::string createIndexCallstackFromVertex(
    Vertex* const ipVertex,
    int iIndex,
    std::string iArrayName // "initialized" or "used"
){
    if (ipVertex == nullptr)
        return "[]";
    
    std::map<std::string, std::string> map = {};
    map.insert( { "arg_index", std::to_string(iIndex) } );

    std::vector<std::string> callstackEntries = {};
    Vertex* vertex = ipVertex;
    while(vertex != nullptr){
        callstackEntries.push_back(JsonReporter::createCallStackEntry(
            vertex->getFileName(),
            std::to_string(vertex->getLine()),
            vertex->getName()
        ));
        vertex = vertex->getParent();
    }
    map.insert({ iArrayName, createArray(callstackEntries) });
    return createJson(map);
}

static std::string createCallstackFromVertex(
    Vertex* const ipVertex, int firstLine = -1
){
    if (ipVertex == nullptr)
        return "[]";
    
    std::vector<std::string> callstackEntries = {};
    Vertex* vertex = ipVertex;
    while(vertex != nullptr){
        callstackEntries.push_back(JsonReporter::createCallStackEntry(
            vertex->getFileName(),
            std::to_string(vertex == ipVertex && firstLine >= 0 ? firstLine : vertex->getLine()),
            vertex->getName()
        ));
        vertex = vertex->getParent();
    }
    return JsonReporter::createCallStack(callstackEntries);
}

static std::string createDF(
    Identifier* const identifier
){
    std::map<std::string, std::string> map = {};
    map.insert( { "name", identifier->getName() } );
    // decl
    std::vector<std::string> declCallstack = {};
    declCallstack.push_back(createCallstackFromVertex(identifier->getVertex()));
    // def
    std::vector<std::string> defCallstack = {};
    if (auto pDataIdentifier = dynamic_cast<DataIdentifier*>(identifier)) {
        for (auto def: pDataIdentifier->getDefVector()){
            defCallstack.push_back(createCallstackFromVertex(def.first));
        }
    }
    // use
    std::vector<std::string> useCallstack = {};
    if (auto pDataIdentifier = dynamic_cast<DataIdentifier*>(identifier)) {
        for (auto use: pDataIdentifier->getUseVector()){
            useCallstack.push_back(createCallstackFromVertex(use.first));
        }
    }

    map.insert( { "declared", createArray(declCallstack) } );
    map.insert( { "initialized", createArray(defCallstack) } );
    map.insert( { "used", createArray(useCallstack) } );
    return createJson(map);
}

static std::string createDFRef(
        std::string df, // df as json
        std::string localExpression, // local expression
        std::string trueExpression, // real expression
        std::string callStack // callstack as json
){
    std::map<std::string, std::string> map = {};
    map.insert( { "df", df } );
    map.insert( { "local", localExpression } );
    map.insert( { "true", trueExpression } );
    map.insert( { "where", callStack } );
    map.insert( { "conditions", createArray({}) } );//todo
    return createJson(map);
}

static std::string createIndexRange(
        std::string dfRef,
        std::string forLoop, // "for" that creates indices range
        std::string step, // "for" step; expression as json
        std::string offset // "for" offset; expression as json
){
    std::map<std::string, std::string> map = {};
    map.insert( { "df_ref", dfRef } );
    map.insert( { "loop", forLoop } );
    map.insert( { "step", step } );
    map.insert( { "offset", offset } );
    return createJson(map);
}

static std::string createCF(
    CFDeclaration* const cfDeclaration
){
    std::map<std::string, std::string> map = {};
    if (cfDeclaration == nullptr) {
        map.insert( { "name", "INTERNAL_ERROR" } );
        map.insert( { "type", "INTERNAL_ERROR" } );
        map.insert( { "file", "INTERNAL_ERROR" } );
        map.insert( { "line", "INTERNAL_ERROR" } );
    } else {
        map.insert( { "name", cfDeclaration->name } );
        map.insert( { "type", cfDeclaration->type == subCF ? "struct" : "extern" } );
        map.insert( { "file", cfDeclaration->fileName } );
        map.insert( { "line", std::to_string(cfDeclaration->line) } );
    }
    return createJson(map);
}

static std::string createFor(
        std::string iteratorName,
        std::string startExpression,
        std::string endExpression,
        std::string callStack
){
    std::map<std::string, std::string> map = {};
    map.insert( { "var", iteratorName } );
    map.insert( { "first", startExpression } );
    map.insert( { "last", endExpression } );
    map.insert( { "where", callStack } );
    return createJson(map);
}

static std::string createIdentifier(
    Identifier* identifier
){
    std::map<std::string, std::string> map = {};
    map.insert( { "name", identifier->getName() } );
    auto vertex = identifier->getVertex();
    map.insert( { "call_stack_entry", createCallStackEntry(vertex->getFileName(), std::to_string(vertex->getLine()), vertex->getName()) } );
    return createJson(map);
}

//================================= ERROR REPORTS =============================================

// ******************************** SYNTAX **************************************************

static void createSYN1(
    std::string expression,
    Vertex* const vertex,
    CFDeclaration* const cfDeclaration
){
    std::map<std::string, std::string> map = {};
    std::replace( expression.begin(), expression.end(), '\"', '\'');
    map.insert( { "expression", expression } );
    std::string cfType = vertex->getVertexType() == importVF ? "extern" : "struct";
    map.insert( { "cf", createCF(cfDeclaration) } );
    map.insert( { "call_stack_entry", createCallStackEntry(vertex->getFileName(), std::to_string(vertex->getLine()), vertex->getName()) } );
    createReport("SYN1", createJson(map));
}

static void createSYN2(
    std::string fileName,
    int line,
    std::string cfName
){
    std::map<std::string, std::string> map = {};
    map.insert( { "call_stack_entry", createCallStackEntry(fileName, std::to_string(line), cfName) } );
    createReport("SYN2", createJson(map));
}

static void createSYN3(
    Vertex* const vertex,
    CFDeclaration* const cfDeclaration
){
    std::map<std::string, std::string> map = {};
    std::string cfType = vertex->getVertexType() == importVF ? "extern" : "struct";
    map.insert( { "cf", createCF(cfDeclaration) } );
    map.insert( { "call_stack_entry", createCallStackEntry(vertex->getFileName(), std::to_string(vertex->getLine()), vertex->getName()) } );
    createReport("SYN3", createJson(map));
}

static void createSYN5_2(
    Identifier* const identifier
){
    if (identifier->getClass() != letNameClass)
        std::cout << "INTERNAL ERROR: SYN5.2 got wrong type" << std::endl;
    std::map<std::string, std::string> map = {};
    map.insert( { "identifier", createIdentifier(identifier) } );
    createReport("SYN5.2", createJson(map));
}

static void createSYN5_3(
    Identifier* const identifier
){
    if (identifier->getClass() != baseDFNameClass)
        std::cout << "INTERNAL ERROR: SYN5.3 got wrong type" << std::endl;
    std::map<std::string, std::string> map = {};
    map.insert( { "identifier", createIdentifier(identifier) } );
    createReport("SYN5.3", createJson(map));
}

static void createSYN5_4(
    CFDeclaration* const cfDeclaration
){
    if (cfDeclaration->type != importCF)
        std::cout << "INTERNAL ERROR: SYN5.4 got wrong type" << std::endl;
    std::map<std::string, std::string> map = {};
    map.insert( { "cf", createCF(cfDeclaration) } );
    createReport("SYN5.4", createJson(map));
}

static void createSYN5_5(
    CFDeclaration* const cfDeclaration
){
    if (cfDeclaration->type != subCF)
        std::cout << "INTERNAL ERROR: SYN5.5 got wrong type" << std::endl;
    std::map<std::string, std::string> map = {};
    map.insert( { "cf", createCF(cfDeclaration) } );
    createReport("SYN5.5", createJson(map));
}

static void reportUnusedDeclaration(const std::string& code, const std::string& name,
                                    const std::string& file, int line, const std::string& sub) {
    auto declaration = createCallStackEntry(file, std::to_string(line), sub);
    auto identifier = createJson({{"name", name}, {"call_stack_entry", declaration}});
    createReport(code, createJson({{"identifier", identifier}}));
}

static void createSYN6_1(
    CFDeclaration* const cfDeclarationOrigin,
    CFDeclaration* const cfDeclarationDuplicate
){
    if (cfDeclarationOrigin->type != importCF)
        std::cout << "INTERNAL ERROR: SYN6.1 got wrong type" << std::endl;
    std::map<std::string, std::string> map = {};
    map.insert( { "cfs", createArray({ createCF(cfDeclarationOrigin), createCF(cfDeclarationDuplicate) }) } );
    createReport("SYN6.1", createJson(map));
}

static void createSYN6_2(
    CFDeclaration* const cfDeclarationOrigin,
    CFDeclaration* const cfDeclarationDuplicate
){
    if (cfDeclarationOrigin->type != subCF)
        std::cout << "INTERNAL ERROR: SYN6.2 got wrong type" << std::endl;
    std::map<std::string, std::string> map = {};
    map.insert( { "cfs", createArray({ createCF(cfDeclarationOrigin), createCF(cfDeclarationDuplicate) }) } );
    createReport("SYN6.2", createJson(map));
}

static void createSYN7(){
    std::map<std::string, std::string> map = {};
    createReport("SYN7", createJson(map));
}

static void reportNameAt(const std::string& code, const std::string& name,
                                const std::string& file, int line, const std::string& scope) {
    auto declared = createCallStackEntry(file, std::to_string(line), scope);
    auto identifier = createJson({{"name", name}, {"declared", declared}});
    createReport(code, createJson({{"identifier", identifier}}));
}

static void createSYN11(
    std::string expression,
    Vertex* const vertex
){
    std::map<std::string, std::string> map = {};
    map.insert( { "expression", expression } );
    std::replace( expression.begin(), expression.end(), '\"', '\'');
    map.insert( { "call_stack", createCallstackFromVertex(vertex) } );
    createReport("SYN11", createJson(map));
}

// ***************************************** SEMANTIC **************************************************

static std::string scalarDFDeclaration(BaseDFName* baseName) {
    const auto declaration = createCallstackFromVertex(baseName->getVertex(), baseName->getLine());
    return createJson({{"name", baseName->getName()}, {"declared", createArray({declaration})}});
}

static void createSEM2_1(BaseDFName* baseName, const std::vector<std::pair<Vertex*, int>>& definitions) {
    const auto df = scalarDFDeclaration(baseName);
    const auto name = baseName->getName();
    std::vector<std::string> sites;
    for (const auto& [vertex, position] : definitions) {
        auto local = name;
        if (auto call = dynamic_cast<ImportVertex*>(vertex)) {
            local = call->getCallArgs().at(position)->getASTExpr()->to_string();
        } else if (auto loop = dynamic_cast<WhileVertex*>(vertex)) {
            local = loop->getOutName()->getName();
        }
        sites.push_back(createDFRef(df, local, name, createCallstackFromVertex(vertex)));
    }
    createReport("SEM2.1", createJson({{"initialized", sites.front()},
                                       {"other_initializations", createArray({sites.begin() + 1, sites.end()})}}));
}

static void createSEM3_1(BaseDFName* baseName, Vertex* use){
    const auto df = scalarDFDeclaration(baseName);
    const auto name = baseName->getName();
    const auto used = createDFRef(df, name, name, createCallstackFromVertex(use));
    createReport("SEM3.1", createJson({{"used", used}, {"initialized", createArray({})}}));
}

static void createSEM3_2(
    std::vector<Vertex*> loop
){
    std::map<std::string, std::string> map = {};
    for (int i = 0; i < loop.size(); i++) {
        map.insert( { "vertex" + std::to_string(i + 1), createCallStackEntry(loop[i]->getFileName(), std::to_string(loop[i]->getLine()), loop[i]->getName()) } );
    }
    createReport("SEM3.2", createJson(map));
}

static void createSEM4(BaseDFName* baseName, int indexCount) {
    auto name = baseName->getName();
    for (int i = 0; i < indexCount; ++i) name += "[*]";
    auto scope = baseName->getVertex();
    reportNameAt("SEM4", name, scope->getFileName(), baseName->getLine(), scope->getName());
}

static void createSEM5(
    bool type,
    std::string condition,
    std::string fileName,
    int line,
    std::string cfName
){
    std::map<std::string, std::string> map = {};
    if (type)
        map.insert( { "type", "true" } );
    else
        map.insert( { "type", "false" } );
    map.insert( { "condition", condition } );
    map.insert( { "where", createCallStackEntry(fileName, std::to_string(line), cfName) } );
    createReport("SEM5", createJson(map));
}

static void createSEM8(Vertex* recursiveCall) {
    createReport("SEM8", createJson({{"where", createCallstackFromVertex(recursiveCall)}}));
}

static inline std::vector<std::string> REPORTS {};
    
private:

JsonReporter(){}

};
