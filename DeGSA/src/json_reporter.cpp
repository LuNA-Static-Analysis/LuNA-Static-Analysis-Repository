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

                std::vector<std::string> conditionParticipants = {};
                for (auto pConditionIdentifier : pWhileVertex->getConditionExpr()->getAllIdentifiers())
                    conditionParticipants.push_back(DFDcreateParticipant(pConditionIdentifier));
                argEntries.push_back(DFDcreateArgEntry(
                    pWhileVertex->getConditionExpr()->getASTExpr()->to_string(),
                    conditionParticipants,
                    "df"
                ));

                std::vector<std::string> outParticipants = {};
                outParticipants.push_back(DFDcreateParticipant(pWhileVertex->getOutName()));
                argEntries.push_back(DFDcreateArgEntry(
                    pWhileVertex->getOutName()->getName(),
                    outParticipants,
                    "df"
                ));

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

    std::vector<std::string> initialized = {};
    if (auto pDataIdentifier = dynamic_cast<DataIdentifier*>(pIdentifier)) {
        for (auto pair : pDataIdentifier->getDefVector()) {
            std::map<std::string, std::string> initParams = {};
            initParams["callstack"] = createCallstackFromVertex(pair.first);
            initParams["index"] = pair.second;
            initialized.push_back(createJson(initParams));
        }
    }
    params["initialized"] = createArray(initialized);

    std::vector<std::string> used = {};
    if (auto pDataIdentifier = dynamic_cast<DataIdentifier*>(pIdentifier)) {
        for (auto pair : pDataIdentifier->getUseVector()) {
            std::map<std::string, std::string> useParams = {};
            useParams["callstack"] = createCallstackFromVertex(pair.first);
            useParams["index"] = pair.second;
            used.push_back(createJson(useParams));
        }
    }
    params["used"] = createArray(used);

    return createJson(params);
}

static std::string DFDcreateIndexCallstack(
    Vertex* const ipVertex,
    int iIndex
){
    std::map<std::string, std::string> params = {};
    params["callstack"] = createCallstackFromVertex(ipVertex);
    params["index"] = iIndex;
    return createJson(params);
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
    Vertex* const ipVertex
){
    if (ipVertex == nullptr)
        return "[]";
    
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
    map.insert( { "where", createCallStack({createCallStackEntry("todo", "0", "todo")}) } );//todo callstack is wrong
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

static std::string createIdentifierLazy(
    std::string identifierName,
    Vertex* vertex
){
    std::map<std::string, std::string> map = {};
    map.insert( { "name", identifierName } );
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

static void createSYN5_6(
    Identifier* const identifier
){
    if (identifier->getClass() != mutableArgNameClass && identifier->getClass() != immutableArgNameClass)
        std::cout << "INTERNAL ERROR: SYN5.6 got wrong type" << std::endl;
    std::map<std::string, std::string> map = {};
    map.insert( { "identifier", createIdentifier(identifier) } );
    createReport("SYN5.6", createJson(map));
}

static void createSYN5_7(
    Identifier* const identifier
){
    if (identifier->getClass() != forIteratorNameClass)
        std::cout << "INTERNAL ERROR: SYN5.7 got wrong type" << std::endl;
    std::map<std::string, std::string> map = {};
    map.insert( { "identifier", createIdentifier(identifier) } );
    createReport("SYN5.7", createJson(map));
}

static void createSYN5_8(
    Identifier* const identifier
){
    if (identifier->getClass() != whileIteratorNameClass)
        std::cout << "INTERNAL ERROR: SYN5.8 got wrong type" << std::endl;
    std::map<std::string, std::string> map = {};
    map.insert( { "identifier", createIdentifier(identifier) } );
    createReport("SYN5.8", createJson(map));
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

static void createSYN8_1(
    std::string identifierName,
    Vertex* vertex
){
    std::map<std::string, std::string> map = {};
    map.insert( { "identifier", createIdentifierLazy(identifierName, vertex) } );
    createReport("SYN8.1", createJson(map));
}

static void createSYN8_2(
    std::string identifierName,
    Vertex* vertex
){
    std::map<std::string, std::string> map = {};
    map.insert( { "identifier", createIdentifierLazy(identifierName, vertex) } );
    createReport("SYN8.2", createJson(map));
}

static void createSYN8_3(
    std::string identifierName,
    Vertex* vertex
){
    std::map<std::string, std::string> map = {};
    map.insert( { "identifier", createIdentifierLazy(identifierName, vertex) } );
    createReport("SYN8.3", createJson(map));
}

static void createSYN8_4(
    std::string identifierName,
    Vertex* vertex
){
    std::map<std::string, std::string> map = {};
    map.insert( { "identifier", createIdentifierLazy(identifierName, vertex) } );
    createReport("SYN8.4", createJson(map));
}

static void createSYN8_5(
    std::string identifierName,
    Vertex* vertex
){
    std::map<std::string, std::string> map = {};
    map.insert( { "identifier", createIdentifierLazy(identifierName, vertex) } );
    createReport("SYN8.5", createJson(map));
}

static void createSYN9(
    Identifier* const identifier
){
    std::map<std::string, std::string> map = {};
    map.insert( { "identifier", createIdentifier(identifier) } );
    createReport("SYN9", createJson(map));
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

static void createSEM2_1(
    Identifier* const identifier
){
    std::map<std::string, std::string> map = {};
    map.insert( { "initialized", createDFRef("todo", "todo", "todo", "todo") } );//todo
    map.insert( { "other_initializations", createArray( { } )});//todo
    createReport("SEM2.1", createJson(map));
}

// attempt to use uninitialized DF (outside of a loop TODO this is weird, but we'll see)
static void createSEM3_1(
    Identifier* const identifier
){
    std::map<std::string, std::string> map = {};
    map.insert( { "df", createDF(identifier) } );
    createReport("SEM3.1", createJson(map));
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

static void createSEM4(
    Identifier* const identifier
){
    std::map<std::string, std::string> map = {};
    map.insert( { "df", createDF(identifier) } );
    createReport("SEM4", createJson(map));
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

static void createSEM8(
    Vertex* const firstCaller,
    Vertex* const secondCaller
){
    //NOT SYNCHRONIZED TODO
    std::map<std::string, std::string> map = {};
    map.insert( { "first", createCallStackEntry(firstCaller->getFileName(), std::to_string(firstCaller->getLine()), firstCaller->getName()) } );
    map.insert( { "second", createCallStackEntry(secondCaller->getFileName(), std::to_string(secondCaller->getLine()), secondCaller->getName()) } );
    createReport("SEM8", createJson(map));
}

static inline std::vector<std::string> REPORTS {};
    
private:

JsonReporter(){}

};
