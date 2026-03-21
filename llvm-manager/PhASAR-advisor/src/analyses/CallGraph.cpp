#include "CallGraph.hpp"

#include <nlohmann/json.hpp>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/Demangle/Demangle.h>
#include <fstream>

#include "phasar/PhasarLLVM/TypeHierarchy/DIBasedTypeHierarchy.h"

using namespace llvm;

bool isRecursive(const Function* F, const vector<CallInfo>& calls) {
    string funcName = F->getName().str();
    for (const auto& call : calls) {
        if (call.caller == funcName && call.callee == funcName) {
            return true;
        }
    }
    return false;
}

string getFunctionName(const Value* V) {
    if (const Function* F = dyn_cast<Function>(V)) {
        return F->getName().str();
    }
    return "<indirect>";
}

bool isExternalFunction(const Function* F) {
    return F->isDeclaration() || F->empty();
}

static void runMyRealizedAnalysis(LLVMProjectIRDB& IRDB, const Options& opts) {
    if (!opts.useErrorCheckingAlgorithm) {
        cout << "\n=== АНАЛИЗ ГРАФА ВЫЗОВОВ ===\n";
    }
    
    vector<CallInfo> allCalls;
    CallGraphStats stats;
    unordered_set<string> processedFunctions;
    
    const auto* M = IRDB.getModule();
    
    if (!M) {
        cerr << "Error: не удалось получить модуль LLVM IR\n";
        return;
    }
    
    if (!opts.useErrorCheckingAlgorithm) {
        cout << "\nАнализ модуля: " << M->getName().str() << "\n";
    }

    for (const auto& F : *M) {
        if (F.isDeclaration()) {
            continue;
        }

        // Пропускаем приватные/системные функции
        std::string funcName = F.getName().str();
        if (opts.shouldSkipFunction(funcName)) {
            continue;
        }
        
        string callerName = F.getName().str();
        processedFunctions.insert(callerName);
        stats.totalFunctions++;
        
        bool hasCalls = false;
        bool functionHeaderPrinted = false;
        
        for (const auto& BB : F) {
            for (const auto& I : BB) {
                if (const CallInst* CI = dyn_cast<CallInst>(&I)) {
                    if (isa<IntrinsicInst>(CI)) {
                        continue;
                    }
                    
                    Function* calledFunc = CI->getCalledFunction();
                    
                    CallInfo callInfo;
                    callInfo.caller = callerName;
                    callInfo.isDirectCall = (calledFunc != nullptr);
                    
                    // Получаем строковое представление инструкции
                    string instStr;
                    raw_string_ostream rso(instStr);
                    I.print(rso);
                    callInfo.callSite = rso.str();
                    
                    if (calledFunc) {
                        // Прямой вызов
                        callInfo.callee = calledFunc->getName().str();
                        
                        // Пропускаем приватные/системные функции
                        if (opts.shouldSkipFunction(callInfo.callee)) {
                            continue;
                        }
                        
                        callInfo.isExternal = isExternalFunction(calledFunc);
                        
                        stats.directCalls++;
                        if (callInfo.isExternal) {
                            stats.externalCalls++;
                        }
                        
                        if (!functionHeaderPrinted && !opts.useErrorCheckingAlgorithm) {
                            cout << "\nФункция: " << callerName << "\n";
                            functionHeaderPrinted = true;
                        }
                        
                        if (!opts.useErrorCheckingAlgorithm) {
                            cout << "  → Прямой вызов: " << callInfo.callee;
                            if (callInfo.isExternal) {
                                cout << " (внешняя)";
                            }
                            cout << "\n";
                        }
                        
                        hasCalls = true;
                    } else {
                        // Косвенный вызов
                        Value* calledValue = CI->getCalledOperand();
                        callInfo.callee = getFunctionName(calledValue);
                        callInfo.isExternal = false;
                        stats.indirectCalls++;
                        
                        if (!functionHeaderPrinted && !opts.useErrorCheckingAlgorithm) {
                            cout << "\nФункция: " << callerName << "\n";
                            functionHeaderPrinted = true;
                        }
                        
                        if (!opts.useErrorCheckingAlgorithm) {
                            cout << "  → Косвенный вызов: " << callInfo.callee << "\n";
                        }
                        
                        hasCalls = true;
                    }
                    
                    allCalls.push_back(callInfo);
                    stats.totalCallSites++;
                }
            }
        }
    }
    
    for (const auto& F : *M) {
        if (!F.isDeclaration() && isRecursive(&F, allCalls)) {
            stats.recursiveFunctions++;
        }
    }
    
    if (!opts.useErrorCheckingAlgorithm) {
        cout << "\n";
        printCallGraphStats(stats);
        cout << "\n";
    }
    
    analyzeCallChains(allCalls, opts);

    if (!opts.useErrorCheckingAlgorithm) {
        string dotFilename = "callgraph.dot";
        exportCallGraphDot(allCalls, dotFilename);
        cout << "\nГраф вызовов экспортирован в файл: " << dotFilename << "\n";
        cout << "Для визуализации выполните: dot -Tpng " << dotFilename << " -o callgraph.png\n";
    }
}

static void runPhasarAnalysis(LLVMProjectIRDB& IRDB, const Options& opts) {
    cout << "\n=== АНАЛИЗ ГРАФА ВЫЗОВОВ С ИСПОЛЬЗОВАНИЕМ PhASAR ===\n";

    if (!IRDB.getFunctionDefinition("main")) {
        llvm::errs() << "Required function 'main' not found\n";
        return;
    }

    DIBasedTypeHierarchy TH(IRDB);
    LLVMBasedICFG ICFG(&IRDB, CallGraphAnalysisType::RTA, {"main"}, &TH);

    // const auto& CG = ICFG.getCallGraph();

    // // Проходим по всем местам вызовов
    // for (const auto* Call : CG.getAllVertexCallSites()) {
    //     if (Call->isDebugOrPseudoInst()) {
    //         // Пропускаем автогенерированные отладочные интринсики
    //         continue;
    //     }

    //     llvm::outs() << "Found call-site: " << psr::llvmIRToString(Call) << '\n';

    //     // Самая важная функция: getCalleesOfCallAt()
    //     for (const auto* CalleeFun : CG.getCalleesOfCallAt(Call)) {
    //         llvm::outs() << ">  calling "
    //                      << llvm::demangle(CalleeFun->getName().str()) << '\n';
    //     }
    //     llvm::outs() << '\n';
    // }

    // llvm::outs() << "--------------------------\n";

    // // Можно также идти в обратном направлении:
    // for (const auto* Fun : CG.getAllVertexFunctions()) {
    //     llvm::outs() << "Found Function: " << llvm::demangle(Fun->getName().str())
    //                  << '\n';

    //     // Вторая по важности функция: getCallersOf()
    //     for (const auto* CallSite : CG.getCallersOf(Fun)) {
    //         llvm::outs() << ">  called from " << psr::llvmIRToString(CallSite)
    //                      << '\n';
    //     }
    //     llvm::outs() << '\n';
    // }
    
    // Экспорт ICFG в DOT формат
    std::ofstream icfgDotFile("icfg.dot");
    if (!icfgDotFile.is_open()) {
        llvm::errs() << "Error: Could not create file icfg.dot\n";
        return;
    }

    std::string icfgDot = ICFG.exportICFGAsDot(false);
    icfgDotFile << icfgDot;
    icfgDotFile.close();

    cout << "\nICFG exported to icfg.dot\n";
    cout << "To visualize, run: dot -Tpng icfg.dot -o icfg.png\n";
}

void runCallGraphAnalysis(LLVMProjectIRDB& IRDB, const Options& opts) {
    if (opts.useErrorCheckingAlgorithm) {
        runMyRealizedAnalysis(IRDB, opts);
        return;
    }

    runMyRealizedAnalysis(IRDB, opts);
    runPhasarAnalysis(IRDB, opts);
}

void printCallGraphStats(const CallGraphStats& stats) {
    cout << "СТАТИСТИКА ГРАФА ВЫЗОВОВ:\n";
    cout << "Всего функций: " << stats.totalFunctions << "\n";
    cout << "Всего мест вызовов: " << stats.totalCallSites << "\n";
    cout << "Прямые вызовы: " << stats.directCalls << "\n";
    cout << "Вызовы внешних функций: " << stats.externalCalls << "\n";
    cout << "Рекурсивных функций: " << stats.recursiveFunctions << "\n";
}

void exportCallGraphDot(const vector<CallInfo>& calls, const string& filename) {
    ofstream dotFile(filename);
    
    if (!dotFile.is_open()) {
        cerr << "Error: не удалось создать файл " << filename << "\n";
        return;
    }
    
    dotFile << "digraph CallGraph {\n";
    dotFile << "  rankdir=LR;\n";
    dotFile << "  node [shape=box, style=rounded];\n\n";
    
    // Собираем все уникальные функции (без приватных)
    unordered_set<string> allFunctions;
    for (const auto& call : calls) {
        if (call.caller.empty() || call.caller[0] != '_') {
            allFunctions.insert(call.caller);
        }
        if (call.callee != "<indirect>" && !call.callee.empty() && call.callee[0] != '_') {
            allFunctions.insert(call.callee);
        }
    }
    
    // Находим недостижимые функции из main
    unordered_map<string, vector<string>> callGraph;
    for (const auto& call : calls) {
        if (call.callee != "<indirect>") {
            callGraph[call.caller].push_back(call.callee);
        }
    }
    
    unordered_set<string> reachable;
    vector<string> stack;
    
    if (allFunctions.find("main") != allFunctions.end()) {
        stack.push_back("main");
        reachable.insert("main");
        
        while (!stack.empty()) {
            string current = stack.back();
            stack.pop_back();
            
            for (const auto& callee : callGraph[current]) {
                if (reachable.find(callee) == reachable.end()) {
                    reachable.insert(callee);
                    stack.push_back(callee);
                }
            }
        }
    }
    
    // Определяем стили для разных типов узлов
    for (const auto& func : allFunctions) {
        dotFile << "  \"" << func << "\"";
        
        // Проверяем, является ли функция внешней
        bool isExternal = false;
        for (const auto& call : calls) {
            if (call.callee == func && call.isExternal) {
                isExternal = true;
                break;
            }
        }
        
        bool isReachable = reachable.find(func) != reachable.end();
        
        if (func == "main") {
            dotFile << " [fillcolor=lightgreen, style=\"rounded,filled\"]";
        } else if (!isReachable) {
            // Недостижимые функции выделяем красным с пунктирной границей
            dotFile << " [fillcolor=lightsalmon, style=\"rounded,filled,dashed\", color=red]";
        } else if (isExternal) {
            dotFile << " [fillcolor=lightgray, style=\"rounded,filled\"]";
        }
        
        dotFile << ";\n";
    }
    
    dotFile << "\n";
    
    // Добавляем рёбра (вызовы)
    unordered_map<string, int> edgeCount;
    
    for (const auto& call : calls) {
        if (call.callee == "<indirect>") {
            continue;
        }
        
        string edge = call.caller + " -> " + call.callee;
        edgeCount[edge]++;
    }
    
    for (const auto& [edge, count] : edgeCount) {
        size_t pos = edge.find(" -> ");
        string caller = edge.substr(0, pos);
        string callee = edge.substr(pos + 4);
        
        dotFile << "  \"" << caller << "\" -> \"" << callee << "\"";
        
        if (count > 1) {
            dotFile << " [label=\"" << count << "\"]";
        }
        
        // Проверяем на рекурсию
        if (caller == callee) {
            dotFile << " [color=red, style=bold]";
        }
        
        dotFile << ";\n";
    }
    
    dotFile << "}\n";
    dotFile.close();
}

void analyzeUnreachableFunctions(const vector<CallInfo>& calls, const unordered_set<string>& allFunctions, const Options& opts) {
    if (!opts.useErrorCheckingAlgorithm) {
        cout << "\n=== АНАЛИЗ НЕДОСТИЖИМЫХ ФУНКЦИЙ ===\n";
    }
    
    // Строим граф вызовов (без приватных функций)
    unordered_map<string, vector<string>> callGraph;
    for (const auto& call : calls) {
        if (call.callee != "<indirect>" && !call.callee.empty() && call.callee[0] != '_') {
            if (call.caller.empty() || call.caller[0] != '_') {
                callGraph[call.caller].push_back(call.callee);
            }
        }
    }
    
    // Выполняем обход в глубину от main
    unordered_set<string> reachable;
    vector<string> stack;
    
    if (allFunctions.find("main") != allFunctions.end()) {
        stack.push_back("main");
        reachable.insert("main");
        
        while (!stack.empty()) {
            string current = stack.back();
            stack.pop_back();
            
            for (const auto& callee : callGraph[current]) {
                if (reachable.find(callee) == reachable.end()) {
                    reachable.insert(callee);
                    stack.push_back(callee);
                }
            }
        }
    }
    
    // Находим недостижимые функции
    vector<string> unreachable;
    for (const auto& func : allFunctions) {
        if (reachable.find(func) == reachable.end()) {
            unreachable.push_back(func);
        }
    }
    
    if (!opts.useErrorCheckingAlgorithm) {
        cout << "\nДостижимых функций из main: " << reachable.size() << "\n";
        cout << "Недостижимых функций из main: " << unreachable.size() << "\n";
    }
    
    if (!unreachable.empty()) {
        if (!opts.useErrorCheckingAlgorithm) {
            cout << "\nСписок недостижимых функций:\n";
            for (const auto& func : unreachable) {
                cout << "  - " << func << "\n";
            }
        } else {
            reportUnreachableFunctions(unreachable, opts);
        }
    }
}

void reportUnreachableFunctions(const vector<string> &unreachable, const Options &opts) {
    if (unreachable.empty()) {
        return;
    }

    // Читаем JSON файл из helperFile
    if (opts.helperFile.empty()) {
        cerr << "Error: helperFile path is not specified\n";
        return;
    }

    ifstream helperFileStream(opts.helperFile);
    if (!helperFileStream.is_open()) {
        cerr << "Error: Could not open helper file '" << opts.helperFile << "'\n";
        return;
    }

    try {
        nlohmann::json helperJson;
        helperFileStream >> helperJson;
        helperFileStream.close();

        // Проверяем наличие ключа "functions"
        if (!helperJson.contains("functions")) {
            cerr << "Error: 'functions' key not found in helper file\n";
            return;
        }

        // Извлекаем массив функций
        if (!helperJson["functions"].is_array()) {
            cerr << "Error: 'functions' is not an array in helper file\n";
            return;
        }

        for (const auto& func : unreachable) {
            for (const auto& funcObj : helperJson["functions"]) {
                if (funcObj.is_object() && funcObj.contains("name")) {
                    if (funcObj["name"] == func) {
                        if (funcObj.contains("type") && funcObj["type"] == "extern") {
                            std::string errMsg = "Unreachable extern function: " + func;
                            ErrorBase error(ErrorType::SYN54);
                            error.setObjectDetail("cf", make_unique<cf_struct>(
                                funcObj["name"],
                                "extern",
                                funcObj.value("file", "unknown"),
                                funcObj.value("line", 0)
                            ));
                            ErrorHandler::printError(std::move(errMsg), std::move(error));
                        } else if (funcObj.contains("type") && funcObj["type"] == "struct") {
                            std::string errMsg = "Unreachable struct function: " + func;
                            ErrorBase error(ErrorType::SYN55);
                            error.setObjectDetail("cf", make_unique<cf_struct>(
                                funcObj["name"],
                                "struct",
                                funcObj.value("file", "unknown"),
                                funcObj.value("line", 0)
                            ));
                            ErrorHandler::printError(std::move(errMsg), std::move(error));
                        }

                        break;
                    }
                }
            }
        }
    } catch (const nlohmann::json::exception& e) {
        cerr << "Error parsing JSON from helper file: " << e.what() << "\n";
        return;
    }
}

void analyzeCallChains(const vector<CallInfo>& calls, const Options& opts) {
    if (!opts.useErrorCheckingAlgorithm) {
        cout << "=== АНАЛИЗ ЦЕПОЧЕК ВЫЗОВОВ ===\n";
    }
    
    unordered_map<string, vector<string>> callGraph;
    unordered_set<string> allFunctions;
    
    for (const auto& call : calls) {
        // Пропускаем приватные функции
        if (call.caller.empty() || call.caller[0] == '_') {
            continue;
        }
        
        allFunctions.insert(call.caller);
        if (call.callee != "<indirect>" && !call.callee.empty() && call.callee[0] != '_') {
            callGraph[call.caller].push_back(call.callee);
            allFunctions.insert(call.callee);
        }
    }
    
    if (!opts.useErrorCheckingAlgorithm) {
        cout << "\nФункции-листья (не вызывают другие функции):\n";
    }

    for (const auto& func : allFunctions) {
        if (callGraph[func].empty() && !opts.useErrorCheckingAlgorithm) {
            cout << "  - " << func << "\n";
        }
    }
    
    // Находим функции верхнего уровня (никем не вызываются)
    unordered_set<string> calledFunctions;
    for (const auto& call : calls) {
        if (call.callee != "<indirect>") {
            calledFunctions.insert(call.callee);
        }
    }
    
    if (!opts.useErrorCheckingAlgorithm) {
        cout << "\nФункции верхнего уровня (точки входа):\n";
    }
    
    for (const auto& func : allFunctions) {
        if (calledFunctions.find(func) == calledFunctions.end() && !opts.useErrorCheckingAlgorithm) {
            cout << "  - " << func << "\n";
        }
    }
    
    if (!opts.useErrorCheckingAlgorithm) {
        cout << "\nРекурсивные вызовы:\n";
    }

    bool foundRecursion = false;
    for (const auto& call : calls) {
        if (call.caller == call.callee) {
            if (!opts.useErrorCheckingAlgorithm) {
                cout << "  - " << call.caller << " (прямая рекурсия)\n";
            }

            foundRecursion = true;
        }
    }

    if (!foundRecursion && !opts.useErrorCheckingAlgorithm) {
        cout << "  Прямых рекурсивных вызовов не обнаружено.\n";
    }
    
    // Анализ недостижимых функций
    analyzeUnreachableFunctions(calls, allFunctions, opts);
}
