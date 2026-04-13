#include "CallGraph.hpp"

#include <nlohmann/json.hpp>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/Demangle/Demangle.h>
#include <fstream>
#include <cstdlib>
#include <string>

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

static void runMyRealizedAnalysis(LLVMProjectIRDB& IRDB, const Options& opts, HTMLReporter& htmlReporter) {
    std::stringstream htmlOut;
    std::ostream& out = (opts.outputFormat == OutputFormat::HTML) ? static_cast<std::ostream&>(htmlOut) : static_cast<std::ostream&>(std::cout);

    if (!opts.useErrorCheckingAlgorithm) {
        out << "\n=== АНАЛИЗ ГРАФА ВЫЗОВОВ ===\n";
    }
    
    vector<CallInfo> allCalls;
    CallGraphStats stats;
    unordered_set<string> processedFunctions;
    
    const auto* M = IRDB.getModule();
    
    if (!M) {
        cerr << "Error: не удалось получить модуль LLVM IR\n";
        if (opts.outputFormat == OutputFormat::HTML) {
            htmlReporter.addError("Не удалось получить модуль LLVM IR (анализ графа вызовов)");
        }
        return;
    }
    
    if (!opts.useErrorCheckingAlgorithm) {
        out << "\nАнализ модуля: " << M->getName().str() << "\n";
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
                            out << "\nФункция: " << callerName << "\n";
                            functionHeaderPrinted = true;
                        }
                        
                        if (!opts.useErrorCheckingAlgorithm) {
                            out << "  → Прямой вызов: " << callInfo.callee;
                            if (callInfo.isExternal) {
                                out << " (внешняя)";
                            }
                            out << "\n";
                        }
                        
                        hasCalls = true;
                    } else {
                        // Косвенный вызов
                        Value* calledValue = CI->getCalledOperand();
                        callInfo.callee = getFunctionName(calledValue);
                        callInfo.isExternal = false;
                        stats.indirectCalls++;
                        
                        if (!functionHeaderPrinted && !opts.useErrorCheckingAlgorithm) {
                            out << "\nФункция: " << callerName << "\n";
                            functionHeaderPrinted = true;
                        }
                        
                        if (!opts.useErrorCheckingAlgorithm) {
                            out << "  → Косвенный вызов: " << callInfo.callee << "\n";
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
        out << "\n";
        printCallGraphStats(stats, out);
        out << "\n";
    }
    
    analyzeCallChains(allCalls, opts, out);

    string dotFilename = opts.artifactsDir + "/callgraph.dot";
    string pngFilename = opts.artifactsDir + "/callgraph.png";
    string legendDotFilename = opts.artifactsDir + "/callgraph_legend.dot";
    string legendPngFilename = opts.artifactsDir + "/callgraph_legend.png";

    if (!opts.useErrorCheckingAlgorithm) {
        exportCallGraphDot(allCalls, dotFilename, legendDotFilename);
        out << "\nГраф вызовов экспортирован в файл: " << dotFilename << "\n";
        
        // Пытаемся сгенерировать PNG через Graphviz (dot)
        int result = std::system(("dot -Tpng " + dotFilename + " -o " + pngFilename + " 2>/dev/null").c_str());
        int legendResult = std::system(("dot -Tpng " + legendDotFilename + " -o " + legendPngFilename + " 2>/dev/null").c_str());
        
        if (result == 0) {
            out << "Изображение графа успешно сгенерировано: " << pngFilename << "\n";
        } else {
            out << "Для визуализации выполните: dot -Tpng " << dotFilename << " -o " << pngFilename << "\n";
        }

        if (legendResult == 0) {
            out << "Изображение легенды успешно сгенерировано: " << legendPngFilename << "\n";
        }

        if (opts.outputFormat == OutputFormat::HTML) {
            htmlReporter.addSection("Граф Вызовов (CallGraph) (Basic)", "<pre>" + htmlOut.str() + "</pre>");
            htmlReporter.addImage("CallGraph Image", "callgraph.png");
            htmlReporter.addImage("CallGraph Legend", "callgraph_legend.png");
        }
    }
}

static void runPhasarAnalysis(LLVMProjectIRDB& IRDB, const Options& opts, HTMLReporter& htmlReporter) {
    std::stringstream htmlOut;
    std::ostream& out = (opts.outputFormat == OutputFormat::HTML) ? static_cast<std::ostream&>(htmlOut) : static_cast<std::ostream&>(std::cout);

    out << "\n=== АНАЛИЗ ГРАФА ВЫЗОВОВ С ИСПОЛЬЗОВАНИЕМ PhASAR ===\n";

    if (!IRDB.getFunctionDefinition("main")) {
        llvm::errs() << "Required function 'main' not found\n";
        if (opts.outputFormat == OutputFormat::HTML) {
            htmlReporter.addError("Required function 'main' not found (PhASAR-based Callgraph)");
        }
        return;
    }

    DIBasedTypeHierarchy TH(IRDB);
    LLVMBasedICFG ICFG(&IRDB, CallGraphAnalysisType::RTA, {"main"}, &TH);
    // LLVMBasedICFG ICFG(&IRDB, CallGraphAnalysisType::CHA, {"main"}, &TH);

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

    string dotFilename = opts.artifactsDir + "/phasar-callgraph.dot";
    string pngFilename = opts.artifactsDir + "/phasar-callgraph.png";

    std::ofstream icfgDotFile(dotFilename, std::ios::out | std::ios::trunc);
    if (!icfgDotFile.is_open()) {
        llvm::errs() << "Error: Could not create file " << dotFilename << "\n";
        return;
    }

    // std::string icfgDot = ICFG.exportICFGAsDot(false);
    // icfgDotFile << icfgDot;

    const auto& CG = ICFG.getCallGraph();
    icfgDotFile << "digraph PhasarCallGraph {\n";
    icfgDotFile << "  rankdir=LR;\n";
    icfgDotFile << "  node [shape=box, style=rounded];\n\n";

    std::unordered_set<std::string> uniqueEdges;

    for (const auto* Call : CG.getAllVertexCallSites()) {
        if (Call->isDebugOrPseudoInst()) continue;
        
        const Function* CallerFun = Call->getFunction();
        if (!CallerFun) continue;
        
        std::string callerName = CallerFun->getName().str();
        if (opts.shouldSkipFunction(callerName)) continue;
        
        for (const auto* CalleeFun : CG.getCalleesOfCallAt(Call)) {
            std::string calleeName = CalleeFun->getName().str();
            
            if (opts.shouldSkipFunction(calleeName)) continue;
            
            std::string demangledCaller = llvm::demangle(callerName);
            std::string demangledCallee = llvm::demangle(calleeName);

            std::string edge = "  \"" + demangledCaller + "\" -> \"" + demangledCallee + "\";\n";
            
            if (uniqueEdges.insert(edge).second) {
                icfgDotFile << edge;
            }
        }
    }
    icfgDotFile << "}\n";
    
    icfgDotFile.close();

    out << "\nPhASAR-CallGraph exported to " << dotFilename << "\n";
    
    // Пытаемся сгенерировать PNG через Graphviz (dot)
    int result = std::system(("dot -Tpng " + dotFilename + " -o " + pngFilename + " 2>/dev/null").c_str());
    
    if (result == 0) {
        out << "Image generated: " << pngFilename << "\n";
    } else {
        out << "To visualize, run: dot -Tpng " << dotFilename << " -o " << pngFilename << "\n";
    }

    if (opts.outputFormat == OutputFormat::HTML) {
        htmlReporter.addSection("Граф Вызовов (CallGraph) (Detailed)", "<pre>" + htmlOut.str() + "</pre>");
        htmlReporter.addImage("Phasar Callgraph Image", "phasar-callgraph.png");
    }
}

void runCallGraphAnalysis(LLVMProjectIRDB& IRDB, const Options& opts, HTMLReporter& htmlReporter) {
    if (opts.useErrorCheckingAlgorithm) {
        runMyRealizedAnalysis(IRDB, opts, htmlReporter);
        return;
    }

    if (opts.choice == AnalysisChoice::BasicAnalysis || opts.choice == AnalysisChoice::Both) {
        runMyRealizedAnalysis(IRDB, opts, htmlReporter);
    }

    if (opts.choice == AnalysisChoice::DetailedAnalysis || opts.choice == AnalysisChoice::Both) {
        runPhasarAnalysis(IRDB, opts, htmlReporter);
    }
}

void printCallGraphStats(const CallGraphStats& stats, std::ostream& out) {
    out << "СТАТИСТИКА ГРАФА ВЫЗОВОВ:\n";
    out << "Всего функций: " << stats.totalFunctions << "\n";
    out << "Всего мест вызовов: " << stats.totalCallSites << "\n";
    out << "Прямые вызовы: " << stats.directCalls << "\n";
    out << "Вызовы внешних функций: " << stats.externalCalls << "\n";
    out << "Рекурсивных функций: " << stats.recursiveFunctions << "\n";
}

void exportCallGraphDot(const vector<CallInfo>& calls, const string& filename, const string& legendFilename) {
    ofstream dotFile(filename, std::ios::out | std::ios::trunc);
    
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

    // Создаем отдельный файл для легенды
    ofstream legendFile(legendFilename, std::ios::out | std::ios::trunc);
    if (!legendFile.is_open()) {
        cerr << "Error: не удалось создать файл легенды " << legendFilename << "\n";
        return;
    }
    
    legendFile << "digraph Legend {\n";
    legendFile << "  rankdir=TB;\n";
    legendFile << "  node [shape=none, margin=0];\n";
    legendFile << "  legend [label=<\n";
    legendFile << "    <TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"4\">\n";
    legendFile << "      <TR><TD COLSPAN=\"2\" BORDER=\"0\"><B>Легенда</B></TD></TR>\n";
    legendFile << "      <TR><TD BGCOLOR=\"lightgreen\"></TD><TD ALIGN=\"left\">Точка входа (main)</TD></TR>\n";
    legendFile << "      <TR><TD BGCOLOR=\"white\"></TD><TD ALIGN=\"left\">Обычная функция</TD></TR>\n";
    legendFile << "      <TR><TD BGCOLOR=\"lightgray\"></TD><TD ALIGN=\"left\">Внешняя функция</TD></TR>\n";
    legendFile << "      <TR><TD BGCOLOR=\"lightsalmon\" COLOR=\"red\"></TD><TD ALIGN=\"left\">Недостижимая функция</TD></TR>\n";
    legendFile << "      <TR><TD BGCOLOR=\"white\"><FONT COLOR=\"red\"><B>&#8594;</B></FONT></TD><TD ALIGN=\"left\">Рекурсивный вызов</TD></TR>\n";
    legendFile << "    </TABLE>\n";
    legendFile << "  >];\n";
    legendFile << "}\n";
    legendFile.close();
}

void analyzeUnreachableFunctions(const vector<CallInfo>& calls, const unordered_set<string>& allFunctions, const Options& opts, std::ostream& out) {
    if (!opts.useErrorCheckingAlgorithm) {
        out << "\n=== АНАЛИЗ НЕДОСТИЖИМЫХ ФУНКЦИЙ ===\n";
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
        out << "\nДостижимых функций из main: " << reachable.size() << "\n";
        out << "Недостижимых функций из main: " << unreachable.size() << "\n";
    }
    
    if (!unreachable.empty()) {
        if (!opts.useErrorCheckingAlgorithm) {
            out << "\nСписок недостижимых функций:\n";
            for (const auto& func : unreachable) {
                out << "  - " << func << "\n";
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

void analyzeCallChains(const vector<CallInfo>& calls, const Options& opts, std::ostream& out) {
    if (!opts.useErrorCheckingAlgorithm) {
        out << "=== АНАЛИЗ ЦЕПОЧЕК ВЫЗОВОВ ===\n";
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
        out << "\nФункции-листья (не вызывают другие функции):\n";
    }

    for (const auto& func : allFunctions) {
        if (callGraph[func].empty() && !opts.useErrorCheckingAlgorithm) {
            out << "  - " << func << "\n";
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
        out << "\nФункции верхнего уровня (точки входа):\n";
    }
    
    for (const auto& func : allFunctions) {
        if (calledFunctions.find(func) == calledFunctions.end() && !opts.useErrorCheckingAlgorithm) {
            out << "  - " << func << "\n";
        }
    }
    
    if (!opts.useErrorCheckingAlgorithm) {
        out << "\nРекурсивные вызовы:\n";
    }

    bool foundRecursion = false;
    for (const auto& call : calls) {
        if (call.caller == call.callee) {
            if (!opts.useErrorCheckingAlgorithm) {
                out << "  - " << call.caller << " (прямая рекурсия)\n";
            }

            foundRecursion = true;
        }
    }

    if (!foundRecursion && !opts.useErrorCheckingAlgorithm) {
        out << "  Прямых рекурсивных вызовов не обнаружено.\n";
    }
    
    // Анализ недостижимых функций
    analyzeUnreachableFunctions(calls, allFunctions, opts, out);
}
