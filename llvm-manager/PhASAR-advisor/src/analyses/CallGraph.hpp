#ifndef PHASAR_ANALYSES_CALLGRAPH_H
#define PHASAR_ANALYSES_CALLGRAPH_H

#include "../dependes.hpp"

#include <html_reporter.hpp>

// Структура для хранения информации о вызове
struct CallInfo {
    string caller;           // Вызывающая функция
    string callee;           // Вызываемая функция
    string callSite;         // Место вызова (строка IR)
    bool isDirectCall;       // Прямой или косвенный вызов
    bool isExternal;         // Внешняя функция
};

// Структура для статистики графа вызовов
struct CallGraphStats {
    size_t totalFunctions = 0;
    size_t totalCallSites = 0;
    size_t directCalls = 0;
    size_t indirectCalls = 0;
    size_t externalCalls = 0;
    size_t recursiveFunctions = 0;
};

void runCallGraphAnalysis(LLVMProjectIRDB& IRDB, const Options& opts, HTMLReporter& htmlReporter);

void printCallGraphStats(const CallGraphStats& stats, std::ostream& out);
void exportCallGraphDot(const vector<CallInfo>& calls, const string& filename, const string& legendFilename);
void analyzeCallChains(const vector<CallInfo>& calls, const Options& opts, std::ostream& out);
void analyzeUnreachableFunctions(const vector<CallInfo>& calls, const unordered_set<string>& allFunctions, const Options& opts, std::ostream& out);
void reportUnreachableFunctions(const vector<string>& unreachable, const Options& opts);

#endif // PHASAR_ANALYSES_CALLGRAPH_H
