#ifndef PHASAR_ANALYSES_CALLGRAPH_H
#define PHASAR_ANALYSES_CALLGRAPH_H

#include "../dependes.hpp"

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

void runCallGraphAnalysis(LLVMProjectIRDB& IRDB, const Options& opts);

void printCallGraphStats(const CallGraphStats& stats);
void exportCallGraphDot(const vector<CallInfo>& calls, const string& filename);
void analyzeCallChains(const vector<CallInfo>& calls, const Options& opts);
void analyzeUnreachableFunctions(const vector<CallInfo>& calls, const unordered_set<string>& allFunctions, const Options& opts);
void reportUnreachableFunctions(const vector<string>& unreachable, const Options& opts);

#endif // PHASAR_ANALYSES_CALLGRAPH_H
