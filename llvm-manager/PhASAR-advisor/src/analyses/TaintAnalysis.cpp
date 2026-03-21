#include "TaintAnalysis.hpp"

// #include "phasar/DataFlow.h"               // For solveIFDSProblem()
// #include "phasar/PhasarLLVM/DB.h"          // For the LLVMProjectIRDB
// #include "phasar/PhasarLLVM/DataFlow.h"    // For the IFDSTaintAnalysis
// #include "phasar/PhasarLLVM/Pointer.h"     // For the LLVMAliasSet
// #include "phasar/PhasarLLVM/TaintConfig.h" // For the LLVMTaintConfig

#include <llvm/Support/raw_ostream.h>

#include <phasar/ControlFlow/CallGraphAnalysisType.h>
#include <phasar/PhasarLLVM/ControlFlow/LLVMBasedICFG.h>
#include <phasar/PhasarLLVM/DataFlow/IfdsIde/Problems/IFDSTaintAnalysis.h>
#include <phasar/PhasarLLVM/Pointer/LLVMAliasSet.h>
#include <phasar/PhasarLLVM/SimpleAnalysisConstructor.h>
#include <phasar/PhasarLLVM/TaintConfig/LLVMTaintConfig.h>
#include <phasar/PhasarLLVM/Utils/LLVMShorthands.h>

#include <set>
#include <string>
#include <vector>

// Источники загрязнения (Taint Sources)
static const set<string> TAINT_SOURCES = {
    "scanf", "gets", "fgets", "getchar", "getc", "fgetc",
    "read", "recv", "recvfrom", "getenv", "getpwnam", 
    "fread", "input", "cin"
};

// Опасные стоки (Dangerous Sinks)
static const set<string> DANGEROUS_SINKS = {
    "system", "popen", "execl", "execlp", "execle", "execv", "execvp",
    "printf", "fprintf", "sprintf", "snprintf", "vprintf", "vfprintf",
    "strcpy", "strcat", "memcpy", "memmove", "gets", "scanf"
};

// Функции выделения памяти
static const set<string> MEMORY_ALLOC = {
    "malloc", "calloc", "realloc", "new"
};

// Функции освобождения памяти  
static const set<string> MEMORY_FREE = {
    "free", "delete"
};

struct SecurityIssue {
    enum Type {
        TAINT_FLOW,           // Поток загрязненных данных
        BUFFER_OVERFLOW,      // Переполнение буфера
        COMMAND_INJECTION,    // Инъекция команд
        FORMAT_STRING,        // Уязвимость форматной строки
        USE_AFTER_FREE,       // Использование после освобождения
        MEMORY_LEAK,          // Утечка памяти
        NULL_DEREFERENCE,     // Разыменование NULL
        UNINITIALIZED_USE     // Использование неинициализированных данных
    } type;
    
    string description;
    string location;
    string severity;
};

void analyzeSecurityIssues(llvm::Function& F, vector<SecurityIssue>& issues);
void detectTaintFlow(llvm::Function& F, vector<SecurityIssue>& issues);
void detectMemoryIssues(llvm::Function& F, vector<SecurityIssue>& issues);
void detectBufferOverflows(llvm::Function& F, vector<SecurityIssue>& issues);
string getFunctionNameFromCall(llvm::CallInst* CI);

static void runMyRealizedAnalysis(LLVMProjectIRDB& IRDB, const Options& opts) {
    cout << "\n=== АНАЛИЗ БЕЗОПАСНОСТИ ===\n\n";

    try {
        auto* M = IRDB.getModule();

        if (!M) {
            cerr << "Error: Не удалось получить модуль из IRDB.\n";
            return;
        }

        cout << "Анализ безопасности модуля: " << M->getName().str() << "\n\n";
        
        vector<SecurityIssue> allIssues;
        int functionsAnalyzed = 0;
        
        for (auto& F : *M) {
            if (!F.isDeclaration()) {
                cout << "Анализ функции: " << F.getName().str() << "\n";
                functionsAnalyzed++;
                
                vector<SecurityIssue> functionIssues;
                analyzeSecurityIssues(F, functionIssues);
                
                if (!functionIssues.empty()) {
                    for (const auto& issue : functionIssues) {
                        cout << "  " << issue.severity << ": " << issue.description << "\n";
                        cout << "     Расположение: " << issue.location << "\n";
                        allIssues.push_back(issue);
                    }
                } else {
                    cout << "  Критических проблем не обнаружено\n";
                }

                cout << "\n";
            }
        }
        
        // Итоговая статистика
        cout << "ИТОГОВАЯ СТАТИСТИКА БЕЗОПАСНОСТИ:\n";
        cout << "  Проанализировано функций: " << functionsAnalyzed << "\n";
        cout << "  Всего найдено проблем: " << allIssues.size() << "\n";

        // Группировка по типам
        map<SecurityIssue::Type, int> issuesByType;
        for (const auto& issue : allIssues) {
            issuesByType[issue.type]++;
        }
        
        if (!issuesByType.empty()) {
            cout << "  Типы проблем:\n";
            for (const auto& [type, count] : issuesByType) {
                string typeName;
                switch (type) {
                    case SecurityIssue::TAINT_FLOW: typeName = "Поток загрязненных данных"; break;
                    case SecurityIssue::BUFFER_OVERFLOW: typeName = "Переполнение буфера"; break;
                    case SecurityIssue::COMMAND_INJECTION: typeName = "Инъекция команд"; break;
                    case SecurityIssue::FORMAT_STRING: typeName = "Уязвимость форматной строки"; break;
                    case SecurityIssue::USE_AFTER_FREE: typeName = "Use-after-free"; break;
                    case SecurityIssue::MEMORY_LEAK: typeName = "Утечка памяти"; break;
                    case SecurityIssue::NULL_DEREFERENCE: typeName = "NULL dereference"; break;
                    case SecurityIssue::UNINITIALIZED_USE: typeName = "Неинициализированные данные"; break;
                }
                cout << "    - " << typeName << ": " << count << "\n";
            }
        }
    } catch (const exception& e) {
        cerr << "Error: Ошибка при выполнении анализа безопасности: " << e.what() << "\n";
    }
}

static void runPhasarAnalysis(LLVMProjectIRDB& IRDB, const Options& opts) {
    cout << "\n=== АНАЛИЗ ЗАГРЯЗНЕНИЯ ДАННЫХ (TAINT ANALYSIS) С ИСПОЛЬЗОВАНИЕМ PhASAR ===\n\n";

    // LLVMTaintConfig TC(HA.getProjectIRDB());
    // TC.print();
    
    // auto TaintProblem = psr::createAnalysisProblem<psr::IFDSTaintAnalysis>(HA, &TC, EntryPoints, false);
    // solveIFDSProblem(TaintProblem, HA.getICFG());
    
    // for (const auto& [key, values] : TaintProblem.Leaks) {
    //     llvm::outs() << "Обнаружен поток загрязненных данных от " << llvmIRToString(key) << "\n";
    //     for (const auto* val : values) {
    //         llvm::outs() << "> " << llvmIRToShortString(val) << "\n";
    //     }
    //     llvm::outs() << "\n";
    // }

    psr::LLVMAliasSet AS(&IRDB);
    psr::LLVMTaintConfig TC(IRDB);

    TC.print();

    psr::IFDSTaintAnalysis TaintProblem(&IRDB, &AS, &TC, {"main"}, false);
    psr::LLVMBasedICFG ICFG(&IRDB, psr::CallGraphAnalysisType::OTF, {"main"}, nullptr, &AS);
    psr::solveIFDSProblem(TaintProblem, ICFG);

    for (const auto &[LeakInst, LeakFacts] : TaintProblem.Leaks) {
        llvm::outs() << "Detected taint leak at " << psr::llvmIRToString(LeakInst) << '\n';
        for (const auto *Fact : LeakFacts) {
            llvm::outs() << ">  leaking fact " << psr::llvmIRToShortString(Fact) << '\n';
        }
        llvm::outs() << '\n';
    }
}

void runTaintAnalysis(LLVMProjectIRDB& IRDB, const Options& opts) {
    if (opts.choice == AnalysisChoice::MyRealizedAnalysis || opts.choice == AnalysisChoice::Both) {
        runMyRealizedAnalysis(IRDB, opts);
    }
    
    if (opts.choice == AnalysisChoice::PhasarAnalysis || opts.choice == AnalysisChoice::Both) {
        runPhasarAnalysis(IRDB, opts);
    }
}

void analyzeSecurityIssues(llvm::Function& F, vector<SecurityIssue>& issues) {
    detectTaintFlow(F, issues);
    detectMemoryIssues(F, issues);
    detectBufferOverflows(F, issues);
}

void detectTaintFlow(llvm::Function& F, vector<SecurityIssue>& issues) {
    set<llvm::Value*> taintedValues;
    
    for (auto& BB : F) {
        for (auto& I : BB) {
            if (auto* CI = llvm::dyn_cast<llvm::CallInst>(&I)) {
                string funcName = getFunctionNameFromCall(CI);
                
                // Обнаруживаем источники загрязнения
                if (TAINT_SOURCES.count(funcName)) {
                    taintedValues.insert(CI);
                    
                    SecurityIssue issue;
                    issue.type = SecurityIssue::TAINT_FLOW;
                    issue.severity = "ВЫСОКАЯ";
                    issue.description = "Источник загрязнения: " + funcName + "()";
                    issue.location = F.getName().str();
                    issues.push_back(issue);
                }
                
                // Обнаруживаем опасные стоки
                if (DANGEROUS_SINKS.count(funcName)) {
                    bool usesTaintedData = false;
                    
                    // Проверяем аргументы на загрязнение
                    for (unsigned i = 0; i < CI->arg_size(); ++i) {
                        if (taintedValues.count(CI->getArgOperand(i))) {
                            usesTaintedData = true;
                            break;
                        }
                    }
                    
                    SecurityIssue issue;
                    if (usesTaintedData) {
                        issue.severity = "КРИТИЧЕСКАЯ";
                        issue.description = "Загрязненные данные передаются в " + funcName + "()";
                        
                        if (funcName == "system" || funcName == "popen") {
                            issue.type = SecurityIssue::COMMAND_INJECTION;
                            issue.description += " - возможна инъекция команд!";
                        } else if (funcName.find("printf") != string::npos) {
                            issue.type = SecurityIssue::FORMAT_STRING;
                            issue.description += " - уязвимость форматной строки!";
                        } else {
                            issue.type = SecurityIssue::TAINT_FLOW;
                        }
                    } else {
                        issue.severity = "СРЕДНЯЯ";
                        issue.type = SecurityIssue::TAINT_FLOW;
                        issue.description = "Потенциально опасная функция: " + funcName + "()";
                    }
                    
                    issue.location = F.getName().str();
                    issues.push_back(issue);
                }
            }
        }
    }
}

void detectMemoryIssues(llvm::Function& F, vector<SecurityIssue>& issues) {
    set<llvm::Value*> allocatedPointers;
    set<llvm::Value*> freedPointers;
    
    for (auto& BB : F) {
        for (auto& I : BB) {
            if (auto* CI = llvm::dyn_cast<llvm::CallInst>(&I)) {
                string funcName = getFunctionNameFromCall(CI);
                
                // Отслеживаем выделение памяти
                if (MEMORY_ALLOC.count(funcName)) {
                    allocatedPointers.insert(CI);
                }
                
                // Отслеживаем освобождение памяти
                if (MEMORY_FREE.count(funcName)) {
                    if (CI->arg_size() > 0) {
                        llvm::Value* ptr = CI->getArgOperand(0);
                        
                        if (freedPointers.count(ptr)) {
                            SecurityIssue issue;
                            issue.type = SecurityIssue::USE_AFTER_FREE;
                            issue.severity = "КРИТИЧЕСКАЯ";
                            issue.description = "Double-free: повторное освобождение памяти";
                            issue.location = F.getName().str();
                            issues.push_back(issue);
                        }
                        
                        freedPointers.insert(ptr);
                    }
                }
            }
            
            // Обнаруживаем использование после освобождения
            if (auto* LI = llvm::dyn_cast<llvm::LoadInst>(&I)) {
                llvm::Value* ptr = LI->getPointerOperand();
                if (freedPointers.count(ptr)) {
                    SecurityIssue issue;
                    issue.type = SecurityIssue::USE_AFTER_FREE;
                    issue.severity = "КРИТИЧЕСКАЯ";
                    issue.description = "Use-after-free: использование освобожденной памяти";
                    issue.location = F.getName().str();
                    issues.push_back(issue);
                }
            }
            
            if (auto* SI = llvm::dyn_cast<llvm::StoreInst>(&I)) {
                llvm::Value* ptr = SI->getPointerOperand();
                if (freedPointers.count(ptr)) {
                    SecurityIssue issue;
                    issue.type = SecurityIssue::USE_AFTER_FREE;
                    issue.severity = "КРИТИЧЕСКАЯ";
                    issue.description = "Use-after-free: запись в освобожденную память";
                    issue.location = F.getName().str();
                    issues.push_back(issue);
                }
            }
        }
    }
    
    // Проверяем на утечки памяти (упрощенно)
    for (llvm::Value* ptr : allocatedPointers) {
        if (!freedPointers.count(ptr)) {
            SecurityIssue issue;
            issue.type = SecurityIssue::MEMORY_LEAK;
            issue.severity = "СРЕДНЯЯ";
            issue.description = "Потенциальная утечка памяти: память не освобождена";
            issue.location = F.getName().str();
            issues.push_back(issue);
        }
    }
}

void detectBufferOverflows(llvm::Function& F, vector<SecurityIssue>& issues) {
    for (auto& BB : F) {
        for (auto& I : BB) {
            if (auto* CI = llvm::dyn_cast<llvm::CallInst>(&I)) {
                string funcName = getFunctionNameFromCall(CI);
                
                // Опасные функции для переполнения буфера
                if (funcName == "strcpy" || funcName == "strcat" || 
                    funcName == "sprintf" || funcName == "gets") {
                    
                    SecurityIssue issue;
                    issue.type = SecurityIssue::BUFFER_OVERFLOW;
                    issue.severity = "ВЫСОКАЯ";
                    issue.description = "Опасная функция " + funcName + "() может вызвать переполнение буфера";
                    issue.location = F.getName().str();
                    issues.push_back(issue);
                }
            }
        }
    }
}

string getFunctionNameFromCall(llvm::CallInst* CI) {
    if (auto* func = CI->getCalledFunction()) {
        return func->getName().str();
    }
    return "unknown";
}
