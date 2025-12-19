#include "CodeComplexity.hpp"

#include <llvm/IR/CFG.h>
#include <llvm/Analysis/LoopInfo.h>
#include <llvm/Analysis/PostDominators.h>
#include <llvm/IR/Dominators.h>

// Пороговые значения для предупреждений
struct ComplexityThresholds {
    static constexpr size_t MAX_CYCLOMATIC_COMPLEXITY = 10;
    static constexpr size_t MAX_NESTING_DEPTH = 4;
    static constexpr size_t MAX_INSTRUCTIONS = 200;
    static constexpr size_t MAX_PARAMETERS = 5;
    static constexpr size_t MAX_BASIC_BLOCKS = 30;
    static constexpr size_t MAX_BRANCH_POINTS = 15;
};

struct FunctionComplexityMetrics {
    string functionName;
    size_t cyclomaticComplexity = 0;
    size_t nestingDepth = 0;
    size_t instructionCount = 0;
    size_t parameterCount = 0;
    size_t basicBlockCount = 0;
    size_t branchPoints = 0;
    
    vector<string> warnings;
    
    void checkThresholds() {
        if (cyclomaticComplexity > ComplexityThresholds::MAX_CYCLOMATIC_COMPLEXITY) {
            warnings.push_back("Цикломатическая сложность слишком высока: " + 
                             to_string(cyclomaticComplexity) + " > " + 
                             to_string(ComplexityThresholds::MAX_CYCLOMATIC_COMPLEXITY));
        }
        
        if (nestingDepth > ComplexityThresholds::MAX_NESTING_DEPTH) {
            warnings.push_back("Глубина вложенности слишком высока: " + 
                             to_string(nestingDepth) + " > " + 
                             to_string(ComplexityThresholds::MAX_NESTING_DEPTH));
        }
        
        if (instructionCount > ComplexityThresholds::MAX_INSTRUCTIONS) {
            warnings.push_back("Слишком много инструкций: " + 
                             to_string(instructionCount) + " > " + 
                             to_string(ComplexityThresholds::MAX_INSTRUCTIONS));
        }
        
        if (parameterCount > ComplexityThresholds::MAX_PARAMETERS) {
            warnings.push_back("Слишком много параметров: " + 
                             to_string(parameterCount) + " > " + 
                             to_string(ComplexityThresholds::MAX_PARAMETERS));
        }
        
        if (basicBlockCount > ComplexityThresholds::MAX_BASIC_BLOCKS) {
            warnings.push_back("Слишком много базовых блоков: " + 
                             to_string(basicBlockCount) + " > " + 
                             to_string(ComplexityThresholds::MAX_BASIC_BLOCKS));
        }
    }
};

// Вычисление цикломатической сложности: M = E - N + 2P
// где E - рёбра, N - узлы, P - компоненты связности (обычно 1)
size_t calculateCyclomaticComplexity(llvm::Function& F) {
    if (F.empty()) return 0;
    
    size_t nodes = 0;  // N - количество базовых блоков
    size_t edges = 0;  // E - количество рёбер CFG
    
    // Подсчитываем узлы и рёбра
    for (auto& BB : F) {
        nodes++;
        
        // Подсчитываем исходящие рёбра из текущего базового блока
        auto* term = BB.getTerminator();
        
        if (auto* br = llvm::dyn_cast<llvm::BranchInst>(term)) {
            if (br->isConditional()) {
                edges += 2;  
            } else {
                edges += 1;  
            }
        } else if (auto* sw = llvm::dyn_cast<llvm::SwitchInst>(term)) {
            edges += sw->getNumCases() + 1;
        } else if (auto* invoke = llvm::dyn_cast<llvm::InvokeInst>(term)) {
            edges += 2;  
        } else if (llvm::isa<llvm::ReturnInst>(term) || 
                   llvm::isa<llvm::UnreachableInst>(term)) {
            edges += 0;
        } else {
            edges += llvm::succ_size(&BB);
        }
    }
    
    size_t complexity = 0;
    if (edges >= nodes) {
        complexity = edges - nodes + 2;
    } else {
        complexity = 1;
    }
    
    return complexity;
}

// Вычисление максимальной глубины вложенности через доминаторы
size_t calculateNestingDepth(llvm::Function& F) {
    if (F.empty()) return 0;
    
    llvm::DominatorTree DT(F);
    
    size_t maxDepth = 0;
    
    for (auto& BB : F) {
        size_t depth = 0;
        llvm::DomTreeNode* Node = DT.getNode(&BB);
        
        while (Node) {
            depth++;
            Node = Node->getIDom();
        }
        
        if (depth > maxDepth) {
            maxDepth = depth;
        }
    }
    
    return maxDepth;
}

// Подсчёт точек ветвления
size_t countBranchPoints(llvm::Function& F) {
    size_t branches = 0;
    
    for (auto& BB : F) {
        auto* term = BB.getTerminator();
        
        if (llvm::isa<llvm::BranchInst>(term) || 
            llvm::isa<llvm::SwitchInst>(term) ||
            llvm::isa<llvm::InvokeInst>(term)) {
            branches++;
        }
    }
    
    return branches;
}

FunctionComplexityMetrics analyzeFunctionComplexity(llvm::Function& F) {
    FunctionComplexityMetrics metrics;
    
    metrics.functionName = F.getName().str();
    metrics.cyclomaticComplexity = calculateCyclomaticComplexity(F);
    metrics.nestingDepth = calculateNestingDepth(F);
    metrics.instructionCount = F.getInstructionCount();
    metrics.parameterCount = F.arg_size();
    metrics.basicBlockCount = F.size();
    metrics.branchPoints = countBranchPoints(F);
    
    metrics.checkThresholds();
    
    return metrics;
}

void printComplexityMetrics(const FunctionComplexityMetrics& metrics) {
    cout << "\n  Функция: " << metrics.functionName << "\n";
    cout << "    Цикломатическая сложность: " << metrics.cyclomaticComplexity;
    if (metrics.cyclomaticComplexity > ComplexityThresholds::MAX_CYCLOMATIC_COMPLEXITY) {
        cout << "  [ПРЕДУПРЕЖДЕНИЕ]";
    }
    cout << "\n";
    
    cout << "    Глубина вложенности: " << metrics.nestingDepth;
    if (metrics.nestingDepth > ComplexityThresholds::MAX_NESTING_DEPTH) {
        cout << "  [ПРЕДУПРЕЖДЕНИЕ]";
    }
    cout << "\n";
    
    cout << "    Количество инструкций: " << metrics.instructionCount;
    if (metrics.instructionCount > ComplexityThresholds::MAX_INSTRUCTIONS) {
        cout << "  [ПРЕДУПРЕЖДЕНИЕ]";
    }
    cout << "\n";
    
    cout << "    Количество параметров: " << metrics.parameterCount;
    if (metrics.parameterCount > ComplexityThresholds::MAX_PARAMETERS) {
        cout << "  [ПРЕДУПРЕЖДЕНИЕ]";
    }
    cout << "\n";
    
    cout << "    Базовых блоков: " << metrics.basicBlockCount;
    if (metrics.basicBlockCount > ComplexityThresholds::MAX_BASIC_BLOCKS) {
        cout << "  [ПРЕДУПРЕЖДЕНИЕ]";
    }
    cout << "\n";
    
    cout << "    Точек ветвления: " << metrics.branchPoints << "\n";
    
    if (!metrics.warnings.empty()) {
        cout << "\n    ПРЕДУПРЕЖДЕНИЯ:\n";
        for (const auto& warning : metrics.warnings) {
            cout << "      [ПРЕДУПРЕЖДЕНИЕ]  " << warning << "\n";
        }
    }
}

static void runMyRealizedAnalysis(LLVMProjectIRDB& IRDB, const Options& opts) {
    cout << "\n=== АНАЛИЗ СЛОЖНОСТИ КОДА ===\n";
    
    try {
        auto* M = IRDB.getModule();
        
        if (!M) {
            cerr << "Error: Не удалось получить модуль из IRDB.\n";
            return;
        }
        
        vector<FunctionComplexityMetrics> allMetrics;
        size_t functionsWithWarnings = 0;
        
        cout << "\nАНАЛИЗ СЛОЖНОСТИ ФУНКЦИЙ (без внешних и приватных):\n";
        
        for (auto& F : *M) {
            if (F.isDeclaration()) {
                continue;
            }
            
            string funcName = F.getName().str();
            if (opts.shouldSkipFunction(funcName)) {
                continue;
            }
            
            auto metrics = analyzeFunctionComplexity(F);
            allMetrics.push_back(metrics);
            
            if (!metrics.warnings.empty()) {
                functionsWithWarnings++;
            }
            
            printComplexityMetrics(metrics);
        }
        
        // Статистика
        if (!allMetrics.empty()) {
            cout << "\nОБЩАЯ СТАТИСТИКА:\n";
            cout << "  Всего проанализировано функций: " << allMetrics.size() << "\n";
            cout << "  Функций с предупреждениями: " << functionsWithWarnings << "\n";
            
            // Вычисляем средние значения
            double avgCyclomatic = 0, avgNesting = 0, avgInstructions = 0, avgParams = 0, avgBlocks = 0;
            size_t maxCyclomatic = 0, maxNesting = 0, maxInstructions = 0;
            
            for (const auto& m : allMetrics) {
                avgCyclomatic += m.cyclomaticComplexity;
                avgNesting += m.nestingDepth;
                avgInstructions += m.instructionCount;
                avgParams += m.parameterCount;
                avgBlocks += m.basicBlockCount;
                
                maxCyclomatic = max(maxCyclomatic, m.cyclomaticComplexity);
                maxNesting = max(maxNesting, m.nestingDepth);
                maxInstructions = max(maxInstructions, m.instructionCount);
            }
            
            cout << "\n  МАКСИМАЛЬНЫЕ ЗНАЧЕНИЯ:\n";
            cout << "    Цикломатическая сложность: " << maxCyclomatic << "\n";
            cout << "    Глубина вложенности: " << maxNesting << "\n";
            cout << "    Количество инструкций: " << maxInstructions << "\n";
            
            cout << "\n  ПОРОГОВЫЕ ЗНАЧЕНИЯ:\n";
            cout << "    Макс. цикломатическая сложность: " << ComplexityThresholds::MAX_CYCLOMATIC_COMPLEXITY << "\n";
            cout << "    Макс. глубина вложенности: " << ComplexityThresholds::MAX_NESTING_DEPTH << "\n";
            cout << "    Макс. количество инструкций: " << ComplexityThresholds::MAX_INSTRUCTIONS << "\n";
            cout << "    Макс. количество параметров: " << ComplexityThresholds::MAX_PARAMETERS << "\n";
            cout << "    Макс. базовых блоков: " << ComplexityThresholds::MAX_BASIC_BLOCKS << "\n";
            
            if (functionsWithWarnings > 0) {
                cout << "\n  РЕКОМЕНДАЦИИ:\n";
                cout << "    Обнаружены функции с повышенной сложностью.\n";
                cout << "    Рекомендуется рефакторинг для улучшения читаемости и поддерживаемости кода\n";
            } else {
                cout << "\n  ✓ Все функции имеют приемлемый уровень сложности!\n";
            }
        } else {
            cout << "\n  Не найдено функций для анализа.\n";
        }
        
    } catch (const exception& e) {
        cerr << "Error: Ошибка при анализе сложности кода: " << e.what() << "\n";
    }
}

static void runPhasarAnalysis(LLVMProjectIRDB& IRDB, const Options& opts) {
    cout << "\n=== АНАЛИЗ СЛОЖНОСТИ С ИСПОЛЬЗОВАНИЕМ PhASAR ===\n";
    cout << "PhASAR не предоставляет встроенного анализа сложности кода.\n";
    cout << "Используйте --my-analysis или --both для запуска собственного анализа.\n";
}

void runCodeComplexityAnalysis(LLVMProjectIRDB& IRDB, const Options& opts) {
    if (opts.choice == AnalysisChoice::MyRealizedAnalysis || opts.choice == AnalysisChoice::Both) {
        runMyRealizedAnalysis(IRDB, opts);
    }
    
    if (opts.choice == AnalysisChoice::PhasarAnalysis) {
        runPhasarAnalysis(IRDB, opts);
    }
}
