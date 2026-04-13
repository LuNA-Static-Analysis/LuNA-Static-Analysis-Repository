#include "UninitializedVariables.hpp"

#include <llvm/IR/Instructions.h>
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/IR/DebugInfoMetadata.h>
#include <llvm/IR/CFG.h>
#include <llvm/Support/raw_os_ostream.h>

#include "phasar/PhasarLLVM/TypeHierarchy/DIBasedTypeHierarchy.h"

using namespace llvm;

// Структура для отслеживания состояния переменной
struct VariableState {
    const AllocaInst* alloca;
    string name;

    bool isInitialized = false;
    bool hasRead = false;
    bool hasWrite = false;
    
    const Instruction* firstRead = nullptr;
    const Instruction* firstWrite = nullptr;
    vector<const Instruction*> uninitReads;
    
    unordered_set<const BasicBlock*> blocksWithWrite;  // В каких блоках есть запись
    unordered_set<const BasicBlock*> blocksWithRead;   // В каких блоках есть чтение
    vector<pair<const Instruction*, const BasicBlock*>> potentiallyUninitReads; // Возможно неинициализированные чтения
};

// Получить имя переменной из отладочной информации или IR
string getVariableName(const AllocaInst* AI) {
    // Пытаемся получить имя из отладочной информации
    if (auto* DbgDeclare = AI->getNextNode()) {
        if (auto* DDI = dyn_cast<DbgDeclareInst>(DbgDeclare)) {
            if (auto* Var = DDI->getVariable()) {
                return Var->getName().str();
            }
        }
    }
    
    // Ищем DbgDeclare в следующих инструкциях
    for (auto It = AI->getIterator(), End = AI->getParent()->end(); 
         It != End && std::distance(AI->getIterator(), It) < 5; ++It) {
        if (auto* DDI = dyn_cast<DbgDeclareInst>(&*It)) {
            if (DDI->getAddress() == AI) {
                if (auto* Var = DDI->getVariable()) {
                    return Var->getName().str();
                }
            }
        }
    }
    
    // Если нет отладочной информации, используем имя из IR
    if (AI->hasName()) {
        return AI->getName().str();
    }
    
    return "<unnamed>";
}

// Получить тип переменной в читаемом виде
string getTypeString(Type* T) {
    string typeStr;
    raw_string_ostream rso(typeStr);
    T->print(rso);
    return rso.str();
}

// Проверяет, является ли инструкция инициализацией
bool isInitialization(const StoreInst* SI, const AllocaInst* AI) {
    return SI->getPointerOperand() == AI;
}

// Проверяет, читает ли инструкция из переменной
bool isReadingFrom(const LoadInst* LI, const AllocaInst* AI) {
    return LI->getPointerOperand() == AI;
}

// Проверяет, может ли блок быть достигнут без инициализации переменной
bool canReachWithoutInit(const BasicBlock* readBB, 
                         const unordered_set<const BasicBlock*>& writeBBs,
                         const Function& F) {
    // Если чтение в том же блоке, что и запись, нужна более детальная проверка
    if (writeBBs.count(readBB) > 0) {
        return false;  
    }
    
    // Если нет записей вообще - точно проблема
    if (writeBBs.empty()) {
        return true;
    }
    
    // Если запись только в условных блоках (не в entry блоке)
    const BasicBlock* entryBB = &F.getEntryBlock();
    
    // Если чтение в entry блоке, а записей в нём нет - проблема
    if (readBB == entryBB && writeBBs.count(entryBB) == 0) {
        return true;
    }
    
    if (pred_begin(readBB) != pred_end(readBB)) {
        int predCount = 0;
        int predsWithWrite = 0;
        
        for (const BasicBlock* pred : predecessors(readBB)) {
            predCount++;
            if (writeBBs.count(pred) > 0) {
                predsWithWrite++;
            }
        }
        
        // Если не все предшественники имеют запись - потенциальная проблема
        if (predCount > 1 && predsWithWrite < predCount) {
            return true;
        }
    }
    
    return false;
}

// Проверяет, является ли переменная техническим артефактом компиляции
bool isTechnicalVariable(const AllocaInst* AI, const Function& F) {
    string varName = getVariableName(AI);
    
    // Если имя "<unnamed>" - это техническая переменная
    if (varName == "<unnamed>") {
        return true;
    }
    
    return false;
}

// Анализ функции на неинициализированные переменные
void analyzeFunction(const Function& F, unordered_map<const AllocaInst*, VariableState>& varStates) {
    // Первый проход: находим все аллокации
    for (const auto& BB : F) {
        for (const auto& I : BB) {
            if (const auto* AI = dyn_cast<AllocaInst>(&I)) {
                if (isTechnicalVariable(AI, F)) {
                    continue;
                }
                
                VariableState state;
                state.alloca = AI;
                state.name = getVariableName(AI);
                varStates[AI] = state;
            }
        }
    }
    
    // Второй проход: отслеживаем чтения и записи
    for (const auto& BB : F) {
        for (const auto& I : BB) {
            // Проверяем записи (Store)
            if (const auto* SI = dyn_cast<StoreInst>(&I)) {
                const Value* ptr = SI->getPointerOperand();
                
                for (auto& [alloca, state] : varStates) {
                    if (ptr == alloca) {
                        if (!state.hasWrite) {
                            state.firstWrite = SI;
                        }
                        state.hasWrite = true;
                        state.isInitialized = true;
                        state.blocksWithWrite.insert(&BB);
                    }
                }
            }
            
            // Проверяем чтения (Load)
            if (const auto* LI = dyn_cast<LoadInst>(&I)) {
                const Value* ptr = LI->getPointerOperand();
                
                for (auto& [alloca, state] : varStates) {
                    if (ptr == alloca) {
                        if (!state.hasRead) {
                            state.firstRead = LI;
                        }
                        state.hasRead = true;
                        state.blocksWithRead.insert(&BB);
                        
                        // Если читаем до инициализации - потенциальная проблема
                        if (!state.isInitialized) {
                            state.uninitReads.push_back(LI);
                        }
                        
                        // Проверяем, может ли это чтение быть выполнено без инициализации
                        if (canReachWithoutInit(&BB, state.blocksWithWrite, F)) {
                            state.potentiallyUninitReads.push_back({LI, &BB});
                        }
                    }
                }
            }
            
            // Проверяем передачу в функции (может быть инициализацией)
            if (const auto* CI = dyn_cast<CallInst>(&I)) {
                if (isa<IntrinsicInst>(CI)) {
                    continue;
                }
                
                for (unsigned i = 0; i < CI->arg_size(); ++i) {
                    Value* arg = CI->getArgOperand(i);
                    
                    for (auto& [alloca, state] : varStates) {
                        if (arg == alloca) {
                            if (arg->getType()->isPointerTy()) {
                                state.isInitialized = true;
                                if (!state.hasWrite) {
                                    state.firstWrite = CI;
                                }
                                state.hasWrite = true;
                                state.blocksWithWrite.insert(&BB);
                            }
                        }
                    }
                }
            }
        }
    }
}

static void runMyRealizedAnalysis(LLVMProjectIRDB& IRDB, const Options& opts, HTMLReporter& htmlReporter) {
    std::stringstream htmlOut;
    std::ostream& out = (opts.outputFormat == OutputFormat::HTML) ? static_cast<std::ostream&>(htmlOut) : static_cast<std::ostream&>(std::cout);

    out << "\n=== АНАЛИЗ НЕИНИЦИАЛИЗИРОВАННЫХ ПЕРЕМЕННЫХ ===\n";
    
    try {
        auto* M = IRDB.getModule();

        if (!M) {
            cerr << "Error: не удалось получить модуль из IRDB.\n";
            if (opts.outputFormat == OutputFormat::HTML) {
                htmlReporter.addError("Error: не удалось получить модуль из IRDB.");
            }
            return;
        }
        
        size_t totalVars = 0;
        size_t uninitVars = 0;
        size_t conditionalVars = 0;
        size_t unusedVars = 0;
        
        for (const auto& F : *M) {
            if (F.isDeclaration()) {
                continue;
            }
            
            unordered_map<const AllocaInst*, VariableState> varStates;
            analyzeFunction(F, varStates);
            
            if (varStates.empty()) {
                continue;
            }
            
            totalVars += varStates.size();
            
            // Собираем все проблемы в буфер
            std::ostringstream funcOutput;
            bool hasFunctionProblems = false;
            
            for (const auto& [alloca, state] : varStates) {
                bool hasProblem = false;
                
                // Проблема 1: Чтение неинициализированной переменной
                if (!state.uninitReads.empty()) {
                    hasProblem = true;
                    hasFunctionProblems = true;
                    uninitVars++;
                    
                    funcOutput << "ПРЕДУПРЕЖДЕНИЕ: Неинициализированная переменная\n";
                    funcOutput << "   Переменная: " << state.name << "\n";
                    funcOutput << "   Тип: " << getTypeString(alloca->getAllocatedType()) << "\n";
                    funcOutput << "   Аллокация: " << llvmIRToString(alloca) << "\n";
                    
                    funcOutput << "   Чтения до инициализации:\n";
                    for (const auto* readInst : state.uninitReads) {
                        funcOutput << "     → " << llvmIRToString(readInst) << "\n";
                    }
                    
                    if (state.hasWrite) {
                        funcOutput << "   Первая запись: " << llvmIRToString(state.firstWrite) << "\n";
                    }
                }
                
                // Проблема 1.5: Условная инициализация (потенциально неинициализированная)
                if (!state.potentiallyUninitReads.empty() && state.uninitReads.empty()) {
                    hasProblem = true;
                    hasFunctionProblems = true;
                    conditionalVars++;
                    
                    funcOutput << "ПРЕДУПРЕЖДЕНИЕ: Условная инициализация (может быть неинициализирована)\n";
                    funcOutput << "   Переменная: " << state.name << "\n";
                    funcOutput << "   Тип: " << getTypeString(alloca->getAllocatedType()) << "\n";
                    funcOutput << "   Аллокация: " << llvmIRToString(alloca) << "\n";
                    
                    funcOutput << "   Переменная инициализируется не во всех путях выполнения:\n";
                    funcOutput << "   Блоки с записью: " << state.blocksWithWrite.size() << "\n";
                    
                    funcOutput << "   Потенциально небезопасные чтения:\n";
                    for (const auto& [readInst, readBB] : state.potentiallyUninitReads) {
                        funcOutput << "       " << llvmIRToString(readInst) << "\n";
                    }
                    
                    if (state.hasWrite) {
                        funcOutput << "   Первая запись: " << llvmIRToString(state.firstWrite) << "\n";
                    }
                    
                    funcOutput << "   Рекомендация: Инициализируйте переменную при объявлении или во всех ветках\n";
                }
                
                // Проблема 2: Переменная объявлена, но никогда не используется
                if (!state.hasRead && !state.hasWrite) {
                    hasProblem = true;
                    hasFunctionProblems = true;
                    unusedVars++;

                    funcOutput << "ПРЕДУПРЕЖДЕНИЕ: Неиспользуемая переменная\n";
                    funcOutput << "   Переменная: " << state.name << "\n";
                    funcOutput << "   Тип: " << getTypeString(alloca->getAllocatedType()) << "\n";
                    funcOutput << "   Аллокация: " << llvmIRToString(alloca) << "\n";
                    funcOutput << "   Никогда не читается и не записывается\n";
                }
                
                // Проблема 3: Записывается, но никогда не читается
                if (state.hasWrite && !state.hasRead) {
                    funcOutput << "ИНФО: Переменная записывается, но не читается\n";
                    funcOutput << "   Переменная: " << state.name << "\n";
                    funcOutput << "   Тип: " << getTypeString(alloca->getAllocatedType()) << "\n";
                    funcOutput << "   Аллокация: " << llvmIRToString(alloca) << "\n";
                    funcOutput << "   Первая запись: " << llvmIRToString(state.firstWrite) << "\n";
                }
            }
            
            // Выводим функцию только если есть проблемы
            if (hasFunctionProblems) {
                out << "\nФункция: " << F.getName().str() << "\n";
                out << funcOutput.str();
            }
        }
        
        out << "\nСТАТИСТИКА:\n";
        out << "Всего локальных переменных: " << totalVars << "\n";
        out << "Неинициализированных переменных: " << uninitVars << "\n";
        out << "Условно инициализированных: " << conditionalVars << "\n";
        out << "Неиспользуемых переменных: " << unusedVars << "\n";
        
        if (uninitVars > 0) {
            out << "\nВНИМАНИЕ: Обнаружены потенциально опасные неинициализированные переменные!\n";
        }
        
        if (conditionalVars > 0) {
            out << "\nВНИМАНИЕ: Обнаружены переменные с условной инициализацией!\n";
            out << "   Эти переменные могут быть неинициализированы в некоторых путях выполнения.\n";
        }
        
        if (uninitVars == 0 && conditionalVars == 0) {
            out << "\nКритических проблем не обнаружено.\n";
        }

        if (opts.outputFormat == OutputFormat::HTML) {
            htmlReporter.addSection("Анализ неинициализированных переменных (Basic)", "<pre>" + htmlOut.str() + "</pre>");
        }

    } catch (const exception& e) {
        cerr << "Critical error: " << e.what() << "\n";
    }
}

static void runPhasarAnalysis(LLVMProjectIRDB& IRDB, const Options& opts, HTMLReporter& htmlReporter) {
    std::stringstream htmlOut;
    std::ostream& out = (opts.outputFormat == OutputFormat::HTML) ? static_cast<std::ostream&>(htmlOut) : static_cast<std::ostream&>(std::cout);

    out << "\n=== АНАЛИЗ НЕИНИЦИАЛИЗИРОВАННЫХ ПЕРЕМЕННЫХ С ИСПОЛЬЗОВАНИЕМ PhASAR ===\n\n";

    IFDSUninitializedVariables UninitProblem(&IRDB, {"main"});
    DIBasedTypeHierarchy TH(IRDB);
    
    LLVMBasedICFG ICFG(&IRDB, CallGraphAnalysisType::CHA, {"main"}, &TH);
    
    IFDSSolver Solver(UninitProblem, &ICFG);
    Solver.solve();

    llvm::raw_os_ostream raw_out(out);
    UninitProblem.emitTextReport(Solver.getSolverResults(), raw_out);
    raw_out.flush();
    
    // Дополнительная статистика
    // out << "\n" << string(64, '-') << "\n";
    out << "Statistics:\n";
    // out << string(64, '-') << "\n";
    
    const auto& UndefUses = UninitProblem.getAllUndefUses();
    std::set<const llvm::Function*> AffectedFunctions;

    for (const auto& [Inst, Vars] : UndefUses) {
        if (const auto* F = Inst->getFunction()) {
            AffectedFunctions.insert(F);
        }
    }
    
    out << "  * Functions with uninitialized variables: " << AffectedFunctions.size() << "\n";
    
    if (!AffectedFunctions.empty()) {
        out << "\n  Affected functions:\n";
        
        for (const auto* F : AffectedFunctions) {
            out << "    -> " << F->getName().str() << "\n";
        }
    }
    
    out << "\nAnalysis completed successfully!\n";
    
    if (opts.outputFormat == OutputFormat::HTML) {
        htmlReporter.addSection("Анализ неинициализированных переменных (Detailed)", "<pre>" + htmlOut.str() + "</pre>");
    }
}

void runUninitializedVariables(LLVMProjectIRDB& IRDB, const Options& opts, HTMLReporter& htmlReporter) {
    if (opts.choice == AnalysisChoice::BasicAnalysis || opts.choice == AnalysisChoice::Both) {
        runMyRealizedAnalysis(IRDB, opts, htmlReporter);
    }
    
    if (opts.choice == AnalysisChoice::DetailedAnalysis || opts.choice == AnalysisChoice::Both) {
        runPhasarAnalysis(IRDB, opts, htmlReporter);
    }
}
