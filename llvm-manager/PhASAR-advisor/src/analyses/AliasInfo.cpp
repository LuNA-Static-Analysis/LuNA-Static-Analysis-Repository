#include "AliasInfo.hpp"

#include <llvm/IR/InstIterator.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Operator.h>
#include <llvm/IR/Constants.h>
#include <llvm/Support/raw_ostream.h>

#include <phasar/PhasarLLVM/Pointer/LLVMAliasSet.h>
#include <phasar/PhasarLLVM/Pointer/LLVMAliasInfo.h>
#include <phasar/PhasarLLVM/Utils/LLVMShorthands.h>

#include <set>
#include <cassert>
#include <map>

// Вспомогательные функции для анализа алиасов
static void analyzeGlobalAliases(llvm::Module* M);
static void analyzePointerRelationships(llvm::Function& F);
static void analyzeMemoryAccesses(llvm::Function& F);
static void detectPotentialAliases(llvm::Function& F);
static string getValueDescription(const llvm::Value* V);
static string getShortValueName(const llvm::Value* V);

static void runMyRealizedAnalysis(LLVMProjectIRDB& IRDB, const Options& opts) {
    cout << "\n=== АНАЛИЗ АЛИАСОВ (ALIAS ANALYSIS) ===\n";
    
    try {
        auto* M = IRDB.getModule();
        
        if (!M) {
            cerr << "Error: Не удалось получить модуль из IRDB.\n";
            return;
        }
        
        // Анализ глобальных алиасов
        analyzeGlobalAliases(M);
        
        // Анализ алиасов для каждой функции
        cout << "\n=== АНАЛИЗ АЛИАСОВ ПО ФУНКЦИЯМ ===\n";
        
        for (auto& F : *M) {
            if (F.isDeclaration()) {
                continue;
            }
            
            // Пропускаем приватные/системные функции
            string funcName = F.getName().str();
            if (opts.shouldSkipFunction(funcName)) {
                continue;
            }
            
            // Собираем все указатели в функции
            vector<const llvm::Value*> pointers;
            
            for (auto& arg : F.args()) {
                if (arg.getType()->isPointerTy()) {
                    pointers.push_back(&arg);
                }
            }
            
            for (auto& BB : F) {
                for (auto& I : BB) {
                    if (I.getType()->isPointerTy()) {
                        pointers.push_back(&I);
                    }
                }
            }
            
            if (pointers.empty()) {
                continue;
            }
            
            cout << "\n" << funcName << " (" << pointers.size() << " указателей):\n";
            
            // Анализируем отношения между указателями
            analyzePointerRelationships(F);
            
            // Обнаруживаем потенциальные алиасы
            detectPotentialAliases(F);
            
            // Анализируем обращения к памяти
            analyzeMemoryAccesses(F);
        }
        
    } catch (const exception& e) {
        cerr << "Error: Ошибка при анализе алиасов: " << e.what() << "\n";
    }
}

static void runPhasarAnalysis(LLVMProjectIRDB& IRDB, const Options& opts) {
    cout << "\n=== АНАЛИЗ АЛИАСОВ С ИСПОЛЬЗОВАНИЕМ PhASAR (PhASAR ALIAS ANALYSIS) ===\n";
    
    try {
        auto* M = IRDB.getModule();
        
        if (!M) {
            cerr << "Error: Не удалось получить модуль из IRDB.\n";
            return;
        }
        
        LLVMAliasSet AS(&IRDB, false);
        
        for (auto& F : *M) {
            if (F.isDeclaration()) {
                continue;
            }

            // Пропускаем приватные/системные функции
            std::string funcName = F.getName().str();
            if (opts.shouldSkipFunction(funcName)) {
                continue;
            }
            
            cout << "\nФункция: " << F.getName().str() << "\n";
            
            // Собираем информацию о всех указателях
            std::map<const llvm::Value*, std::vector<const llvm::Value*>> aliasGroups;
            std::map<const llvm::Value*, std::vector<const llvm::Value*>> allocSites;
            std::vector<const llvm::Instruction*> allPointers;
            
            for (const auto &Inst: llvm::instructions(F)) {
                if (!Inst.getType()->isPointerTy()) {
                    continue;
                }
                
                allPointers.push_back(&Inst);
                
                auto AliasesOfInstAtInst = AS.getAliasSet(&Inst, &Inst);
                if (AliasesOfInstAtInst && !AliasesOfInstAtInst->empty()) {
                    // Фильтруем: не показываем, если алиас только на себя
                    if (AliasesOfInstAtInst->size() > 1 || 
                        (AliasesOfInstAtInst->size() == 1 && *AliasesOfInstAtInst->begin() != &Inst)) {
                        std::vector<const llvm::Value*> vec(AliasesOfInstAtInst->begin(), AliasesOfInstAtInst->end());
                        aliasGroups[&Inst] = vec;
                    }
                }
                
                auto ReachableAllocSites = AS.getReachableAllocationSites(&Inst, true, &Inst);
                if (ReachableAllocSites && !ReachableAllocSites->empty()) {
                    // Фильтруем: не показываем, если указывает только на себя
                    if (ReachableAllocSites->size() > 1 || 
                        (ReachableAllocSites->size() == 1 && *ReachableAllocSites->begin() != &Inst)) {
                        std::vector<const llvm::Value*> vec(ReachableAllocSites->begin(), ReachableAllocSites->end());
                        allocSites[&Inst] = vec;
                    }
                }
            }
            
            if (allPointers.empty()) {
                cout << "  (нет указателей для анализа)\n";
                continue;
            }
            
            cout << "  Указателей: " << allPointers.size() << "\n";
            
            // Показываем только интересные алиасы
            if (!aliasGroups.empty()) {
                cout << "  Обнаруженные алиасы:\n";
                
                int shownAliases = 0;
                for (const auto& [ptr, aliases] : aliasGroups) {
                    // Показываем только если есть реальные алиасы (не только сам указатель)
                    std::vector<const llvm::Value*> realAliases;
                    for (const auto* alias : aliases) {
                        if (alias != ptr) {
                            realAliases.push_back(alias);
                        }
                    }
                    
                    if (!realAliases.empty() && shownAliases < 8) {
                        cout << "     • " << getShortValueName(ptr) << " ≡ ";
                        bool first = true;
                        int count = 0;
                        for (const auto* alias : realAliases) {
                            if (count >= 3) {
                                cout << "... (+" << (realAliases.size() - 3) << ")";
                                break;
                            }
                            if (!first) cout << ", ";
                            cout << getShortValueName(alias);
                            first = false;
                            count++;
                        }
                        cout << "\n";
                        shownAliases++;
                    }
                }
                
                if (shownAliases >= 8 && aliasGroups.size() > 8) {
                    cout << "  ... (еще " << (aliasGroups.size() - 8) << " указателей с алиасами)\n";
                }
            } else {
                cout << "  (явных алиасов не обнаружено)\n";
            }
        }

    } catch (const exception& e) {
        cerr << "Error: Ошибка при анализе алиасов: " << e.what() << "\n";
    }
}

void runAliasAnalysis(LLVMProjectIRDB& IRDB, const Options& opts) {
    if (opts.choice == AnalysisChoice::MyRealizedAnalysis || opts.choice == AnalysisChoice::Both) {
        runMyRealizedAnalysis(IRDB, opts);
    }
    
    if (opts.choice == AnalysisChoice::PhasarAnalysis || opts.choice == AnalysisChoice::Both) {
        runPhasarAnalysis(IRDB, opts);
    }
}

static string getValueDescription(const llvm::Value* V) {
    if (!V) return "[null]";
    
    if (V->hasName()) {
        return V->getName().str();
    }
    
    string desc;
    llvm::raw_string_ostream os(desc);
    V->print(os);
    
    // Ограничиваем длину
    if (desc.length() > 60) {
        desc = desc.substr(0, 57) + "...";
    }
    
    return desc;
}

// Короткое описание значения для вывода
static string getShortValueName(const llvm::Value* V) {
    if (!V) return "[null]";
    
    if (V->hasName() && !V->getName().empty()) {
        return "%" + V->getName().str();
    }
    
    // Для инструкций без имени используем короткую форму
    if (auto* I = llvm::dyn_cast<llvm::Instruction>(V)) {
        string opName;
        if (llvm::isa<llvm::AllocaInst>(I)) opName = "alloca";
        else if (llvm::isa<llvm::LoadInst>(I)) opName = "load";
        else if (llvm::isa<llvm::StoreInst>(I)) opName = "store";
        else if (llvm::isa<llvm::GetElementPtrInst>(I)) opName = "gep";
        else if (llvm::isa<llvm::CallInst>(I)) opName = "call";
        else opName = I->getOpcodeName();
        
        string desc;
        llvm::raw_string_ostream os(desc);
        I->printAsOperand(os, false);
        
        if (!desc.empty()) {
            return desc + " (" + opName + ")";
        }
        return "<" + opName + ">";
    }
    
    // Для аргументов функций
    if (auto* A = llvm::dyn_cast<llvm::Argument>(V)) {
        return "arg" + std::to_string(A->getArgNo());
    }
    
    return "<unnamed>";
}

static void analyzeGlobalAliases(llvm::Module* M) {
    cout << "\nГЛОБАЛЬНЫЕ АЛИАСЫ:\n";
    
    vector<llvm::GlobalVariable*> globalPointers;
    
    for (auto& GV : M->getGlobalList()) {
        if (GV.getValueType()->isPointerTy()) {
            globalPointers.push_back(&GV);
        }
    }
    
    vector<llvm::GlobalAlias*> aliases;
    for (auto& GA : M->getAliasList()) {
        aliases.push_back(&GA);
    }
    
    if (globalPointers.empty() && aliases.empty()) {
        cout << "  Глобальных указателей и алиасов не найдено\n";
        return;
    }
    
    if (!globalPointers.empty()) {
        cout << "Найдено глобальных указателей: " << globalPointers.size() << "\n";
        
        for (auto* GV : globalPointers) {
            cout << "  " << GV->getName().str();
            
            if (GV->hasInitializer()) {
                auto* init = GV->getInitializer();
                if (auto* GVInit = llvm::dyn_cast<llvm::GlobalVariable>(init)) {
                    cout << " -> " << GVInit->getName().str();
                }
            }
            cout << "\n";
        }
    }
    
    if (!aliases.empty()) {
        cout << "\nГлобальные алиасы (LLVM GlobalAlias): " << aliases.size() << "\n";
        for (auto* GA : aliases) {
            cout << "  " << GA->getName().str();
            auto* aliasee = GA->getAliasee();
            if (aliasee && aliasee->hasName()) {
                cout << " -> " << aliasee->getName().str();
            }
            cout << "\n";
        }
    }
}

static void analyzePointerRelationships(llvm::Function& F) {
    map<const llvm::Value*, set<const llvm::Value*>> pointsTo;
    
    for (auto& BB : F) {
        for (auto& I : BB) {
            if (auto* SI = llvm::dyn_cast<llvm::StoreInst>(&I)) {
                auto* val = SI->getValueOperand();
                auto* ptr = SI->getPointerOperand();
                
                if (val->getType()->isPointerTy()) {
                    pointsTo[ptr].insert(val);
                }
            }
            else if (auto* LI = llvm::dyn_cast<llvm::LoadInst>(&I)) {
                if (LI->getType()->isPointerTy()) {
                    auto* src = LI->getPointerOperand();
                    pointsTo[&I].insert(src);
                }
            }
            else if (auto* GEP = llvm::dyn_cast<llvm::GetElementPtrInst>(&I)) {
                auto* base = GEP->getPointerOperand();
                pointsTo[&I].insert(base);
            }
            else if (auto* BC = llvm::dyn_cast<llvm::BitCastInst>(&I)) {
                auto* src = BC->getOperand(0);
                if (src->getType()->isPointerTy()) {
                    pointsTo[&I].insert(src);
                }
            }
            else if (auto* PHI = llvm::dyn_cast<llvm::PHINode>(&I)) {
                if (PHI->getType()->isPointerTy()) {
                    for (unsigned i = 0; i < PHI->getNumIncomingValues(); ++i) {
                        auto* incoming = PHI->getIncomingValue(i);
                        pointsTo[PHI].insert(incoming);
                    }
                }
            }
        }
    }
    
    if (!pointsTo.empty()) {
        cout << "  [Отношения указателей]\n";
        int shown = 0;
        for (const auto& [ptr, targets] : pointsTo) {
            if (shown >= 5) {
                cout << "    ... (еще " << (pointsTo.size() - 5) << " отношений)\n";
                break;
            }
            cout << "    " << getShortValueName(ptr) << " -> ";
            bool first = true;
            int tcount = 0;
            for (const auto* target : targets) {
                if (tcount >= 2) {
                    cout << "... (еще " << (targets.size() - 2) << ")";
                    break;
                }
                if (!first) cout << ", ";
                cout << getShortValueName(target);
                first = false;
                tcount++;
            }
            cout << "\n";
            shown++;
        }
    }
}

static void detectPotentialAliases(llvm::Function& F) {
    struct PointerInfo {
        const llvm::Value* pointer;
        const llvm::Value* source;
        string context;
    };
    
    vector<PointerInfo> pointerInfos;
    
    for (auto& BB : F) {
        for (auto& I : BB) {
            if (auto* SI = llvm::dyn_cast<llvm::StoreInst>(&I)) {
                auto* val = SI->getValueOperand();
                auto* ptr = SI->getPointerOperand();
                
                if (val->getType()->isPointerTy()) {
                    PointerInfo info;
                    info.pointer = ptr;
                    info.source = val;
                    info.context = "store";
                    pointerInfos.push_back(info);
                }
            }
            
            if (auto* CI = llvm::dyn_cast<llvm::CallInst>(&I)) {
                auto* called = CI->getCalledFunction();
                if (called && !called->getName().empty()) {
                    vector<const llvm::Value*> ptrArgs;
                    
                    for (unsigned i = 0; i < CI->arg_size(); ++i) {
                        auto* arg = CI->getArgOperand(i);
                        if (arg->getType()->isPointerTy()) {
                            ptrArgs.push_back(arg);
                        }
                    }
                    
                    if (ptrArgs.size() >= 2) {
                        for (size_t i = 0; i < ptrArgs.size(); ++i) {
                            for (size_t j = i + 1; j < ptrArgs.size(); ++j) {
                                if (ptrArgs[i] == ptrArgs[j]) {
                                    cout << "  Вызов " << called->getName().str() 
                                         << "(): аргументы " << i << "==" << j << "\n";
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    
    int foundAliases = 0;
    bool headerShown = false;
    for (size_t i = 0; i < pointerInfos.size(); ++i) {
        for (size_t j = i + 1; j < pointerInfos.size(); ++j) {
            if (pointerInfos[i].source == pointerInfos[j].source) {
                if (!headerShown) {
                    cout << "  [Потенциальные алиасы]\n";
                    headerShown = true;
                }
                if (foundAliases < 3) {
                    cout << "    " << getShortValueName(pointerInfos[i].pointer)
                         << " == " << getShortValueName(pointerInfos[j].pointer)
                         << " (источник: " << getShortValueName(pointerInfos[i].source) << ")\n";
                }
                foundAliases++;
            }
        }
    }
    
    if (foundAliases > 3) {
        cout << "    ... (еще " << (foundAliases - 3) << " алиасов)\n";
    }
}

static void analyzeMemoryAccesses(llvm::Function& F) {
    struct MemoryAccess {
        const llvm::Value* pointer;
        bool isLoad;
        bool isStore;
    };
    
    vector<MemoryAccess> accesses;
    
    for (auto& BB : F) {
        for (auto& I : BB) {
            if (auto* load = llvm::dyn_cast<llvm::LoadInst>(&I)) {
                MemoryAccess access;
                access.pointer = load->getPointerOperand();
                access.isLoad = true;
                access.isStore = false;
                accesses.push_back(access);
            } else if (auto* store = llvm::dyn_cast<llvm::StoreInst>(&I)) {
                MemoryAccess access;
                access.pointer = store->getPointerOperand();
                access.isLoad = false;
                access.isStore = true;
                accesses.push_back(access);
            }
        }
    }
    
    if (accesses.empty()) {
        return;
    }
    
    size_t loads = 0, stores = 0;
    for (const auto& acc : accesses) {
        if (acc.isLoad) loads++;
        if (acc.isStore) stores++;
    }
    
    int conflicts = 0;
    for (size_t i = 0; i < accesses.size(); ++i) {
        for (size_t j = i + 1; j < accesses.size(); ++j) {
            auto& acc1 = accesses[i];
            auto& acc2 = accesses[j];
            
            if ((acc1.isStore || acc2.isStore) && acc1.pointer == acc2.pointer) {
                conflicts++;
            }
        }
    }
    
    cout << "  [Обращения к памяти]\n";
    cout << "    Чтений (read):  " << loads << "\n";
    cout << "    Записей (write): " << stores << "\n";
    
    if (conflicts > 0) {
        cout << "    Потенциальных конфликтов: " << conflicts << "\n";
    }
}
