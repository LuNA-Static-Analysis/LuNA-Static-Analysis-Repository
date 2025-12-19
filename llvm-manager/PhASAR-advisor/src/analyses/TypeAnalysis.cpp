#include "TypeAnalysis.hpp"

#include "phasar/PhasarLLVM/TypeHierarchy/DIBasedTypeHierarchy.h"

static void runMyRealizedAnalysis(LLVMProjectIRDB& IRDB, const Options& opts) {
    cout << "\n=== TYPE ANALYSIS ===\n";
    
    try {
        int castCount = 0;
        
        auto* M = IRDB.getModule();

        if (!M) {
            cerr << "Error: Не удалось получить модуль из IRDB.\n";
            return;
        }

        for (auto& F : *M) {
            if (!F.isDeclaration()) {
                for (auto& BB : F) {
                    for (auto& I : BB) {
                        if (auto* CI = llvm::dyn_cast<llvm::CastInst>(&I)) {
                            // cout << "  Приведение типа: " << llvmIRToString(CI) << "\n";
                            castCount++;
                        }
                    }
                }
            }
        }

        cout << "\nСтатистика анализа типов:\n";
        cout << "  Всего приведений типов: " << castCount << "\n";
    } catch (const exception& e) {
        cerr << "Ошибка при выполнении type analysis: " << e.what() << "\n";
    }
}

static void runPhasarAnalysis(LLVMProjectIRDB& IRDB, const Options& opts) {
    cout << "\n=== PHASAR ANALYSIS ===\n";

    DIBasedTypeHierarchy TH(IRDB);

    cout << "\nPhASAR статистика по типам:\n";
    
    for (const auto& Ty : TH.getAllTypes()) {
        std::string typeName = Ty->getName().str();
        if (typeName.empty() || typeName[0] == '_' ) {
            continue;
        }

        llvm::outs() << "Тип: " << typeName << " (" << TH.getTypeName(Ty) << ")\n";
        auto SubTypes = TH.getSubTypes(Ty);

        bool hasSubTypes = false;
        for (const auto& SubTy : SubTypes) {
            // Пропускаем сам тип
            if (SubTy == Ty) {
                continue;
            }
            
            if (!hasSubTypes) {
                llvm::outs() << "  Подтипы:\n";
                hasSubTypes = true;
            }
            
            llvm::outs() << "    - " << SubTy->getName() << " (" << TH.getTypeName(SubTy) << ")\n";
        }
        
        if (!hasSubTypes) {
            llvm::outs() << "  Нет подтипов.\n";
        }
    }
}

void runTypeAnalysis(LLVMProjectIRDB& IRDB, const Options& opts) {
    if (opts.choice == AnalysisChoice::MyRealizedAnalysis || opts.choice == AnalysisChoice::Both) {
        runMyRealizedAnalysis(IRDB, opts);
    }
    
    if (opts.choice == AnalysisChoice::PhasarAnalysis || opts.choice == AnalysisChoice::Both) {
        runPhasarAnalysis(IRDB, opts);
    }
}
