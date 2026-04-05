#include "TypeAnalysis.hpp"

#include "phasar/PhasarLLVM/TypeHierarchy/DIBasedTypeHierarchy.h"

static void runMyRealizedAnalysis(LLVMProjectIRDB& IRDB, const Options& opts, HTMLReporter& htmlReporter) {
    std::stringstream htmlOut;
    std::ostream& out = (opts.outputFormat == OutputFormat::HTML) ? static_cast<std::ostream&>(htmlOut) : static_cast<std::ostream&>(std::cout);
    
    out << "\n=== TYPE ANALYSIS ===\n";
    
    try {
        int castCount = 0;
        
        auto* M = IRDB.getModule();

        if (!M) {
            cerr << "Error: Не удалось получить модуль из IRDB.\n";
            if (opts.outputFormat == OutputFormat::HTML) {
                htmlReporter.addError("Не удалось получить модуль из IRDB (анализ типов).");
            }
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

        out << "\nСтатистика анализа типов:\n";
        out << "  Всего приведений типов: " << castCount << "\n";

        if (opts.outputFormat == OutputFormat::HTML) {
            htmlReporter.addSection("Анализ типов (Basic)", "<pre>" + htmlOut.str() + "</pre>");
        }
    } catch (const exception& e) {
        cerr << "Ошибка при выполнении type analysis: " << e.what() << "\n";
        if (opts.outputFormat == OutputFormat::HTML) {
            htmlReporter.addError("Ошибка при выполнении type analysis: " + string(e.what()));
        }
    }
}

static void runPhasarAnalysis(LLVMProjectIRDB& IRDB, const Options& opts, HTMLReporter& htmlReporter) {
    std::stringstream htmlOut;
    std::ostream& out = (opts.outputFormat == OutputFormat::HTML) ? static_cast<std::ostream&>(htmlOut) : static_cast<std::ostream&>(std::cout);

    out << "\n=== PHASAR ANALYSIS ===\n";

    DIBasedTypeHierarchy TH(IRDB);

    out << "\nPhASAR статистика по типам:\n";
    
    for (const auto& Ty : TH.getAllTypes()) {
        std::string typeName = Ty->getName().str();
        if (typeName.empty() || typeName[0] == '_' ) {
            continue;
        }

        out << "Тип: " << typeName << " (" << TH.getTypeName(Ty).str() << ")\n";
        auto SubTypes = TH.getSubTypes(Ty);

        bool hasSubTypes = false;
        for (const auto& SubTy : SubTypes) {
            // Пропускаем сам тип
            if (SubTy == Ty) {
                continue;
            }
            
            if (!hasSubTypes) {
                out << "  Подтипы:\n";
                hasSubTypes = true;
            }
            
            out << "    - " << SubTy->getName().str() << " (" << TH.getTypeName(SubTy).str() << ")\n";
        }
        
        if (!hasSubTypes) {
            out << "  Нет подтипов.\n";
        }
    }

    if (opts.outputFormat == OutputFormat::HTML) {
        htmlReporter.addSection("Анализ типов (Detailed)", "<pre>" + htmlOut.str() + "</pre>");
    }
}

void runTypeAnalysis(LLVMProjectIRDB& IRDB, const Options& opts, HTMLReporter& htmlReporter) {
    if (opts.choice == AnalysisChoice::BasicAnalysis || opts.choice == AnalysisChoice::Both) {
        runMyRealizedAnalysis(IRDB, opts, htmlReporter);
    }
    
    if (opts.choice == AnalysisChoice::DetailedAnalysis || opts.choice == AnalysisChoice::Both) {
        runPhasarAnalysis(IRDB, opts, htmlReporter);
    }
}
