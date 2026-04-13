#include "BasicInfo.hpp"
#include "html_reporter.hpp"

#include <iostream>
#include <phasar/PhasarLLVM/Passes/GeneralStatisticsAnalysis.h>
#include <vector>

void analyzeModuleMetadata(llvm::Module* M, std::ostream& out);
void analyzeFunctionDetails(llvm::Function& F, std::ostream& out);
void analyzeGlobalVariables(llvm::Module* M, const Options& opts, std::ostream& out);

void reportNonConstantGlobalVariables(const vector<string>& nonConstGlobals, const Options& opts);

static void runMyRealizedAnalysis(LLVMProjectIRDB& IRDB, const Options& opts, HTMLReporter& htmlReporter) {
    std::stringstream htmlOut;
    std::ostream& out = (opts.outputFormat == OutputFormat::HTML) ? static_cast<std::ostream&>(htmlOut) : static_cast<std::ostream&>(std::cout);

    if (!opts.useErrorCheckingAlgorithm) {
        out << "\n=== БАЗОВАЯ ИНФОРМАЦИЯ ОБ IR ===\n";
    }

    try {
        auto* M = IRDB.getModule();

        if (!M) {
            std::cerr << "Error: Не удалось получить модуль из IRDB.\n";
            if (opts.outputFormat == OutputFormat::HTML) {
                htmlReporter.addError("Не удалось получить модуль LLVM IR (анализ базовой информации).");
            }
            return;
        }

        if (opts.useErrorCheckingAlgorithm) {
            analyzeGlobalVariables(M, opts, out);
            return;
        }

        // 1. Основная информация о модуле
        out << "\nОБЩАЯ ИНФОРМАЦИЯ О МОДУЛЕ:\n";
        out << "  Имя модуля: " << M->getName().str() << "\n";
        out << "  Target Triple: " << M->getTargetTriple() << "\n";
        out << "  Data Layout: " << M->getDataLayoutStr() << "\n";
        
        // 2. Статистика
        out << "\nСТАТИСТИКА:\n";
        out << "  Функций всего: " << M->getFunctionList().size() << "\n";
        out << "  Глобальных переменных: " << M->getGlobalList().size() << "\n";
        out << "  Глобальных алиасов: " << M->getAliasList().size() << "\n";
        
        size_t totalBB = 0, totalInstr = 0, definedFunctions = 0;
        for (auto& F : *M) {
            // Пропускаем внешние функции и функции, начинающиеся с _
            if (F.isDeclaration()) {
                continue;
            }
            
            string funcName = F.getName().str();
            if (opts.shouldSkipFunction(funcName)) {
                continue;
            }
            
            definedFunctions++;
            totalBB += F.size();
            totalInstr += F.getInstructionCount();
        }
        
        out << "  Определенных функций (без внешних и приватных): " << definedFunctions << "\n";
        out << "  Всего базовых блоков: " << totalBB << "\n";
        out << "  Всего инструкций: " << totalInstr << "\n";

        // if (definedFunctions > 0) {
        //     out << "  Среднее блоков на функцию: " << (double)totalBB / definedFunctions << "\n";
        //     out << "  Среднее инструкций на функцию: " << (double)totalInstr / definedFunctions << "\n";
        // }

        // 3. Анализ метаданных
        analyzeModuleMetadata(M, out);
        
        // 4. Детальный анализ функций (без внешних и приватных)
        out << "\nДЕТАЛЬНЫЙ АНАЛИЗ ФУНКЦИЙ (без внешних и приватных):\n";
        for (auto& F : *M) {
            if (F.isDeclaration()) {
                continue;
            }
            
            string funcName = F.getName().str();
            if (opts.shouldSkipFunction(funcName)) {
                continue;
            }
            
            analyzeFunctionDetails(F, out);
        }
        
        // 5. Анализ глобальных переменных
        analyzeGlobalVariables(M, opts, out);
        
        if (opts.outputFormat == OutputFormat::HTML) {
            htmlReporter.addSection("Статистика IR (Basic)", "<pre>" + htmlOut.str() + "</pre>");
        }
        
    } catch (const exception& e) {
        std::cerr << "Error: Ошибка при получении базовой информации: " << e.what() << "\n";
        if (opts.outputFormat == OutputFormat::HTML) {
            htmlReporter.addError("Ошибка при получении базовой информации: " + string(e.what()));
        }
    }
}

static void runPhasarAnalysis(LLVMProjectIRDB& IRDB, const Options& opts, HTMLReporter& htmlReporter) {
    std::stringstream htmlOut;
    std::ostream& out = (opts.outputFormat == OutputFormat::HTML) ? static_cast<std::ostream&>(htmlOut) : static_cast<std::ostream&>(std::cout);

    out << "\n=== АНАЛИЗ БАЗОВОЙ ИНФОРМАЦИИ С ИСПОЛЬЗОВАНИЕМ PhASAR ===\n";

    try {
        auto* M = IRDB.getModule();

        if (!M) {
            std::cerr << "Error: Не удалось получить модуль из IRDB.\n";
            if (opts.outputFormat == OutputFormat::HTML) {
                htmlReporter.addError("Не удалось получить модуль LLVM IR (анализ базовой информации).");
            }
            return;
        }

        GeneralStatisticsAnalysis stats;
        auto result = stats.runOnModule(*M);

        out << "\nСтатистика, собранная PhASAR:\n";
        out << "  Имя модуля: " << result.ModuleName << "\n\n";
        
        out << "ФУНКЦИИ:\n";
        out << "  Всего функций: " << result.Functions << "\n";
        out << "  Внешних функций: " << result.ExternalFunctions << "\n";
        out << "  Определений функций: " << result.FunctionDefinitions << "\n";
        out << "  Функций с взятым адресом: " << result.AddressTakenFunctions << "\n\n";
        
        out << "ГЛОБАЛЬНЫЕ ПЕРЕМЕННЫЕ:\n";
        out << "  Всего глобальных: " << result.Globals << "\n";
        out << "  Глобальных констант: " << result.GlobalConsts << "\n";
        out << "  Внешних глобальных: " << result.ExternalGlobals << "\n";
        out << "  Определений глобальных: " << result.GlobalsDefinitions << "\n\n";
        
        out << "БАЗОВЫЕ БЛОКИ И ИНСТРУКЦИИ:\n";
        out << "  Базовых блоков: " << result.BasicBlocks << "\n";
        out << "  Всего инструкций: " << result.Instructions << "\n";
        out << "  Инструкций store: " << result.StoreInstructions << "\n";
        out << "  Инструкций load: " << result.LoadInstructions << "\n";
        out << "  Non-void инструкций: " << result.NonVoidInsts << "\n\n";
        
        out << "ВЫЗОВЫ И АЛЛОКАЦИИ:\n";
        out << "  Мест выделения памяти: " << result.AllocationSites << "\n";
        out << "  Мест вызовов: " << result.CallSites << "\n";
        out << "  Косвенных вызовов: " << result.IndCalls << "\n";
        out << "  Inline asm: " << result.NumInlineAsm << "\n\n";
        
        out << "УПРАВЛЯЮЩИЕ ИНСТРУКЦИИ:\n";
        out << "  Ветвлений: " << result.Branches << "\n";
        out << "  Switch: " << result.Switches << "\n";
        out << "  Landing pads: " << result.LandingPads << "\n";
        out << "  Phi nodes: " << result.PhiNodes << "\n\n";
        
        out << "СПЕЦИАЛЬНЫЕ ИНСТРУКЦИИ:\n";
        out << "  GetElementPtr: " << result.GetElementPtrs << "\n";
        out << "  Отладочных интринсиков: " << result.DebugIntrinsics << "\n";
        out << "  Интринсиков памяти: " << result.MemIntrinsics << "\n\n";
        
        out << "СТАТИСТИКА ИСПОЛЬЗОВАНИЯ:\n";
        out << "  Всего операндов: " << result.TotalNumOperands << "\n";
        out << "  Всего использований: " << result.TotalNumUses << "\n";
        out << "  Инструкций с множественным использованием: " << result.NumInstWithMultipleUses << "\n";
        out << "  Инструкций, используемых вне BB: " << result.NumInstsUsedOutsideBB << "\n\n";
        
        out << "МАКСИМАЛЬНЫЕ ЗНАЧЕНИЯ:\n";
        out << "  Макс. операндов: " << result.MaxNumOperands << "\n";
        out << "  Макс. использований: " << result.MaxNumUses << "\n";
        out << "  Макс. предшественников BB: " << result.MaxNumPredecessorBBs << "\n";
        out << "  Макс. последователей BB: " << result.MaxNumSuccessorBBs << "\n\n";
        
        // out << "СРЕДНИЕ ЗНАЧЕНИЯ:\n";
        // if (result.BasicBlocks > 0) {
        //     out << "  Средн. предшественников BB: " 
        //          << (double)result.TotalNumPredecessorBBs / result.BasicBlocks << "\n";
        //     out << "  Средн. последователей BB: " 
        //          << (double)result.TotalNumSuccessorBBs / result.BasicBlocks << "\n";
        // }
        // if (result.Instructions > 0) {
        //     out << "  Средн. операндов на инструкцию: " 
        //          << (double)result.TotalNumOperands / result.Instructions << "\n";
        //     out << "  Средн. использований на инструкцию: " 
        //          << (double)result.TotalNumUses / result.Instructions << "\n";
        // }
        
        out << "ВЫДЕЛЕННЫЕ ТИПЫ:\n";
        out << "  Количество выделенных типов: " << result.AllocatedTypes.size() << "\n";
        
        // Можно также использовать встроенный оператор вывода:
        // out << "\n--- Полный вывод через operator<< ---\n";
        // llvm::outs() << result << "\n"; // llvm::outs() is hard to redirect to std::ostream generically without custom raw_ostream, leaving it commented out.
        
        // Или JSON формат:
        // out << "\n--- JSON формат ---\n";
        // result.printAsJson(llvm::outs());

        if (opts.outputFormat == OutputFormat::HTML) {
            htmlReporter.addSection("Статистика IR (Detailed)", "<pre>" + htmlOut.str() + "</pre>");
        }
        
    } catch (const exception& e) {
        std::cerr << "Error: Ошибка при анализе базовой информации Detailed: " << e.what() << "\n";
        if (opts.outputFormat == OutputFormat::HTML) {
            htmlReporter.addError("Ошибка при анализе базовой информации Detailed: " + string(e.what()));
        }
    }
}

void runBasicInfo(LLVMProjectIRDB& IRDB, const Options& opts, HTMLReporter& htmlReporter) {
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

void analyzeModuleMetadata(llvm::Module* M, std::ostream& out) {
    out << "\nМЕТАДАННЫЕ МОДУЛЯ:\n";
    
    // Анализ named metadata
    auto& namedMD = M->getNamedMDList();
    if (namedMD.empty()) {
        out << "  Именованных метаданных не найдено\n";
    } else {
        out << "  Именованные метаданные (" << namedMD.size() << "):\n";
        for (auto& NMD : namedMD) {
            out << "    - " << NMD.getName().str() 
                 << " (операндов: " << NMD.getNumOperands() << ")\n";
        }
    }
    
    // Проверяем debug info
    if (M->getNamedMetadata("llvm.dbg.cu")) {
        out << "  Debug информация присутствует\n";
    } else {
        out << "  Debug информация отсутствует\n";
    }
}

void analyzeFunctionDetails(llvm::Function& F, std::ostream& out) {
    out << "  " << F.getName().str() << ":\n";
    out << "    Блоков: " << F.size() << ", Инструкций: " << F.getInstructionCount() << "\n";
    out << "    Параметров: " << F.arg_size() << "\n";
    out << "    Linkage: ";

    switch (F.getLinkage()) {
        case llvm::GlobalValue::ExternalLinkage: out << "External"; break;
        case llvm::GlobalValue::InternalLinkage: out << "Internal"; break;
        case llvm::GlobalValue::PrivateLinkage: out << "Private"; break;
        default: out << "Other"; break;
    }
    out << "\n";
    
    // out << "    Свойства функции: ";
    // bool hasProperties = false;
    
    // if (F.doesNotThrow()) {
    //     out << "[nothrow] ";
    //     hasProperties = true;
    // }
    // if (F.hasInternalLinkage()) {
    //     out << "[internal] ";
    //     hasProperties = true;
    // }
    // if (F.isVarArg()) {
    //     out << "[vararg] ";
    //     hasProperties = true;
    // }
    // if (F.isDeclaration()) {
    //     out << "[declaration] ";
    //     hasProperties = true;
    // } else {
    //     out << "[defined] ";
    //     hasProperties = true;
    // }
    
    // llvm::CallingConv::ID cc = F.getCallingConv();
    // if (cc != llvm::CallingConv::C) {
    //     out << "[cc:" << cc << "] ";
    //     hasProperties = true;
    // }
    
    // if (F.getVisibility() != llvm::GlobalValue::DefaultVisibility) {
    //     out << "[visibility:";
    //     switch (F.getVisibility()) {
    //         case llvm::GlobalValue::HiddenVisibility: out << "hidden"; break;
    //         case llvm::GlobalValue::ProtectedVisibility: out << "protected"; break;
    //         default: out << "other"; break;
    //     }
    //     out << "] ";
    //     hasProperties = true;
    // }
    
    // if (!hasProperties) {
    //     out << "стандартные свойства";
    // }
    // out << "\n";
    
    // out << "    Атрибуты (строковый анализ): ";
    // string funcStr;
    // llvm::raw_string_ostream funcOS(funcStr);
    // F.print(funcOS);
    // string funcString = funcOS.str();
    
    // bool hasStringAttrs = false;
    // if (funcString.find("nounwind") != string::npos) {
    //     out << "[nounwind] ";
    //     hasStringAttrs = true;
    // }
    // if (funcString.find("readonly") != string::npos) {
    //     out << "[readonly] ";
    //     hasStringAttrs = true;
    // }
    // if (funcString.find("readnone") != string::npos) {
    //     out << "[readnone] ";
    //     hasStringAttrs = true;
    // }
    // if (funcString.find("alwaysinline") != string::npos) {
    //     out << "[alwaysinline] ";
    //     hasStringAttrs = true;
    // }
    // if (funcString.find("noinline") != string::npos) {
    //     out << "[noinline] ";
    //     hasStringAttrs = true;
    // }
    // if (funcString.find("optnone") != string::npos) {
    //     out << "[optnone] ";
    //     hasStringAttrs = true;
    // }
    // if (funcString.find("mustprogress") != string::npos) {
    //     out << "[mustprogress] ";
    //     hasStringAttrs = true;
    // }
    // if (funcString.find("uwtable") != string::npos) {
    //     out << "[uwtable] ";
    //     hasStringAttrs = true;
    // }
    // if (funcString.find("norecurse") != string::npos) {
    //     out << "[norecurse] ";
    //     hasStringAttrs = true;
    // }
    
    // if (!hasStringAttrs) {
    //     out << "нет специальных атрибутов";
    // }
    // out << "\n";
    
    if (F.arg_size() > 0) {
        out << "    Параметры: ";
        bool first = true;
        for (auto& arg : F.args()) {
            if (!first) {
                out << ", ";
            }
            first = false;
            
            string argTypeStr;
            llvm::raw_string_ostream argTypeOS(argTypeStr);
            arg.getType()->print(argTypeOS);
            
            // Упрощаем типы для лучшей читаемости
            string simplifiedType = argTypeOS.str();
            if (simplifiedType == "i32") simplifiedType = "int";
            else if (simplifiedType == "i64") simplifiedType = "long";
            else if (simplifiedType == "i8") simplifiedType = "char";
            else if (simplifiedType == "i1") simplifiedType = "bool";
            else if (simplifiedType == "float") simplifiedType = "float";
            else if (simplifiedType == "double") simplifiedType = "double";
            else if (simplifiedType.find("ptr") != string::npos) {
                // Упрощаем указатели
                simplifiedType = "ptr";
            }
            
            out << simplifiedType;
            if (!arg.getName().empty()) {
                out << " " << arg.getName().str();
            }
        }
        out << "\n";
    }
    
    llvm::Type* retType = F.getReturnType();
    string retTypeStr;
    llvm::raw_string_ostream retTypeOS(retTypeStr);
    retType->print(retTypeOS);
    
    // Упрощаем тип для лучшей читаемости
    string simplifiedRetType = retTypeOS.str();
    if (simplifiedRetType == "i32") simplifiedRetType = "int";
    else if (simplifiedRetType == "i64") simplifiedRetType = "long";
    else if (simplifiedRetType == "i8") simplifiedRetType = "char";
    else if (simplifiedRetType == "i1") simplifiedRetType = "bool";
    else if (simplifiedRetType == "void") simplifiedRetType = "void";
    else if (simplifiedRetType == "float") simplifiedRetType = "float";
    else if (simplifiedRetType == "double") simplifiedRetType = "double";
    else if (simplifiedRetType.find("ptr") != string::npos) {
        simplifiedRetType = "ptr";
    }
    
    out << "    Возвращаемый тип: " << simplifiedRetType << "\n";

    out << "\n";
}

void analyzeGlobalVariables(llvm::Module* M, const Options& opts, std::ostream& out) {
    if (!opts.useErrorCheckingAlgorithm) {
        out << "АНАЛИЗ ГЛОБАЛЬНЫХ ЗНАЧЕНИЙ (без приватных):\n";
    }

    if (M->getGlobalList().empty()) {
        if (!opts.useErrorCheckingAlgorithm) {
            out << "  Глобальных значений не найдено\n";
        }

        return;
    }
    
    size_t visibleCount = 0;
    size_t nonConstCount = 0;
    size_t constCount = 0;
    size_t serviceConstCount = 0;

    std::vector<std::string> nonConstVars;
    
    for (auto& GV : M->getGlobalList()) {
        // Пропускаем приватные глобальные переменные
        string varName = GV.getName().str();
        if (!varName.empty() && varName[0] == '_') {
            continue;
        }
        
        // Определяем служебные переменные (строковые литералы, stderr, stdout, stdin и т.д.)
        bool isService = (varName.find(".str") == 0) || 
                        varName == "stderr" || 
                        varName == "stdout" || 
                        varName == "stdin";
        
        visibleCount++;
        
        if (GV.isConstant()) {
            if (isService) {
                serviceConstCount++;
            } else {
                constCount++;
            }
        } else {
            if (!isService) {
                nonConstCount++;
                nonConstVars.push_back(varName);
            }
        }
        
        if (!opts.useErrorCheckingAlgorithm) {
            out << "  " << varName;
            
            if (isService) {
                out << " [служебная]";
            }
            
            string typeStr;
            llvm::raw_string_ostream typeOS(typeStr);
            GV.getValueType()->print(typeOS);
            out << " (тип: " << typeOS.str() << ")";
            
            if (GV.hasInitializer()) {
                out << " [инициализирована";
                
                // Если это константа, пытаемся показать её значение
                if (GV.isConstant()) {
                    llvm::Constant* initializer = GV.getInitializer();
                    
                    if (auto* CI = llvm::dyn_cast<llvm::ConstantInt>(initializer)) {
                        // Целочисленная константа
                        out << " = " << CI->getSExtValue();
                    } else if (auto* CF = llvm::dyn_cast<llvm::ConstantFP>(initializer)) {
                        // Константа с плавающей точкой
                        out << " = " << CF->getValueAPF().convertToDouble();
                    } else if (auto* CS = llvm::dyn_cast<llvm::ConstantDataSequential>(initializer)) {
                        // Строковая константа или массив
                        if (CS->isString() || CS->isCString()) {
                            string strValue = CS->isString() ? CS->getAsString().str() : CS->getAsCString().str();
                            
                            // Экранируем служебные символы
                            string escapedStr;
                            for (char c : strValue) {
                                switch (c) {
                                    case '\n': escapedStr += "\\n"; break;
                                    case '\r': escapedStr += "\\r"; break;
                                    case '\t': escapedStr += "\\t"; break;
                                    case '\0': escapedStr += "\\0"; break;
                                    case '\\': escapedStr += "\\\\"; break;
                                    case '\"': escapedStr += "\\\""; break;
                                    case '\a': escapedStr += "\\a"; break;
                                    case '\b': escapedStr += "\\b"; break;
                                    case '\f': escapedStr += "\\f"; break;
                                    case '\v': escapedStr += "\\v"; break;
                                    default:
                                        if (c >= 32 && c <= 126) {
                                            escapedStr += c;
                                        } else {
                                            // Для непечатаемых символов используем шестнадцатеричное представление
                                            char buf[5];
                                            snprintf(buf, sizeof(buf), "\\x%02x", (unsigned char)c);
                                            escapedStr += buf;
                                        }
                                        break;
                                }
                            }
                            
                            out << " = \"" << escapedStr << "\"";
                        } else {
                            out << " = [массив из " << CS->getNumElements() << " элементов]";
                        }
                    } else if (llvm::isa<llvm::ConstantAggregateZero>(initializer)) {
                        out << " = 0 (zeroinitializer)";
                    } else if (auto* CA = llvm::dyn_cast<llvm::ConstantArray>(initializer)) {
                        out << " = [массив из " << CA->getNumOperands() << " элементов]";
                    } else if (auto* CS = llvm::dyn_cast<llvm::ConstantStruct>(initializer)) {
                        out << " = {структура из " << CS->getNumOperands() << " полей}";
                    } else {
                        // Для других типов показываем общее представление
                        string valueStr;
                        llvm::raw_string_ostream valueOS(valueStr);
                        initializer->print(valueOS);
                        string value = valueOS.str();
                        // Обрезаем длинные значения
                        if (value.length() > 50) {
                            value = value.substr(0, 47) + "...";
                        }
                        out << " = " << value;
                    }
                }
                
                out << "]";
            } else {
                out << " [не инициализирована]";
            }
            
            if (GV.isConstant() && !GV.hasInitializer()) {
                out << " [константа]";
            }
            
            out << "\n";
        }
    }
    
    if (!opts.useErrorCheckingAlgorithm) {
        if (visibleCount == 0) {
            out << "  Видимых глобальных значений не найдено (все приватные)\n";
        } else {
            out << "\nСТАТИСТИКА ГЛОБАЛЬНЫХ ЗНАЧЕНИЙ:\n";
            out << "  Всего видимых: " << visibleCount << "\n";
            out << "  Служебные константы: " << serviceConstCount << "\n";
            out << "  Пользовательские константы: " << constCount << "\n";
            out << "  Изменяемые: " << nonConstCount << "\n";
            
            if (constCount > 0) {
                out << "\n  РЕКОМЕНДАЦИЯ:\n";
                out << "  Обнаружены глобальные константы (" << constCount << ").\n";
                out << "  Рекомендуется избегать использования глобальных значений, включая константы.\n";
            }
            
            if (nonConstCount > 0) {
                out << "\n  ПРЕДУПРЕЖДЕНИЕ:\n";
                out << "  Обнаружены изменяемые глобальные значения (" << nonConstCount << ")!\n";
            }
        }
    } else {
        reportNonConstantGlobalVariables(nonConstVars, opts);
    }
}

void reportNonConstantGlobalVariables(const vector<string>& nonConstGlobals, const Options& opts) {
    if (nonConstGlobals.empty()) {
        return;
    }

    // Читаем JSON файл из helperFile
    if (opts.helperFile.empty()) {
        std::cerr << "Error: helperFile path is not specified\n";
        return;
    }

    ifstream helperFileStream(opts.helperFile);
    if (!helperFileStream.is_open()) {
        std::cerr << "Error: Could not open helper file '" << opts.helperFile << "'\n";
        return;
    }

    try {
        nlohmann::json helperJson;
        helperFileStream >> helperJson;
        helperFileStream.close();

        // Проверяем наличие ключа "globals"
        if (!helperJson.contains("globals")) {
            std::cerr << "Error: 'globals' key not found in helper file\n";
            return;
        }

        // Извлекаем массив функций
        if (!helperJson["globals"].is_array()) {
            std::cerr << "Error: 'globals' is not an array in helper file\n";
            return;
        }

        for (const auto& global : nonConstGlobals) {
            for (const auto& globalObj : helperJson["globals"]) {
                if (globalObj.is_object() && globalObj.contains("name")) {
                    if (globalObj["name"] == global) {
                        std::string errMsg = "File have non-constant global: " + global;
                        ErrorBase error(ErrorType::SEM13);
                        error.setObjectDetail("global_var", make_unique<global_var>(
                            globalObj["name"],
                            globalObj.value("file", "unknown"),
                            globalObj.value("line", 0)
                        ));
                        ErrorHandler::printError(std::move(errMsg), std::move(error));
                        
                        break;
                    }
                }
            }
        }
    } catch (const nlohmann::json::exception& e) {
        std::cerr << "Error parsing JSON from helper file: " << e.what() << "\n";
        return;
    }
}
