#include "BasicInfo.hpp"

#include <phasar/PhasarLLVM/Passes/GeneralStatisticsAnalysis.h>
#include <vector>

void analyzeModuleMetadata(llvm::Module* M);
void analyzeFunctionDetails(llvm::Function& F);
void analyzeGlobalVariables(llvm::Module* M, const Options& opts);
void reportNonConstantGlobalVariables(const vector<string>& nonConstGlobals, const Options& opts);

static void runMyRealizedAnalysis(LLVMProjectIRDB& IRDB, const Options& opts) {
    if (!opts.useErrorCheckingAlgorithm) {
        cout << "\n=== БАЗОВАЯ ИНФОРМАЦИЯ ОБ IR ===\n";
    }

    try {
        auto* M = IRDB.getModule();

        if (!M) {
            cerr << "Error: Не удалось получить модуль из IRDB.\n";
            return;
        }

        if (opts.useErrorCheckingAlgorithm) {
            analyzeGlobalVariables(M, opts);
            return;
        }

        // 1. Основная информация о модуле
        cout << "\nОБЩАЯ ИНФОРМАЦИЯ О МОДУЛЕ:\n";
        cout << "  Имя модуля: " << M->getName().str() << "\n";
        cout << "  Target Triple: " << M->getTargetTriple() << "\n";
        cout << "  Data Layout: " << M->getDataLayoutStr() << "\n";
        
        // 2. Статистика
        cout << "\nСТАТИСТИКА:\n";
        cout << "  Функций всего: " << M->getFunctionList().size() << "\n";
        cout << "  Глобальных переменных: " << M->getGlobalList().size() << "\n";
        cout << "  Глобальных алиасов: " << M->getAliasList().size() << "\n";
        
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
        
        cout << "  Определенных функций (без внешних и приватных): " << definedFunctions << "\n";
        cout << "  Всего базовых блоков: " << totalBB << "\n";
        cout << "  Всего инструкций: " << totalInstr << "\n";

        // if (definedFunctions > 0) {
        //     cout << "  Среднее блоков на функцию: " << (double)totalBB / definedFunctions << "\n";
        //     cout << "  Среднее инструкций на функцию: " << (double)totalInstr / definedFunctions << "\n";
        // }

        // 3. Анализ метаданных
        analyzeModuleMetadata(M);
        
        // 4. Детальный анализ функций (без внешних и приватных)
        cout << "\nДЕТАЛЬНЫЙ АНАЛИЗ ФУНКЦИЙ (без внешних и приватных):\n";
        for (auto& F : *M) {
            if (F.isDeclaration()) {
                continue;
            }
            
            string funcName = F.getName().str();
            if (opts.shouldSkipFunction(funcName)) {
                continue;
            }
            
            analyzeFunctionDetails(F);
        }
        
        // 5. Анализ глобальных переменных
        analyzeGlobalVariables(M, opts);
    } catch (const exception& e) {
        cerr << "Error: Ошибка при получении базовой информации: " << e.what() << "\n";
    }
}

static void runPhasarAnalysis(LLVMProjectIRDB& IRDB, const Options& opts) {
    cout << "\n=== АНАЛИЗ БАЗОВОЙ ИНФОРМАЦИИ С ИСПОЛЬЗОВАНИЕМ PhASAR ===\n";

    try {
        auto* M = IRDB.getModule();

        if (!M) {
            cerr << "Error: Не удалось получить модуль из IRDB.\n";
            return;
        }

        GeneralStatisticsAnalysis stats;
        auto result = stats.runOnModule(*M);

        cout << "\nСтатистика, собранная PhASAR:\n";
        cout << "  Имя модуля: " << result.ModuleName << "\n\n";
        
        cout << "ФУНКЦИИ:\n";
        cout << "  Всего функций: " << result.Functions << "\n";
        cout << "  Внешних функций: " << result.ExternalFunctions << "\n";
        cout << "  Определений функций: " << result.FunctionDefinitions << "\n";
        cout << "  Функций с взятым адресом: " << result.AddressTakenFunctions << "\n\n";
        
        cout << "ГЛОБАЛЬНЫЕ ПЕРЕМЕННЫЕ:\n";
        cout << "  Всего глобальных: " << result.Globals << "\n";
        cout << "  Глобальных констант: " << result.GlobalConsts << "\n";
        cout << "  Внешних глобальных: " << result.ExternalGlobals << "\n";
        cout << "  Определений глобальных: " << result.GlobalsDefinitions << "\n\n";
        
        cout << "БАЗОВЫЕ БЛОКИ И ИНСТРУКЦИИ:\n";
        cout << "  Базовых блоков: " << result.BasicBlocks << "\n";
        cout << "  Всего инструкций: " << result.Instructions << "\n";
        cout << "  Инструкций store: " << result.StoreInstructions << "\n";
        cout << "  Инструкций load: " << result.LoadInstructions << "\n";
        cout << "  Non-void инструкций: " << result.NonVoidInsts << "\n\n";
        
        cout << "ВЫЗОВЫ И АЛЛОКАЦИИ:\n";
        cout << "  Мест выделения памяти: " << result.AllocationSites << "\n";
        cout << "  Мест вызовов: " << result.CallSites << "\n";
        cout << "  Косвенных вызовов: " << result.IndCalls << "\n";
        cout << "  Inline asm: " << result.NumInlineAsm << "\n\n";
        
        cout << "УПРАВЛЯЮЩИЕ ИНСТРУКЦИИ:\n";
        cout << "  Ветвлений: " << result.Branches << "\n";
        cout << "  Switch: " << result.Switches << "\n";
        cout << "  Landing pads: " << result.LandingPads << "\n";
        cout << "  Phi nodes: " << result.PhiNodes << "\n\n";
        
        cout << "СПЕЦИАЛЬНЫЕ ИНСТРУКЦИИ:\n";
        cout << "  GetElementPtr: " << result.GetElementPtrs << "\n";
        cout << "  Отладочных интринсиков: " << result.DebugIntrinsics << "\n";
        cout << "  Интринсиков памяти: " << result.MemIntrinsics << "\n\n";
        
        cout << "СТАТИСТИКА ИСПОЛЬЗОВАНИЯ:\n";
        cout << "  Всего операндов: " << result.TotalNumOperands << "\n";
        cout << "  Всего использований: " << result.TotalNumUses << "\n";
        cout << "  Инструкций с множественным использованием: " << result.NumInstWithMultipleUses << "\n";
        cout << "  Инструкций, используемых вне BB: " << result.NumInstsUsedOutsideBB << "\n\n";
        
        cout << "МАКСИМАЛЬНЫЕ ЗНАЧЕНИЯ:\n";
        cout << "  Макс. операндов: " << result.MaxNumOperands << "\n";
        cout << "  Макс. использований: " << result.MaxNumUses << "\n";
        cout << "  Макс. предшественников BB: " << result.MaxNumPredecessorBBs << "\n";
        cout << "  Макс. последователей BB: " << result.MaxNumSuccessorBBs << "\n\n";
        
        // cout << "СРЕДНИЕ ЗНАЧЕНИЯ:\n";
        // if (result.BasicBlocks > 0) {
        //     cout << "  Средн. предшественников BB: " 
        //          << (double)result.TotalNumPredecessorBBs / result.BasicBlocks << "\n";
        //     cout << "  Средн. последователей BB: " 
        //          << (double)result.TotalNumSuccessorBBs / result.BasicBlocks << "\n";
        // }
        // if (result.Instructions > 0) {
        //     cout << "  Средн. операндов на инструкцию: " 
        //          << (double)result.TotalNumOperands / result.Instructions << "\n";
        //     cout << "  Средн. использований на инструкцию: " 
        //          << (double)result.TotalNumUses / result.Instructions << "\n";
        // }
        
        cout << "ВЫДЕЛЕННЫЕ ТИПЫ:\n";
        cout << "  Количество выделенных типов: " << result.AllocatedTypes.size() << "\n";
        
        // Можно также использовать встроенный оператор вывода:
        // cout << "\n--- Полный вывод через operator<< ---\n";
        // llvm::outs() << result << "\n";
        
        // Или JSON формат:
        // cout << "\n--- JSON формат ---\n";
        // result.printAsJson(llvm::outs());
        
    } catch (const exception& e) {
        cerr << "Error: Ошибка при анализе базовой информации с использованием PhASAR: " << e.what() << "\n";
    }
}

void runBasicInfo(LLVMProjectIRDB& IRDB, const Options& opts) {
    if (opts.useErrorCheckingAlgorithm) {
        runMyRealizedAnalysis(IRDB, opts);
        return;
    }

    if (opts.choice == AnalysisChoice::MyRealizedAnalysis || opts.choice == AnalysisChoice::Both) {
        runMyRealizedAnalysis(IRDB, opts);
    }
    
    if (opts.choice == AnalysisChoice::PhasarAnalysis || opts.choice == AnalysisChoice::Both) {
        runPhasarAnalysis(IRDB, opts);
    }
}

void analyzeModuleMetadata(llvm::Module* M) {
    cout << "\nМЕТАДАННЫЕ МОДУЛЯ:\n";
    
    // Анализ named metadata
    auto& namedMD = M->getNamedMDList();
    if (namedMD.empty()) {
        cout << "  Именованных метаданных не найдено\n";
    } else {
        cout << "  Именованные метаданные (" << namedMD.size() << "):\n";
        for (auto& NMD : namedMD) {
            cout << "    - " << NMD.getName().str() 
                 << " (операндов: " << NMD.getNumOperands() << ")\n";
        }
    }
    
    // Проверяем debug info
    if (M->getNamedMetadata("llvm.dbg.cu")) {
        cout << "  Debug информация присутствует\n";
    } else {
        cout << "  Debug информация отсутствует\n";
    }
}

void analyzeFunctionDetails(llvm::Function& F) {
    cout << "  " << F.getName().str() << ":\n";
    cout << "    Блоков: " << F.size() << ", Инструкций: " << F.getInstructionCount() << "\n";
    cout << "    Параметров: " << F.arg_size() << "\n";
    cout << "    Linkage: ";

    switch (F.getLinkage()) {
        case llvm::GlobalValue::ExternalLinkage: cout << "External"; break;
        case llvm::GlobalValue::InternalLinkage: cout << "Internal"; break;
        case llvm::GlobalValue::PrivateLinkage: cout << "Private"; break;
        default: cout << "Other"; break;
    }
    cout << "\n";
    
    // cout << "    Свойства функции: ";
    // bool hasProperties = false;
    
    // if (F.doesNotThrow()) {
    //     cout << "[nothrow] ";
    //     hasProperties = true;
    // }
    // if (F.hasInternalLinkage()) {
    //     cout << "[internal] ";
    //     hasProperties = true;
    // }
    // if (F.isVarArg()) {
    //     cout << "[vararg] ";
    //     hasProperties = true;
    // }
    // if (F.isDeclaration()) {
    //     cout << "[declaration] ";
    //     hasProperties = true;
    // } else {
    //     cout << "[defined] ";
    //     hasProperties = true;
    // }
    
    // llvm::CallingConv::ID cc = F.getCallingConv();
    // if (cc != llvm::CallingConv::C) {
    //     cout << "[cc:" << cc << "] ";
    //     hasProperties = true;
    // }
    
    // if (F.getVisibility() != llvm::GlobalValue::DefaultVisibility) {
    //     cout << "[visibility:";
    //     switch (F.getVisibility()) {
    //         case llvm::GlobalValue::HiddenVisibility: cout << "hidden"; break;
    //         case llvm::GlobalValue::ProtectedVisibility: cout << "protected"; break;
    //         default: cout << "other"; break;
    //     }
    //     cout << "] ";
    //     hasProperties = true;
    // }
    
    // if (!hasProperties) {
    //     cout << "стандартные свойства";
    // }
    // cout << "\n";
    
    // cout << "    Атрибуты (строковый анализ): ";
    // string funcStr;
    // llvm::raw_string_ostream funcOS(funcStr);
    // F.print(funcOS);
    // string funcString = funcOS.str();
    
    // bool hasStringAttrs = false;
    // if (funcString.find("nounwind") != string::npos) {
    //     cout << "[nounwind] ";
    //     hasStringAttrs = true;
    // }
    // if (funcString.find("readonly") != string::npos) {
    //     cout << "[readonly] ";
    //     hasStringAttrs = true;
    // }
    // if (funcString.find("readnone") != string::npos) {
    //     cout << "[readnone] ";
    //     hasStringAttrs = true;
    // }
    // if (funcString.find("alwaysinline") != string::npos) {
    //     cout << "[alwaysinline] ";
    //     hasStringAttrs = true;
    // }
    // if (funcString.find("noinline") != string::npos) {
    //     cout << "[noinline] ";
    //     hasStringAttrs = true;
    // }
    // if (funcString.find("optnone") != string::npos) {
    //     cout << "[optnone] ";
    //     hasStringAttrs = true;
    // }
    // if (funcString.find("mustprogress") != string::npos) {
    //     cout << "[mustprogress] ";
    //     hasStringAttrs = true;
    // }
    // if (funcString.find("uwtable") != string::npos) {
    //     cout << "[uwtable] ";
    //     hasStringAttrs = true;
    // }
    // if (funcString.find("norecurse") != string::npos) {
    //     cout << "[norecurse] ";
    //     hasStringAttrs = true;
    // }
    
    // if (!hasStringAttrs) {
    //     cout << "нет специальных атрибутов";
    // }
    // cout << "\n";
    
    if (F.arg_size() > 0) {
        cout << "    Параметры: ";
        bool first = true;
        for (auto& arg : F.args()) {
            if (!first) {
                cout << ", ";
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
            
            cout << simplifiedType;
            if (!arg.getName().empty()) {
                cout << " " << arg.getName().str();
            }
        }
        cout << "\n";
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
    
    cout << "    Возвращаемый тип: " << simplifiedRetType << "\n";

    cout << "\n";
}

void analyzeGlobalVariables(llvm::Module* M, const Options& opts) {
    if (!opts.useErrorCheckingAlgorithm) {
        cout << "АНАЛИЗ ГЛОБАЛЬНЫХ ЗНАЧЕНИЙ (без приватных):\n";
    }

    if (M->getGlobalList().empty()) {
        if (!opts.useErrorCheckingAlgorithm) {
            cout << "  Глобальных значений не найдено\n";
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
            cout << "  " << varName;
            
            if (isService) {
                cout << " [служебная]";
            }
            
            string typeStr;
            llvm::raw_string_ostream typeOS(typeStr);
            GV.getValueType()->print(typeOS);
            cout << " (тип: " << typeOS.str() << ")";
            
            if (GV.hasInitializer()) {
                cout << " [инициализирована";
                
                // Если это константа, пытаемся показать её значение
                if (GV.isConstant()) {
                    llvm::Constant* initializer = GV.getInitializer();
                    
                    if (auto* CI = llvm::dyn_cast<llvm::ConstantInt>(initializer)) {
                        // Целочисленная константа
                        cout << " = " << CI->getSExtValue();
                    } else if (auto* CF = llvm::dyn_cast<llvm::ConstantFP>(initializer)) {
                        // Константа с плавающей точкой
                        cout << " = " << CF->getValueAPF().convertToDouble();
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
                            
                            cout << " = \"" << escapedStr << "\"";
                        } else {
                            cout << " = [массив из " << CS->getNumElements() << " элементов]";
                        }
                    } else if (llvm::isa<llvm::ConstantAggregateZero>(initializer)) {
                        cout << " = 0 (zeroinitializer)";
                    } else if (auto* CA = llvm::dyn_cast<llvm::ConstantArray>(initializer)) {
                        cout << " = [массив из " << CA->getNumOperands() << " элементов]";
                    } else if (auto* CS = llvm::dyn_cast<llvm::ConstantStruct>(initializer)) {
                        cout << " = {структура из " << CS->getNumOperands() << " полей}";
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
                        cout << " = " << value;
                    }
                }
                
                cout << "]";
            } else {
                cout << " [не инициализирована]";
            }
            
            if (GV.isConstant() && !GV.hasInitializer()) {
                cout << " [константа]";
            }
            
            cout << "\n";
        }
    }
    
    if (!opts.useErrorCheckingAlgorithm) {
        if (visibleCount == 0) {
            cout << "  Видимых глобальных значений не найдено (все приватные)\n";
        } else {
            cout << "\nСТАТИСТИКА ГЛОБАЛЬНЫХ ЗНАЧЕНИЙ:\n";
            cout << "  Всего видимых: " << visibleCount << "\n";
            cout << "  Служебные константы: " << serviceConstCount << "\n";
            cout << "  Пользовательские константы: " << constCount << "\n";
            cout << "  Изменяемые: " << nonConstCount << "\n";
            
            if (constCount > 0) {
                cout << "\n  РЕКОМЕНДАЦИЯ:\n";
                cout << "  Обнаружены глобальные константы (" << constCount << ").\n";
                cout << "  Рекомендуется избегать использования глобальных значений, включая константы.\n";
            }
            
            if (nonConstCount > 0) {
                cout << "\n  ПРЕДУПРЕЖДЕНИЕ:\n";
                cout << "  Обнаружены изменяемые глобальные значения (" << nonConstCount << ")!\n";
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

        // Проверяем наличие ключа "globals"
        if (!helperJson.contains("globals")) {
            cerr << "Error: 'globals' key not found in helper file\n";
            return;
        }

        // Извлекаем массив функций
        if (!helperJson["globals"].is_array()) {
            cerr << "Error: 'globals' is not an array in helper file\n";
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
        cerr << "Error parsing JSON from helper file: " << e.what() << "\n";
        return;
    }
}
