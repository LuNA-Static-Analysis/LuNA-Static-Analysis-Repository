#include "ConstantPropagation.hpp"
#include "html_reporter.hpp"

#include <iomanip>
#include <llvm/Support/raw_ostream.h>
#include <phasar/PhasarLLVM/ControlFlow/LLVMBasedICFG.h>

using namespace std;

struct ConstantValue {
    enum State { UNDEFINED, CONSTANT, VARYING } state;
    int64_t value;
    
    ConstantValue() : state(UNDEFINED), value(0) {}
    ConstantValue(int64_t val) : state(CONSTANT), value(val) {}
    
    bool isConstant() const { return state == CONSTANT; }
    bool isUndefined() const { return state == UNDEFINED; }
    bool isVarying() const { return state == VARYING; }
    
    void setVarying() { state = VARYING; }
    void setConstant(int64_t val) { state = CONSTANT; value = val; }
};

// Глобальное состояние для межпроцедурного анализа
static unordered_map<string, unordered_map<int, ConstantValue>> functionReturnValues;
static unordered_map<string, vector<ConstantValue>> functionParameterEffects;

// Вспомогательные функции
void analyzeFunctionWithOptions(llvm::Function& F, const ConstPropOptions& opts, std::ostream& out);
void analyzeCallSiteAdvanced(llvm::CallInst* CI, unordered_map<llvm::Value*, ConstantValue>& constants, const ConstPropOptions& opts);
ConstantValue getConstValueAdvanced(llvm::Value* val, const unordered_map<llvm::Value*, ConstantValue>& constants);
void printVerbose(const string& msg, const ConstPropOptions& opts, std::ostream& out);

// Основная функция
void runConstantPropagation(LLVMProjectIRDB& IRDB, const Options& opts, HTMLReporter& htmlReporter) {
    ConstPropOptions defaultOpts;
    defaultOpts.verbose = false;
    defaultOpts.interProcedural = true;
    defaultOpts.trackPointers = true;
    defaultOpts.showOptimizations = true;
    runConstantPropagationWithOptions(IRDB, opts, defaultOpts, htmlReporter);
}

static void runMyRealizedAnalysis(LLVMProjectIRDB& IRDB, const Options& funcOpts, const ConstPropOptions& opts, HTMLReporter& htmlReporter) {
    std::stringstream htmlOut;
    std::ostream& out = (funcOpts.outputFormat == OutputFormat::HTML) ? static_cast<std::ostream&>(htmlOut) : static_cast<std::ostream&>(std::cout);

    out << "\n=== АНАЛИЗ РАСПРОСТРАНЕНИЯ КОНСТАНТ ===\n";
    
    if (opts.interProcedural) {
        out << "\nРежим: Межпроцедурный анализ\n";
    } else {
        out << "\nРежим: Внутрипроцедурный анализ\n";
    }
    
    if (!opts.verbose) {
        out << "Краткий режим вывода\n";
    }
    
    try {
        auto* M = IRDB.getModule();
        if (!M) {
            cerr << "Error: Не удалось получить модуль из IRDB.\n";
            if (funcOpts.outputFormat == OutputFormat::HTML) {
                htmlReporter.addError("Не удалось получить модуль из IRDB (анализ распространения констант).");
            }
            return;
        }

        out << "Анализ модуля: " << M->getName().str() << "\n\n";
        
        // Межпроцедурная подготовка
        if (opts.interProcedural) {
            out << "Подготовка межпроцедурного анализа...\n";
            for (auto& F : *M) {
                if (!F.isDeclaration()) {
                    string funcName = F.getName().str();
                    if (funcOpts.shouldSkipFunction(funcName)) {
                        continue;
                    }
                    functionReturnValues[funcName] = {};
                    functionParameterEffects[funcName] = vector<ConstantValue>(F.arg_size());
                    printVerbose("  Функция зарегистрирована: " + funcName, opts, out);
                }
            }
            out << "\n";
        }
        
        // Анализ каждой функции
        for (auto& F : *M) {
            if (!F.isDeclaration()) {
                string funcName = F.getName().str();
                if (funcOpts.shouldSkipFunction(funcName)) {
                    continue;
                }
                out << "Функция: " << funcName << "\n";
                analyzeFunctionWithOptions(F, opts, out);
                out << "\n";
            }
        }
        
        // Межпроцедурные выводы
        if (opts.interProcedural) {
            out << "МЕЖПРОЦЕДУРНЫЕ ВЫВОДЫ:\n";
            for (const auto& [funcName, retVals] : functionReturnValues) {
                if (!retVals.empty()) {
                    for (const auto& [idx, val] : retVals) {
                        if (val.isConstant()) {
                            out << "  " << funcName << " всегда возвращает: " << val.value << "\n";
                        }
                    }
                }
            }
            
            for (const auto& [funcName, params] : functionParameterEffects) {
                for (size_t i = 0; i < params.size(); ++i) {
                    if (params[i].isConstant()) {
                        out << "  " << funcName << " параметр " << i << " всегда: " << params[i].value << "\n";
                    }
                }
            }
        }

        if (funcOpts.outputFormat == OutputFormat::HTML) {
            htmlReporter.addSection("Анализ распространения констант (Basic)", "<pre>" + htmlOut.str() + "</pre>");
        }
        
    } catch (const exception& e) {
        cerr << "Error: " << e.what() << "\n";
        if (funcOpts.outputFormat == OutputFormat::HTML) {
            htmlReporter.addError("Исключение при выполнении анализа распространения констант: " + string(e.what()));
        }
    }
}

static void runPhasarAnalysis(LLVMProjectIRDB& IRDB, const Options& funcOpts, const ConstPropOptions& opts, HTMLReporter& htmlReporter) {
    std::stringstream htmlOut;
    std::ostream& out = (funcOpts.outputFormat == OutputFormat::HTML) ? static_cast<std::ostream&>(htmlOut) : static_cast<std::ostream&>(std::cout);

    out << "\n=== АНАЛИЗ РАСПРОСТРАНЕНИЯ КОНСТАНТ С ИСПОЛЬЗОВАНИЕМ PhASAR ===\n";
    
    psr::LLVMBasedICFG ICFG(&IRDB, psr::CallGraphAnalysisType::OTF, {"main"});
    psr::IDELinearConstantAnalysis LCAProblem(&IRDB, &ICFG, {"main"});
    auto Results = psr::solveIDEProblem(LCAProblem, ICFG);
    
    auto* M = IRDB.getModule();
    if (!M) {
        llvm::errs() << "Error: Не удалось получить модуль.\n";
        if (funcOpts.outputFormat == OutputFormat::HTML) {
            htmlReporter.addError("Не удалось получить модуль (анализ распространения констант).");
        }
        return;
    }
    
    int totalOptimizations = 0;
    int functionsWithOptimizations = 0;
    
    // Проходим по всем функциям
    for (const auto& F : *M) {
        if (F.isDeclaration()) {
            continue;
        }

        // Пропускаем приватные/системные функции
        std::string funcName = F.getName().str();
        if (funcOpts.shouldSkipFunction(funcName)) {
            continue;
        }

        std::ostringstream funcOutput;
        int funcOptimizations = 0;
        
        // Проходим по всем базовым блокам
        for (const auto& BB : F) {
            // Проходим по всем инструкциям в блоке
            for (const auto& I : BB) {
                // Пропускаем load и alloca
                if (llvm::isa<llvm::LoadInst>(&I) || llvm::isa<llvm::AllocaInst>(&I)) {
                    continue;
                }
                
                // Получаем результаты анализа для этой инструкции
                const auto& ConstantsAtInst = Results.resultsAt(&I);
                
                // Подсчитываем константы (не bottom)
                int constCount = 0;
                for (const auto& [LLVMVar, ConstVal] : ConstantsAtInst) {
                    if (!ConstVal.isBottom()) {
                        constCount++;
                    }
                }
                
                // Определяем, есть ли оптимизация
                bool hasOptimization = false;
                std::string optimization;
                std::string readableInst;
                
                if (llvm::isa<llvm::BinaryOperator>(&I) && constCount >= 2) {
                    hasOptimization = true;
                    optimization = "Вычисление на этапе компиляции";
                    
                    auto* BO = llvm::dyn_cast<llvm::BinaryOperator>(&I);
                    std::string opName;
                    switch (BO->getOpcode()) {
                        case llvm::Instruction::Add: opName = "+"; break;
                        case llvm::Instruction::Sub: opName = "-"; break;
                        case llvm::Instruction::Mul: opName = "*"; break;
                        case llvm::Instruction::SDiv: opName = "/"; break;
                        case llvm::Instruction::SRem: opName = "%"; break;
                        case llvm::Instruction::And: opName = "&"; break;
                        case llvm::Instruction::Or: opName = "|"; break;
                        case llvm::Instruction::Xor: opName = "^"; break;
                        case llvm::Instruction::Shl: opName = "<<"; break;
                        case llvm::Instruction::LShr: opName = ">>"; break;
                        default: opName = BO->getOpcodeName();
                    }
                    
                    // Получаем значения операндов
                    std::string op1Str, op2Str;
                    
                    // Проверяем прямые константы в LLVM IR
                    if (auto* CI = llvm::dyn_cast<llvm::ConstantInt>(BO->getOperand(0))) {
                        op1Str = std::to_string(CI->getSExtValue());
                    }
                    if (auto* CI = llvm::dyn_cast<llvm::ConstantInt>(BO->getOperand(1))) {
                        op2Str = std::to_string(CI->getSExtValue());
                    }
                    
                    // Ищем значения переменных в результатах анализа
                    // Если операнд - это переменная, ищем её значение
                    if (op1Str.empty()) {
                        for (const auto& [LLVMVar, ConstVal] : ConstantsAtInst) {
                            if (!ConstVal.isBottom() && LLVMVar == BO->getOperand(0)) {
                                std::ostringstream oss;
                                oss << ConstVal;
                                op1Str = oss.str();
                                break;
                            }
                        }
                    }
                    
                    if (op2Str.empty()) {
                        for (const auto& [LLVMVar, ConstVal] : ConstantsAtInst) {
                            if (!ConstVal.isBottom() && LLVMVar == BO->getOperand(1)) {
                                std::ostringstream oss;
                                oss << ConstVal;
                                op2Str = oss.str();
                                break;
                            }
                        }
                    }
                    
                    if (!op1Str.empty() && !op2Str.empty()) {
                        readableInst = op1Str + " " + opName + " " + op2Str;
                    }
                    
                } else if (llvm::isa<llvm::ICmpInst>(&I) && constCount >= 2) {
                    hasOptimization = true;
                    optimization = "Результат сравнения известен";
                    
                    auto* IC = llvm::dyn_cast<llvm::ICmpInst>(&I);
                    std::string predName;
                    switch (IC->getPredicate()) {
                        case llvm::CmpInst::ICMP_EQ: predName = "=="; break;
                        case llvm::CmpInst::ICMP_NE: predName = "!="; break;
                        case llvm::CmpInst::ICMP_SGT: predName = ">"; break;
                        case llvm::CmpInst::ICMP_SGE: predName = ">="; break;
                        case llvm::CmpInst::ICMP_SLT: predName = "<"; break;
                        case llvm::CmpInst::ICMP_SLE: predName = "<="; break;
                        default: predName = "cmp";
                    }
                    
                    // Получаем значения операндов
                    std::string op1Str, op2Str;
                    
                    // Проверяем прямые константы в LLVM IR
                    if (auto* CI = llvm::dyn_cast<llvm::ConstantInt>(IC->getOperand(0))) {
                        op1Str = std::to_string(CI->getSExtValue());
                    }
                    if (auto* CI = llvm::dyn_cast<llvm::ConstantInt>(IC->getOperand(1))) {
                        op2Str = std::to_string(CI->getSExtValue());
                    }
                    
                    // Ищем значения переменных в результатах анализа
                    if (op1Str.empty()) {
                        for (const auto& [LLVMVar, ConstVal] : ConstantsAtInst) {
                            if (!ConstVal.isBottom() && LLVMVar == IC->getOperand(0)) {
                                std::ostringstream oss;
                                oss << ConstVal;
                                op1Str = oss.str();
                                break;
                            }
                        }
                    }
                    
                    if (op2Str.empty()) {
                        for (const auto& [LLVMVar, ConstVal] : ConstantsAtInst) {
                            if (!ConstVal.isBottom() && LLVMVar == IC->getOperand(1)) {
                                std::ostringstream oss;
                                oss << ConstVal;
                                op2Str = oss.str();
                                break;
                            }
                        }
                    }
                    
                    if (!op1Str.empty() && !op2Str.empty()) {
                        readableInst = op1Str + " " + predName + " " + op2Str;
                    }
                    
                } else if (llvm::isa<llvm::BranchInst>(&I) && constCount > 0) {
                    auto* BI = llvm::dyn_cast<llvm::BranchInst>(&I);
                    if (BI->isConditional()) {
                        hasOptimization = true;
                        optimization = "Упрощение ветвления";
                        
                        // Получаем значение условия
                        for (const auto& [LLVMVar, ConstVal] : ConstantsAtInst) {
                            if (!ConstVal.isBottom() && LLVMVar == BI->getCondition()) {
                                std::ostringstream oss;
                                oss << ConstVal;
                                std::string valStr = oss.str();
                                readableInst = "условие всегда " + valStr;
                                break;
                            }
                        }
                    }
                }
                
                if (hasOptimization) {
                    funcOutput << "  ✓ " << optimization;
                    if (!readableInst.empty()) {
                        funcOutput << ": " << readableInst;
                    }
                    funcOutput << "\n";
                    
                    // Показываем детали только если нужно
                    if (readableInst.empty()) {
                        funcOutput << "    Инструкция: " << psr::llvmIRToString(&I) << "\n";
                    }
                    
                    funcOptimizations++;
                }
            }
        }
        
        // Выводим функцию только если есть оптимизации
        if (funcOptimizations > 0) {
            out << "\n━━━ Функция: " << F.getName().str() << " (" << funcOptimizations << " оптимизаций) ━━━\n";
            out << funcOutput.str();

            functionsWithOptimizations++;
            totalOptimizations += funcOptimizations;
        }
    }
    
    // Общая статистика
    out << "\nИТОГОВАЯ СТАТИСТИКА:\n";
    out << "Функций с оптимизациями: " << functionsWithOptimizations << "\n";
    out << "Всего найдено оптимизаций: " << totalOptimizations << "\n";

    if (funcOpts.outputFormat == OutputFormat::HTML) {
        htmlReporter.addSection("Анализ распространения констант (Detailed)", "<pre>" + htmlOut.str() + "</pre>");
    }
}

void runConstantPropagationWithOptions(LLVMProjectIRDB& IRDB, const Options& funcOpts, const ConstPropOptions& opts, HTMLReporter& htmlReporter) {
    if (funcOpts.choice == AnalysisChoice::BasicAnalysis || funcOpts.choice == AnalysisChoice::Both) {
        runMyRealizedAnalysis(IRDB, funcOpts, opts, htmlReporter);
    }

    if (funcOpts.choice == AnalysisChoice::DetailedAnalysis || funcOpts.choice == AnalysisChoice::Both) {
        runPhasarAnalysis(IRDB, funcOpts, opts, htmlReporter);
    }
}

void printVerbose(const string& msg, const ConstPropOptions& opts, std::ostream& out) {
    if (opts.verbose) {
        out << msg << "\n";
    }
}

void analyzeFunctionWithOptions(llvm::Function& F, const ConstPropOptions& opts, std::ostream& out) {
    unordered_map<llvm::Value*, ConstantValue> constants;
    
    int constantsFound = 0;
    int optimizableOps = 0;
    int totalInstructions = 0;
    int interprocCalls = 0;
    
    // Инициализация параметров для межпроцедурного анализа
    if (opts.interProcedural) {
        string funcName = F.getName().str();
        if (functionParameterEffects.count(funcName)) {
            unsigned argIdx = 0;
            for (auto& arg : F.args()) {
                if (argIdx < functionParameterEffects[funcName].size()) {
                    auto& paramVal = functionParameterEffects[funcName][argIdx];
                    if (paramVal.isConstant()) {
                        constants[&arg] = paramVal;
                    }
                }
                argIdx++;
            }
        }
    }
    
    for (auto& BB : F) {
        for (auto& I : BB) {
            totalInstructions++;
            
            // Store инструкции
            if (auto* SI = llvm::dyn_cast<llvm::StoreInst>(&I)) {
                llvm::Value* storedValue = SI->getValueOperand();
                llvm::Value* pointer = SI->getPointerOperand();
                
                if (auto* CI = llvm::dyn_cast<llvm::ConstantInt>(storedValue)) {
                    constants[pointer].setConstant(CI->getSExtValue());
                    constantsFound++;
                } else {
                    ConstantValue val = getConstValueAdvanced(storedValue, constants);
                    constants[pointer] = val;
                }
            }
            
            // Load инструкции
            else if (auto* LI = llvm::dyn_cast<llvm::LoadInst>(&I)) {
                llvm::Value* pointer = LI->getPointerOperand();
                ConstantValue val = getConstValueAdvanced(pointer, constants);
                constants[LI] = val;
            }
            
            // Арифметические операции
            else if (auto* BO = llvm::dyn_cast<llvm::BinaryOperator>(&I)) {
                ConstantValue op1 = getConstValueAdvanced(BO->getOperand(0), constants);
                ConstantValue op2 = getConstValueAdvanced(BO->getOperand(1), constants);
                
                if (op1.isConstant() && op2.isConstant()) {
                    int64_t result = 0;
                    bool validOp = true;
                    string opName = BO->getOpcodeName();
                    
                    switch (BO->getOpcode()) {
                        case llvm::Instruction::Add: result = op1.value + op2.value; break;
                        case llvm::Instruction::Sub: result = op1.value - op2.value; break;
                        case llvm::Instruction::Mul: result = op1.value * op2.value; break;
                        case llvm::Instruction::SDiv:
                            if (op2.value != 0) result = op1.value / op2.value;
                            else validOp = false;
                            break;
                        default: validOp = false; break;
                    }
                    
                    if (validOp) {
                        constants[BO].setConstant(result);
                        optimizableOps++;
                        printVerbose("    ✓ Вычислено: " + to_string(op1.value) + " " + opName + " " + 
                                   to_string(op2.value) + " = " + to_string(result), opts, out);
                    } else {
                        constants[BO].setVarying();
                    }
                } else {
                    constants[BO].setVarying();
                }
            }
            
            // Вызовы функций
            else if (auto* CI = llvm::dyn_cast<llvm::CallInst>(&I)) {
                if (opts.interProcedural) {
                    analyzeCallSiteAdvanced(CI, constants, opts);
                    interprocCalls++;
                }
            }
            
            // Условные переходы
            else if (auto* BI = llvm::dyn_cast<llvm::BranchInst>(&I)) {
                if (BI->isConditional()) {
                    ConstantValue cond = getConstValueAdvanced(BI->getCondition(), constants);
                    if (cond.isConstant()) {
                        printVerbose("    ✓ Константное условие: " + to_string(cond.value), opts, out);
                        optimizableOps++;
                    }
                }
            }
            
            // Возврат из функции
            else if (auto* RI = llvm::dyn_cast<llvm::ReturnInst>(&I)) {
                if (RI->getReturnValue()) {
                    ConstantValue retVal = getConstValueAdvanced(RI->getReturnValue(), constants);
                    if (retVal.isConstant()) {
                        if (opts.interProcedural) {
                            functionReturnValues[F.getName().str()][0] = retVal;
                        }
                    }
                }
            }
        }
    }
    
    // Статистика
    out << "  Найдено констант: " << constantsFound 
         << " | Оптимизируемых операций: " << optimizableOps
         << " | Всего инструкций: " << totalInstructions;

    if (totalInstructions > 0) {
        double optimizationPercent = (double)optimizableOps / totalInstructions * 100;
        out << " | Потенциал: " << fixed << setprecision(1) << optimizationPercent << "%";
    }
    out << "\n";
}

void analyzeCallSiteAdvanced(llvm::CallInst* CI, unordered_map<llvm::Value*, ConstantValue>& constants, const ConstPropOptions& opts) {
    if (!CI->getCalledFunction()) {
        return;
    }
    
    string funcName = CI->getCalledFunction()->getName().str();
    
    // Анализ аргументов
    for (unsigned i = 0; i < CI->arg_size(); ++i) {
        ConstantValue argVal = getConstValueAdvanced(CI->getArgOperand(i), constants);
        if (argVal.isConstant()) {
            // Обновляем информацию о функции
            if (functionParameterEffects.count(funcName) && i < functionParameterEffects[funcName].size()) {
                functionParameterEffects[funcName][i] = argVal;
            }
        }
    }
    
    // Проверяем возвращаемое значение
    if (functionReturnValues.count(funcName) && functionReturnValues[funcName].count(0)) {
        ConstantValue retVal = functionReturnValues[funcName][0];
        if (retVal.isConstant()) {
            constants[CI] = retVal;
        }
    }
}

ConstantValue getConstValueAdvanced(llvm::Value* val, const unordered_map<llvm::Value*, ConstantValue>& constants) {
    // Прямая константа
    if (auto* CI = llvm::dyn_cast<llvm::ConstantInt>(val)) {
        return ConstantValue(CI->getSExtValue());
    }
    
    // Из таблицы констант
    if (constants.count(val)) {
        return constants.at(val);
    }
    
    // По умолчанию - переменная
    ConstantValue varying;
    varying.setVarying();
    return varying;
}
