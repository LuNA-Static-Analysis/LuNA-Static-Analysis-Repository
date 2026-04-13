#pragma once

// Standard library includes
#include <iostream>
#include <string>
#include <vector>
#include <unordered_set>
#include <unordered_map>
#include <fstream>
#include <filesystem>
#include <algorithm>
#include <exception>
#include <cstdlib>

// LLVM includes
#include <llvm/IR/Module.h>
#include <llvm/Support/raw_ostream.h>

// PhASAR core includes
#include <phasar/PhasarLLVM/ControlFlow/LLVMBasedICFG.h>
#include <phasar/PhasarLLVM/DB/LLVMProjectIRDB.h>
#include <phasar/PhasarLLVM/Utils/LLVMShorthands.h>
#include <phasar/Pointer/PointsToInfo.h>
#include "phasar/PhasarLLVM/TypeHierarchy.h"

// PhASAR analysis includes
#include <phasar/PhasarLLVM/DataFlow/IfdsIde/Problems/IDELinearConstantAnalysis.h>
#include <phasar/PhasarLLVM/DataFlow/IfdsIde/Problems/IFDSTaintAnalysis.h>
#include <phasar/PhasarLLVM/DataFlow/IfdsIde/Problems/IFDSUninitializedVariables.h>
#include <phasar/PhasarLLVM/DataFlow/IfdsIde/Problems/IFDSTypeAnalysis.h>

// PhASAR solver includes
#include <phasar/DataFlow/IfdsIde/Solver/IFDSSolver.h>
#include <phasar/DataFlow/IfdsIde/Solver/IDESolver.h>

// PhASAR utility includes
#include <phasar/Utils/Logger.h>

#include "./error_handler.hpp"
#include "./error_nodes.hpp"

using namespace std;
using namespace psr;

enum class AnalysisChoice {
    BasicAnalysis,
    DetailedAnalysis,
    Both
};

enum class OutputFormat {
    CONSOLE,  
    HTML
};

struct Options {
    AnalysisChoice choice = AnalysisChoice::Both;
    bool logging = false;
    bool includePrivateFunctions = false;  // Анализировать функции с _ (методы классов)
    OutputFormat outputFormat = OutputFormat::CONSOLE;
    std::string artifactsDir = "./PhASAR-artifacts"; // Директория для сохранения артефактов (графов, изображений и т.д.)
    std::string cssFilePath = []() {
        const char* adaptHome = std::getenv("ADAPT_HOME");
        return adaptHome ? std::string(adaptHome) + "/llvm-manager/PhASAR-advisor/src/report_style.css" 
                            : std::string("report_style.css");
    }();
    
    // Вспомогательная функция для проверки, нужно ли пропустить функцию
    bool shouldSkipFunction(const std::string& funcName) const {
        if (funcName.size() >= 2 && funcName[0] == '_' && funcName[1] == '_') {
            // Всегда пропускаем функции с __
            return true;
        }
        
        if (!includePrivateFunctions && !funcName.empty() && funcName[0] == '_') {
            // Пропускаем функции с _ если флаг не установлен
            return true;
        }
        
        return false;
    }

    bool useErrorCheckingAlgorithm = false;
    std::string helperFile = "";
    std::string outputFile = "";
};
