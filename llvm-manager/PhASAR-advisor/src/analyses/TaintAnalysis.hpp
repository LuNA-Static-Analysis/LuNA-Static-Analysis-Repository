#ifndef TAINT_ANALYSIS_HPP
#define TAINT_ANALYSIS_HPP

#include "../dependes.hpp"

#include <html_reporter.hpp>

// #include "phasar/DataFlow.h"               // For solveIFDSProblem()
// #include "phasar/PhasarLLVM.h"             // For the HelperAnalyses
// #include "phasar/PhasarLLVM/DataFlow.h"    // For the IFDSTaintAnalysis
// #include "phasar/PhasarLLVM/TaintConfig.h" // For the LLVMTaintConfig

#include <phasar/PhasarLLVM/HelperAnalyses.h>

void runTaintAnalysis(LLVMProjectIRDB& IRDB, const Options& opts, HTMLReporter& htmlReporter);

#endif // TAINT_ANALYSIS_HPP
