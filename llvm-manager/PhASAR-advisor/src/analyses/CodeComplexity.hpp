#ifndef CODE_COMPLEXITY_HPP
#define CODE_COMPLEXITY_HPP

#include "../dependes.hpp"

#include <html_reporter.hpp>

void runCodeComplexityAnalysis(LLVMProjectIRDB& IRDB, const Options& opts, HTMLReporter& htmlReporter);

#endif // CODE_COMPLEXITY_HPP
