#ifndef TYPE_ANALYSIS_HPP
#define TYPE_ANALYSIS_HPP

#include "../dependes.hpp"

#include <html_reporter.hpp>

void runTypeAnalysis(LLVMProjectIRDB& IRDB, const Options& opts, HTMLReporter& htmlReporter);

#endif // TYPE_ANALYSIS_HPP
