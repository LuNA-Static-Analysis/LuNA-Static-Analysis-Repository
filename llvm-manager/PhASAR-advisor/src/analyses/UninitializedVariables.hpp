#ifndef UNINITIALIZED_VARIABLES_HPP
#define UNINITIALIZED_VARIABLES_HPP

#include "../dependes.hpp"

#include <html_reporter.hpp>

void runUninitializedVariables(LLVMProjectIRDB& IRDB, const Options& opts, HTMLReporter& htmlReporter);

#endif // UNINITIALIZED_VARIABLES_HPP
