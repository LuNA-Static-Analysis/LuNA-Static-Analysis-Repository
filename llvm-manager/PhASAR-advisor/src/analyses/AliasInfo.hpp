#ifndef ALIAS_INFO_HPP
#define ALIAS_INFO_HPP

#include "../dependes.hpp"

#include <html_reporter.hpp>

void runAliasAnalysis(LLVMProjectIRDB& IRDB, const Options& opts, HTMLReporter& htmlReporter);

#endif // ALIAS_INFO_HPP
