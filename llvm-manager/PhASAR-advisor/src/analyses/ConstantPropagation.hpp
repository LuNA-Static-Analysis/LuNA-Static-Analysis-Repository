#ifndef CONSTANT_PROPAGATION_HPP
#define CONSTANT_PROPAGATION_HPP

#include "../dependes.hpp"

#include <html_reporter.hpp>

// Опции для анализа
struct ConstPropOptions {
    bool verbose = true;              // Подробный вывод
    bool interProcedural = true;      // Межпроцедурный анализ
    bool trackPointers = true;        // Отслеживание указателей
    bool showOptimizations = true;    // Показывать возможности оптимизации
};

void runConstantPropagation(LLVMProjectIRDB& IRDB, const Options& opts, HTMLReporter& htmlReporter);
void runConstantPropagationWithOptions(LLVMProjectIRDB& IRDB, const Options& funcOpts, const ConstPropOptions& opts, HTMLReporter& htmlReporter);

#endif // CONSTANT_PROPAGATION_HPP
