#ifndef CONSTANT_PROPAGATION_HPP
#define CONSTANT_PROPAGATION_HPP

#include "../dependes.hpp"

// Опции для анализа
struct ConstPropOptions {
    bool verbose = true;              // Подробный вывод
    bool interProcedural = true;      // Межпроцедурный анализ
    bool trackPointers = true;        // Отслеживание указателей
    bool showOptimizations = true;    // Показывать возможности оптимизации
};

void runConstantPropagation(LLVMProjectIRDB& IRDB, const Options& opts);
void runConstantPropagationWithOptions(LLVMProjectIRDB& IRDB, const Options& funcOpts, const ConstPropOptions& opts);

#endif // CONSTANT_PROPAGATION_HPP
