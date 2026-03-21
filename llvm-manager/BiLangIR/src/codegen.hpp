#pragma once

#include "nodes.hpp"

#include <map>

std::unique_ptr<ExprAST> LogError(const char *Str);
Value *LogErrorV(const char *Str);

Value *plusOrConcat(Value *L, Value *R);
Value *concatFunc(Value *L, Value *R);
