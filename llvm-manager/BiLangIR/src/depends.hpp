#pragma once

#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IR/DebugInfoMetadata.h"
#include "llvm/IR/DebugInfo.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/Linker/Linker.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/raw_ostream.h"
#include <llvm/IR/GlobalVariable.h>

#include <nlohmann/json.hpp>
#include <string>
#include <vector>

using namespace llvm;

using json = nlohmann::json;

enum class InitStatus { NORMAL, INIT, UNKNOWN, FAILED };

extern std::unique_ptr<LLVMContext> TheContext;
extern std::unique_ptr<IRBuilder<>> Builder;
extern std::unique_ptr<Module> TheModule;

extern std::map<std::string, Value *> NamedValues;
extern std::map<std::string, std::string> FunctionProtos;
extern std::map<std::string, int> FunctionProtoPositions;
extern std::map<std::string, std::string> initMemoryMap;
extern std::vector<std::string> ParamsVec;

extern std::vector<std::string> importFunctions;
extern std::vector<std::string> subFunctions;
extern std::map<std::string, int> subFunctionsPositions;
extern std::map<std::string, std::vector<int>> initPosInSubs;
extern std::map<std::string, std::vector<std::string>> subsNamesMap;
extern std::string tempSubName;
extern std::map<std::string, InitStatus> subsStatusMap;
extern std::vector<std::string> subsWhichTaged;

extern std::map<std::string, std::map<std::string, int>> dfDimensionsMap;
extern std::map<std::string, std::vector<std::string>> subNamesParamsMap;
extern std::map<std::string, std::map<std::string, int>> subDimensionsIncreaseMap;
extern std::map<std::string, std::map<std::string, std::vector<std::pair<int, std::string>>>>
    subDfcallsMap;
extern std::map<std::string, std::map<int, std::string>> subPosNameMap;

extern std::map<std::string, Value *> dfArrayMap;
extern std::map<std::string, std::string> paramToActualNameMap;

// Map to store parameter name -> array base address passed as argument
// When a parameter is a name (reference), we store the actual array address here
extern std::map<std::string, Value *> paramArrayAddressMap;

extern std::map<std::string, std::pair<std::string, int>> globalConstantsMap; 
extern std::map<std::string, std::pair<std::string, int>> functionsMap;    
extern std::map<std::string, std::pair<std::string, int>> declarationsMap;   

extern bool isExternFunc;
extern std::string tmpAnalyzingFunc;

extern bool isInitFuncAnalysis;
extern std::string globalFaName;
