#include "codegen.hpp"

#include "debug.hpp"
#include "depends.hpp"
#include "error_handler.hpp"
#include "error_nodes.hpp"
#include "nodes.hpp"

#include <exception>
#include <iostream>
#include <memory>
#include <string>

int counter = 0;
std::map<std::string, std::string> counters;

// Cache for dynamic index expressions: maps expression signature to unique index
std::map<std::string, int> dynamicIndexCache;

namespace {
// Кэшируем типы, чтобы не создавать их повторно
static StructType *serializableType = nullptr;
static StructType *dfType = nullptr;

// Create a signature for an expression to cache dynamic indices
std::string createExprSignature(ExprAST *expr, Value *indexValue) {
    if (!expr || !indexValue) {
        return "";
    }

    // Try to get meaningful name from the expression
    std::string exprText = expr->getText();

    // If it's a variable or name expression, use its text
    if (exprText != "BinaryExprAST" && exprText != "not_generated") {
        return exprText;
    }

    // For complex expressions, create signature based on LLVM value
    std::string signature;
    llvm::raw_string_ostream rso(signature);
    indexValue->print(rso);
    rso.flush();

    return signature;
}

// Create an alloca instruction at the start of the function for better optimization
AllocaInst *CreateEntryBlockAlloca(Function *TheFunction, Type *VarType,
                                   const std::string &VarName) {
    IRBuilder<> TmpB(&TheFunction->getEntryBlock(), TheFunction->getEntryBlock().begin());
    return TmpB.CreateAlloca(VarType, nullptr, VarName);
}

// Store a value to a variable (handling both AllocaInst and direct values)
Value *StoreToVariable(const std::string &VarName, Value *Val) {
    Value *Variable = NamedValues[VarName];
    if (!Variable) {
        // Try NameMap for complex expressions
        Variable = NameMap[VarName];
        if (!Variable) {
            // ErrorHandler::printError("Variable '" + VarName + "' not found for assignment");
            DEBUG_OUT << "StoreToVariable: Variable '" << VarName << "' not found for assignment"
                      << std::endl;
            return nullptr;
        }
    }

    if (AllocaInst *AI = dyn_cast<AllocaInst>(Variable)) {
        Builder->CreateStore(Val, AI);
        // Mark as initialized
        initsNameMap[VarName] = true;
        return Val;
    } else {
        // ErrorHandler::printError("Cannot assign to non-mutable variable '" + VarName + "'");
        DEBUG_OUT << "StoreToVariable: Cannot assign to non-mutable variable '" << VarName << "'"
                  << std::endl;
        return nullptr;
    }
}

Type *getNameObjectType() {
    if (dfType != nullptr) {
        return dfType;
    }

    // Сначала пытаемся найти существующий тип в модуле
    dfType = StructType::getTypeByName(*TheContext, "class.DF");

    if (dfType != nullptr) {
        DEBUG_PRINT("Found existing class.DF type in module for codegen");
        return dfType;
    }

    // Если тип не найден, создаем его
    DEBUG_PRINT("Creating new class.DF type for codegen");

    // Вариант с Serializable
    // %class.DF = type <{ %class.Serializable, ptr, i64, ptr, i32, [4 x i8] }>

    // Сначала создаем тип для базового класса Serializable (содержит vtable pointer)

    if (serializableType == nullptr) {
        std::vector<Type *> serializableElements = {
            PointerType::get(Type::getInt8Ty(*TheContext), 0)  // vtable pointer
        };
        serializableType =
            StructType::create(*TheContext, serializableElements, "class.Serializable");
    }

    // Теперь создаем полный тип для класса DF
    std::vector<Type *> dfElements = {
        serializableType,                                   // %class.Serializable base
        PointerType::get(Type::getInt8Ty(*TheContext), 0),  // ptr (void* ptr_)
        Type::getInt64Ty(*TheContext),                      // i64 (size_t size_)
        PointerType::get(Type::getInt8Ty(*TheContext), 0),  // ptr (atomic_int* refs_)
        Type::getInt32Ty(*TheContext),                      // i32 (ValueType type_)
        ArrayType::get(Type::getInt8Ty(*TheContext), 4)     // [4 x i8] padding
    };

    // // Вариант без Serializable
    // // Создаем тип %"struct.std::__cxx11::basic_string<char>::_Alloc_hider"
    // // Это просто указатель (ptr в структуре basic_string)

    // StructType *allocHiderType = nullptr;
    // allocHiderType = StructType::getTypeByName(*TheContext,
    // "struct.std::__cxx11::basic_string<char>::_Alloc_hider");

    // if (allocHiderType == nullptr) {
    //   std::vector<Type*> allocHiderElements = {
    //       PointerType::get(Type::getInt8Ty(*TheContext), 0) // ptr
    //   };
    //   allocHiderType = StructType::create(*TheContext, allocHiderElements,
    //   "struct.std::__cxx11::basic_string<char>::_Alloc_hider");
    // }

    // // Теперь создаем полный тип для класса DF точно как в output.ll:
    // // %class.DF = type <{ %"struct.std::__cxx11::basic_string<char>::_Alloc_hider", ptr, i64,
    // ptr, i32, [4 x i8] }> std::vector<Type*> dfElements = {
    //     allocHiderType,                                    //
    //     %"struct.std::__cxx11::basic_string<char>::_Alloc_hider"
    //     PointerType::get(Type::getInt8Ty(*TheContext), 0), // ptr
    //     Type::getInt64Ty(*TheContext),                     // i64
    //     PointerType::get(Type::getInt8Ty(*TheContext), 0), // ptr
    //     Type::getInt32Ty(*TheContext),                     // i32
    //     ArrayType::get(Type::getInt8Ty(*TheContext), 4)    // [4 x i8]
    // };

    dfType = StructType::create(*TheContext, dfElements, "class.DF");
    return dfType;
}

Type *getNamePtrType() {
    Type *dfType = getNameObjectType();
    return PointerType::get(dfType, 0);
}

// Create a DF object (name) in LLVM IR
Value *createDFObject(const std::string &name) {
    Type *dfType = getNameObjectType();

    Function *currentFunction = Builder->GetInsertBlock()->getParent();
    AllocaInst *dfAlloca = CreateEntryBlockAlloca(currentFunction, dfType, name + "_df");

    FunctionType *constructorType =
        FunctionType::get(Type::getVoidTy(*TheContext), {PointerType::get(dfType, 0)}, false);

    FunctionCallee constructorFunc =
        TheModule->getOrInsertFunction("_ZN2DFC1Ev",  // Mangled name for DF::DF() constructor
                                       constructorType);

    // Call constructor on allocated object
    Builder->CreateCall(constructorFunc, dfAlloca);

    return dfAlloca;
}

// Create a dynamic array of DF objects
Value *createDFArray(const std::string &baseName, int arraySize, bool makeGlobal = false) {
    Type *dfType = getNameObjectType();
    // Type *dfPtrType = PointerType::get(dfType, 0);

    ArrayType *arrayType = ArrayType::get(dfType, arraySize);
    Value *arrayAlloca = nullptr;

    if (makeGlobal) {
        // Create global array
        GlobalVariable *globalArray =
            new GlobalVariable(*TheModule, arrayType, false, GlobalValue::InternalLinkage,
                               ConstantAggregateZero::get(arrayType), baseName + "_global_array");
        arrayAlloca = globalArray;
    } else {
        // Allocate local array in function
        Function *currentFunction = Builder->GetInsertBlock()->getParent();
        arrayAlloca = CreateEntryBlockAlloca(currentFunction, arrayType, baseName + "_array");
    }

    // // Initialize each element by calling DF constructor
    // FunctionType *constructorType =
    //     FunctionType::get(Type::getVoidTy(*TheContext), {dfPtrType}, false);
    // FunctionCallee constructorFunc = TheModule->getOrInsertFunction("_ZN2DFC1Ev",
    // constructorType);

    for (int i = 0; i < arraySize; i++) {
        // // Get pointer to element i
        // Value *indices[] = {ConstantInt::get(Type::getInt32Ty(*TheContext), 0),
        //                     ConstantInt::get(Type::getInt32Ty(*TheContext), i)};
        // Value *elementPtr = Builder->CreateInBoundsGEP(arrayType, arrayAlloca, indices,
        //                                                baseName + "[" + std::to_string(i) + "]");

        // // Call constructor on this element
        // Builder->CreateCall(constructorFunc, elementPtr);

        initsNameMap[baseName + "[" + std::to_string(i) + "]"] = false;
    }

    return arrayAlloca;
}

// Get or create array element for DF with dimension = 1
Value *getDFArrayElement(const std::string &baseName, Value *indexValue) {
    Value *arrayPtr = nullptr;

    // First, check if this is a parameter that maps to an actual argument
    std::string actualArrayName = baseName;
    if (paramToActualNameMap.find(baseName) != paramToActualNameMap.end()) {
        actualArrayName = paramToActualNameMap[baseName];
        DEBUG_OUT << "getDFArrayElement: Parameter " << baseName << " maps to actual argument "
                  << actualArrayName << std::endl;
    }

    // Check if array exists for the actual name
    if (dfArrayMap.find(actualArrayName) != dfArrayMap.end()) {
        arrayPtr = dfArrayMap[actualArrayName];
        DEBUG_OUT << "getDFArrayElement: Using existing array for " << actualArrayName << std::endl;
    } else if (NameMap.find(baseName) != NameMap.end()) {
        // Check if NameMap has an array for this baseName (parameter case)

        arrayPtr = NameMap[baseName];
        DEBUG_OUT << "getDFArrayElement: Using NameMap array for parameter " << baseName
                  << std::endl;
    }
    // Create new array if not found
    else {
        int arraySize = 100;  // Default size
        arrayPtr = createDFArray(actualArrayName, arraySize);
        dfArrayMap[actualArrayName] = arrayPtr;
        DEBUG_OUT << "getDFArrayElement: Created new array for " << actualArrayName << std::endl;
    }

    Type *dfType = getNameObjectType();
    ArrayType *arrayType = ArrayType::get(dfType, 100);  // Match the size used in creation

    // Ensure index is i32
    Value *indexI32 = indexValue;
    Type *indexType = indexValue->getType();

    DEBUG_OUT << "getDFArrayElement: Index type ID = " << indexType->getTypeID() << std::endl;

    // If index is a pointer to DF object, we need to extract the value
    // For now, use a placeholder index of 0
    if (indexType->isPointerTy()) {
        DEBUG_OUT << "getDFArrayElement: WARNING - Index is a pointer! Using 0 as placeholder"
                  << std::endl;
        indexI32 = ConstantInt::get(Type::getInt32Ty(*TheContext), 0);
    } else if (!indexType->isIntegerTy(32)) {
        DEBUG_OUT << "getDFArrayElement: Converting index from type " << indexType->getTypeID()
                  << " to i32" << std::endl;
        if (indexType->isDoubleTy()) {
            indexI32 =
                Builder->CreateFPToSI(indexValue, Type::getInt32Ty(*TheContext), "idx_to_i32");
        } else if (indexType->isIntegerTy()) {
            indexI32 = Builder->CreateIntCast(indexValue, Type::getInt32Ty(*TheContext), true,
                                              "idx_to_i32");
        }
    }

    // Get pointer to element at index
    Value *indices[] = {ConstantInt::get(Type::getInt32Ty(*TheContext), 0), indexI32};

    // Determine the correct type for GEP based on what arrayPtr is
    Type *gepType = nullptr;
    if (auto *allocaInst = dyn_cast<AllocaInst>(arrayPtr)) {
        // For AllocaInst, get the allocated type (this is the array type)
        gepType = allocaInst->getAllocatedType();
        DEBUG_OUT << "getDFArrayElement: Using AllocaInst allocated type for GEP" << std::endl;
    } else if (auto *globalVar = dyn_cast<GlobalVariable>(arrayPtr)) {
        // For GlobalVariable, get the value type (this is the array type)
        gepType = globalVar->getValueType();
        DEBUG_OUT << "getDFArrayElement: Using GlobalVariable value type for GEP" << std::endl;
    } else {
        // Fallback: assume it's a pointer to array type
        gepType = arrayType;
        DEBUG_OUT << "getDFArrayElement: Using constructed arrayType for GEP" << std::endl;
    }

    DEBUG_OUT << "getDFArrayElement: GEP type ID = " << gepType->getTypeID()
              << ", is array = " << gepType->isArrayTy() << std::endl;

    Value *elementPtr =
        Builder->CreateInBoundsGEP(gepType, arrayPtr, indices, baseName + "_element");

    return elementPtr;
}

// Get pointer to existing DF object
Value *getNameValue(const std::string &name) {  // Fixed: now creates proper DF objects with arrays
                                                // for dimension = 1
    // Check if DF object already exists in NameMap
    if (NameMap.find(name) != NameMap.end() && NameMap[name] != nullptr) {
        return NameMap[name];
    }

    // Check if this DF should use array (dimension = 1)
    bool useDFArray = false;
    if (dfDimensionsMap.find(tempSubName) != dfDimensionsMap.end()) {
        auto &dimMap = dfDimensionsMap[tempSubName];
        if (dimMap.find(name) != dimMap.end() && dimMap[name] == 1) {
            useDFArray = true;
            DEBUG_OUT << "getNameValue: Creating array for " << name << " in " << tempSubName
                      << std::endl;
        } else {
            DEBUG_OUT << "getNameValue: Creating single DF object for " << name << " in "
                      << tempSubName << std::endl;
        }
    } else {
        DEBUG_OUT << "getNameValue: No dimension map for sub " << tempSubName << std::endl;
    }

    // Create DF object or array
    Value *dfObject = nullptr;

    if (useDFArray) {
        // Create array and store it (local to function)
        dfObject = createDFArray(name, 100, false);  // Local array
        dfArrayMap[name] = dfObject;

        // Return pointer to first element as the "DF object"
        // This allows passing it to functions expecting DF*
        Type *dfType = getNameObjectType();
        ArrayType *arrayType = ArrayType::get(dfType, 100);
        Value *indices[] = {ConstantInt::get(Type::getInt32Ty(*TheContext), 0),
                            ConstantInt::get(Type::getInt32Ty(*TheContext), 0)};

        // Get the actual type of the array pointer
        Value *arrayValue = dfArrayMap[name];
        Type *gepType = nullptr;
        if (auto *allocaInst = dyn_cast<AllocaInst>(arrayValue)) {
            gepType = allocaInst->getAllocatedType();
        } else if (auto *globalVar = dyn_cast<GlobalVariable>(arrayValue)) {
            gepType = globalVar->getValueType();
        } else {
            gepType = arrayType;
        }

        dfObject = Builder->CreateInBoundsGEP(gepType, arrayValue, indices, name + "_ptr");
    } else {
        // Create single DF object
        dfObject = createDFObject(name);
        initsNameMap[name] = false;  // Mark as not initialized
    }

    NameMap[name] = dfObject;

    return dfObject;
}
Value *castToString(Value *value) {  // Unused
    Type *inputType = value->getType();

    std::string inputTypeName = std::to_string(counter++);

    Type *i8Type = Type::getInt8Ty(*TheContext);
    Type *bufferType = ArrayType::get(i8Type, 32);
    AllocaInst *buffer = Builder->CreateAlloca(bufferType, nullptr, inputTypeName + "_buffer");

    FunctionCallee sprintfFunc = TheModule->getOrInsertFunction(
        "sprintf", Type::getInt32Ty(*TheContext), Type::getInt8PtrTy(*TheContext),
        Type::getInt8PtrTy(*TheContext), inputType);

    Value *formatStr;

    if (inputType->isIntegerTy()) {
        formatStr = Builder->CreateGlobalStringPtr("%d", inputTypeName + "_format");
    } else if (inputType->isDoubleTy()) {
        formatStr = Builder->CreateGlobalStringPtr("%.17g", inputTypeName + "_format");
    } else {
        return LogErrorV("Invalid type for string cast");
    }

    Value *bufferPtr = Builder->CreateBitCast(buffer, Type::getInt8PtrTy(*TheContext),
                                              inputTypeName + "_buffer_ptr");

    Builder->CreateCall(sprintfFunc, {bufferPtr, formatStr, value},
                        inputTypeName + "_sprintf_call");

    return bufferPtr;
}

std::string getTypeName(Type *type) {
    if (type->isVoidTy())
        return "void";
    if (type->isIntegerTy(1))
        return "bool";
    if (type->isIntegerTy(32))
        return "int32";
    if (type->isIntegerTy(64))
        return "int64";
    if (type->isDoubleTy())
        return "double";
    if (type->isFloatTy())
        return "float";
    if (type->isPointerTy()) {
        return "some pointer type";
    }
    if (type->isArrayTy()) {
        return "array of " + getTypeName(type->getArrayElementType());
    }

    // Для неизвестных типов
    std::string typeStr;
    llvm::raw_string_ostream rso(typeStr);
    type->print(rso);
    return rso.str();
}

}  // namespace

std::unique_ptr<ExprAST> LogError(const char *Str) {  // Ok
    std::cerr << "Internal error: " << Str << std::endl;
    return nullptr;
}

Value *LogErrorV(const char *Str) {  // Ok
    LogError(Str);
    return nullptr;
}

Value *IntExprAST::codegen() {  // Ok
    return ConstantInt::get(*TheContext, APInt(32, Val, true));
}

std::string IntExprAST::getText() {  // Ok
    return "IntExprAST";
}

InitStatus IntExprAST::containsInit() {  // Ok
    return InitStatus::NORMAL;
}

Value *RealExprAST::codegen() {  // Ok
    return ConstantFP::get(Type::getDoubleTy(*TheContext), APFloat(Val));
}

std::string RealExprAST::getText() {  // Ok
    return "RealExprAST";
}

InitStatus RealExprAST::containsInit() {  // Ok
    return InitStatus::NORMAL;
}

Value *StringExprAST::codegen() {  // Ok
    return Builder->CreateGlobalStringPtr(Val, "string");
}

std::string StringExprAST::getText() {  // Ok
    return "StringExprAST";
}

InitStatus StringExprAST::containsInit() {  // Ok
    return InitStatus::NORMAL;
}

Value *NameExprAST::codegen() {  // Fixed: creates DF name object
    // Create or get existing DF object for this name
    Value *dfObject = getNameValue(Name);

    // // Mark as NAME type in the DF object if not already initialized
    // if (initsNameMap.find(Name) != initsNameMap.end() && !initsNameMap[Name]) {
    //     initsNameMap[Name] = true;
    // }

    return dfObject;
}

std::string NameExprAST::getText() {  // Ok
    return Name;
}

InitStatus NameExprAST::containsInit() {  // Ok
    return InitStatus::NORMAL;
}

Value *ValueExprAST::codegen() {  // Fixed: creates DF value object
    // Create or get existing DF object for this value
    Value *dfObject = getNameValue(Name);

    // // Mark as VALUE type in the DF object if not already initialized
    // if (initsNameMap.find(Name) != initsNameMap.end() && !initsNameMap[Name]) {
    //     initsNameMap[Name] = true;
    // }

    return dfObject;
}

std::string ValueExprAST::getText() {  // Ok
    return Name;
}

InitStatus ValueExprAST::containsInit() {  // Ok
    return InitStatus::NORMAL;
}

Value *VariableExprAST::codegen() {  // Fixed: proper variable loading
    // Check if variable exists in current scope
    Value *V = NamedValues[Name];

    if (!V) {
        // Check if it's in the name map (for complex expressions)
        if (initsNameMap.find(Name) != initsNameMap.end()) {
            V = NameMap[Name];
            if (!V) {
                std::string errMsg = "Variable '" + Name + "' declared but not allocated";
                DEBUG_OUT << "VariableExprAST::codegen: NameMap has entry for '" << Name
                          << "' but value is null" << std::endl;
                // ErrorHandler::printError(errMsg);
                return nullptr;
            }
        } else {
            std::string errMsg = "Unknown variable name '" + Name + "'";
            ErrorBase error(ErrorType::SYN9);
            error.setStringDetail("expression", Name);
            ErrorHandler::printError(errMsg, std::move(error));
            return nullptr;
        }
    }

    DEBUG_OUT << "VariableExprAST::codegen: " << Name << ", type: " << V->getType()->getTypeID()
              << std::endl;

    // If V is an AllocaInst, check what type it allocates
    if (AllocaInst *AI = dyn_cast<AllocaInst>(V)) {
        Type *allocatedType = AI->getAllocatedType();
        Type *intType = Type::getInt32Ty(*TheContext);
        Type *doubleType = Type::getDoubleTy(*TheContext);
        Type *stringType = PointerType::get(Type::getInt8Ty(*TheContext), 0);

        // Type *dfObjectType = getNamePtrType(); // Pointer to DF object type

        // Check if this is NOT a DF object type
        if (allocatedType == intType || allocatedType == doubleType ||
            allocatedType == stringType) {
            return Builder->CreateLoad(allocatedType, AI, Name + ".val");
        }
    }

    // If it's already a value (e.g., constants, function arguments), return as-is
    return V;
}

std::string VariableExprAST::getText() {  // Ok
    return Name;
}

InitStatus VariableExprAST::containsInit() {  // Ok
    return InitStatus::NORMAL;
}

Value *ComplexExprAST::codegen() {  // Fixed: proper DF array handling for dimension = 1
    std::string idName = id->getText();

    DEBUG_OUT << "ComplexExprAST::codegen: " << idName << std::endl;

    if (idName == "not_generated") {
        // id->codegen();
        // idName = id->getText();

        LogErrorV("Nested complex expression are not suppoted");
        return nullptr;
    }

    // Check if base name exists (either as name or another ComplexExpr)
    Value *baseDF = nullptr;

    // First check if it's in NameMap (for name objects)
    if (NameMap.find(idName) != NameMap.end()) {
        baseDF = NameMap[idName];
    }
    // Then check if it's a parameter
    else if (std::find(ParamsVec.begin(), ParamsVec.end(), idName) != ParamsVec.end()) {
        baseDF = NamedValues[idName];  // Parameter DF object
    } else {
        std::string errMsg = "Unknown base name '" + idName + "'";
        ErrorBase error(ErrorType::SYN11);
        error.setStringDetail("expression", idName);
        ErrorHandler::printError(errMsg, std::move(error));
        return nullptr;
    }

    if (!baseDF) {
        std::string errMsg = "Base name '" + idName + "' is not initialized";
        // ErrorHandler::printError(errMsg);
        DEBUG_OUT << "ComplexExprAST::codegen: " << errMsg << std::endl;
        return nullptr;
    }

    // Generate the index expression
    Value *indexExpr = expr->codegen();

    // Check if this DF has dimension = 1 (should use array)
    bool useDFArray = true;
    std::string arrayBaseName = idName;  // Default to idName

    // // Check if idName is a parameter that maps to an actual argument
    // if (paramToActualNameMap.find(idName) != paramToActualNameMap.end()) {
    //     arrayBaseName = paramToActualNameMap[idName];
    // }

    // // Check if the base name (either idName or mapped actual name) has dimension = 1
    // if (dfDimensionsMap.find(tempSubName) != dfDimensionsMap.end()) {
    //     auto &dimMap = dfDimensionsMap[tempSubName];
    //     if (dimMap.find(idName) != dimMap.end() && dimMap[idName] == 1) {
    //         useDFArray = true;
    //     }
    // }

    // Create the complex name string
    std::string complexName;

    if (indexExpr) {
        // If using DF array, get the element from array
        if (useDFArray) {
            // Convert index to i32 if needed
            Value *indexI32 = indexExpr;
            if (!indexExpr->getType()->isIntegerTy(32)) {
                if (indexExpr->getType()->isDoubleTy()) {
                    indexI32 =
                        Builder->CreateFPToSI(indexExpr, Type::getInt32Ty(*TheContext), "idx_cast");
                } else if (indexExpr->getType()->isIntegerTy()) {
                    indexI32 = Builder->CreateIntCast(indexExpr, Type::getInt32Ty(*TheContext),
                                                      true, "idx_cast");
                }
            }

            // Get element from array using the mapped base name
            Value *elementPtr = getDFArrayElement(arrayBaseName, indexI32);

            // Create unique name for this access
            auto *constInt = dyn_cast<ConstantInt>(indexExpr);
            if (constInt) {
                int index = constInt->getZExtValue();
                complexName = idName + "[" + std::to_string(index) + "]";
            } else {
                std::string exprSignature = createExprSignature(expr.get(), indexExpr);
                if (!exprSignature.empty()) {
                    std::string cacheKey = idName + ":" + exprSignature;
                    if (dynamicIndexCache.find(cacheKey) != dynamicIndexCache.end()) {
                        int cachedIndex = dynamicIndexCache[cacheKey];
                        complexName = idName + "[dyn_" + std::to_string(cachedIndex) + "]";
                    } else {
                        int newIndex = counter++;
                        dynamicIndexCache[cacheKey] = newIndex;
                        complexName = idName + "[dyn_" + std::to_string(newIndex) + "]";
                    }
                } else {
                    complexName = idName + "[dyn_" + std::to_string(counter++) + "]";
                }
            }

            this->name = complexName;

            // Store reference in NameMap for future use
            NameMap[complexName] = elementPtr;

            // initsNameMap[complexName] = false;  // Will be initialized when used

            return elementPtr;
        }

        // Original logic for non-array DF (dimension != 1)
        // If index is a constant, create static name
        auto *constInt = dyn_cast<ConstantInt>(indexExpr);
        if (constInt) {
            int index = constInt->getZExtValue();
            complexName = idName + "[" + std::to_string(index) + "]";
        } else {
            // Dynamic index - create or reuse cached index based on expression signature
            std::string exprSignature = createExprSignature(expr.get(), indexExpr);

            if (!exprSignature.empty()) {
                // Create cache key combining base name and expression signature
                std::string cacheKey = idName + ":" + exprSignature;

                // Check if we've seen this exact dynamic access before
                if (dynamicIndexCache.find(cacheKey) != dynamicIndexCache.end()) {
                    // Reuse existing index
                    int cachedIndex = dynamicIndexCache[cacheKey];
                    complexName = idName + "[dyn_" + std::to_string(cachedIndex) + "]";
                } else {
                    // Create new index and cache it
                    int newIndex = counter++;
                    dynamicIndexCache[cacheKey] = newIndex;
                    complexName = idName + "[dyn_" + std::to_string(newIndex) + "]";
                }
            } else {
                // Fallback to simple counter if signature creation failed
                complexName = idName + "[dyn_" + std::to_string(counter++) + "]";
            }
        }
    } else {
        // Index is a name reference
        std::string indexName = expr->getText();
        if (initsNameMap.find(indexName) != initsNameMap.end() && initsNameMap[indexName]) {
            complexName = idName + "[" + indexName + "]";
        } else {
            std::string errMsg = "Index name '" + indexName + "' is not initialized";
            // ErrorHandler::printError(errMsg);
            DEBUG_OUT << "CallExprAST::codegen: " << errMsg << std::endl;
            return nullptr;
        }
    }

    this->name = complexName;

    // Check if this complex name already exists
    if (NameMap.find(complexName) != NameMap.end()) {
        return NameMap[complexName];
    }

    // Create new DF object that inherits lifetime from base
    Value *complexDF = createDFObject(complexName);

    NameMap[complexName] = complexDF;

    // initsNameMap[complexName] = false;  // Will be initialized when used

    // Add to current subroutine's name list
    if (subsNamesMap.find(tempSubName) == subsNamesMap.end()) {
        subsNamesMap[tempSubName] = {complexName};
    } else {
        std::vector<std::string> names = subsNamesMap[tempSubName];
        names.push_back(complexName);
        subsNamesMap[tempSubName] = names;
    }

    return complexDF;
}

std::string ComplexExprAST::getText() {  // Ok
    return name;
}

InitStatus ComplexExprAST::containsInit() {  // Ok
    return InitStatus::NORMAL;
}

std::string ComplexExprAST::getBaseName() {
    if (auto *complexId = dynamic_cast<ComplexExprAST *>(id.get())) {
        return complexId->getBaseName();
    } else {
        return id->getText();
    }
}

int ComplexExprAST::getDimension(int currentDim) {
    if (auto *complexId = dynamic_cast<ComplexExprAST *>(id.get())) {
        return complexId->getDimension(currentDim + 1);
    } else {
        return currentDim;
    }
}

Value *BinaryExprAST::codegen() {  // Fixed: proper type handling for int32 and double
    Value *L = LHS->codegen();
    Value *R = RHS->codegen();

    if (!L || !R) {
        return nullptr;
    }

    // Helper function to convert to common type for arithmetic operations
    auto promoteToCommonType = [&](Value *&Left, Value *&Right) -> Type * {
        if (Left->getType()->isDoubleTy() || Right->getType()->isDoubleTy()) {
            // If either is double, convert both to double
            if (!Left->getType()->isDoubleTy()) {
                Left = Builder->CreateSIToFP(Left, Type::getDoubleTy(*TheContext), "L_double");
            }
            if (!Right->getType()->isDoubleTy()) {
                Right = Builder->CreateSIToFP(Right, Type::getDoubleTy(*TheContext), "R_double");
            }
            return Type::getDoubleTy(*TheContext);
        }
        // Both are integers
        return Type::getInt32Ty(*TheContext);
    };

    // Helper function to convert to boolean (i1) for logical operations
    auto convertToBool = [&](Value *Val) -> Value * {
        if (Val->getType()->isDoubleTy()) {
            // For double: compare with 0.0
            Value *Zero = ConstantFP::get(Type::getDoubleTy(*TheContext), 0.0);
            return Builder->CreateFCmpUNE(Val, Zero, "tobool");
        } else if (Val->getType()->isIntegerTy()) {
            // For int: compare with 0
            Value *Zero = ConstantInt::get(Val->getType(), 0);
            return Builder->CreateICmpNE(Val, Zero, "tobool");
        }
        return Val;  // Already boolean
    };

    if (Op == "+") {
        Type *commonType = promoteToCommonType(L, R);
        if (commonType->isDoubleTy()) {
            return Builder->CreateFAdd(L, R, "addtmp");
        } else {
            return Builder->CreateAdd(L, R, "addtmp");
        }
    } else if (Op == "-") {
        Type *commonType = promoteToCommonType(L, R);
        if (commonType->isDoubleTy()) {
            return Builder->CreateFSub(L, R, "subtmp");
        } else {
            return Builder->CreateSub(L, R, "subtmp");
        }
    } else if (Op == "*") {
        Type *commonType = promoteToCommonType(L, R);
        if (commonType->isDoubleTy()) {
            return Builder->CreateFMul(L, R, "multmp");
        } else {
            return Builder->CreateMul(L, R, "multmp");
        }
    } else if (Op == "/") {
        // Division always promotes to double for precision
        if (!L->getType()->isDoubleTy()) {
            L = Builder->CreateSIToFP(L, Type::getDoubleTy(*TheContext), "L_double");
        }
        if (!R->getType()->isDoubleTy()) {
            R = Builder->CreateSIToFP(R, Type::getDoubleTy(*TheContext), "R_double");
        }
        return Builder->CreateFDiv(L, R, "divtmp");
    } else if (Op == "%") {
        // Modulo only works with integers
        if (L->getType()->isDoubleTy() || R->getType()->isDoubleTy()) {
            // ErrorHandler::printError("Modulo operator not supported for floating-point numbers");
            DEBUG_OUT << "BinaryExprAST::codegen: Modulo operator not supported for floating-point "
                         "numbers"
                      << std::endl;
            return nullptr;
        }
        return Builder->CreateSRem(L, R, "modtmp");
    } else if (Op == "<") {
        Type *commonType = promoteToCommonType(L, R);
        if (commonType->isDoubleTy()) {
            return Builder->CreateFCmpULT(L, R, "cmptmp");
        } else {
            return Builder->CreateICmpSLT(L, R, "cmptmp");
        }
    } else if (Op == "<=") {
        Type *commonType = promoteToCommonType(L, R);
        if (commonType->isDoubleTy()) {
            return Builder->CreateFCmpULE(L, R, "cmptmp");
        } else {
            return Builder->CreateICmpSLE(L, R, "cmptmp");
        }
    } else if (Op == ">") {
        Type *commonType = promoteToCommonType(L, R);
        if (commonType->isDoubleTy()) {
            return Builder->CreateFCmpUGT(L, R, "cmptmp");
        } else {
            return Builder->CreateICmpSGT(L, R, "cmptmp");
        }
    } else if (Op == ">=") {
        Type *commonType = promoteToCommonType(L, R);
        if (commonType->isDoubleTy()) {
            return Builder->CreateFCmpUGE(L, R, "cmptmp");
        } else {
            return Builder->CreateICmpSGE(L, R, "cmptmp");
        }
    } else if (Op == "==") {
        Type *commonType = promoteToCommonType(L, R);
        if (commonType->isDoubleTy()) {
            return Builder->CreateFCmpUEQ(L, R, "cmptmp");
        } else {
            return Builder->CreateICmpEQ(L, R, "cmptmp");
        }
    } else if (Op == "!=") {
        Type *commonType = promoteToCommonType(L, R);
        if (commonType->isDoubleTy()) {
            return Builder->CreateFCmpUNE(L, R, "cmptmp");
        } else {
            return Builder->CreateICmpNE(L, R, "cmptmp");
        }
    } else if (Op == "&&") {
        // Logical AND: convert both to boolean first
        Value *LBool = convertToBool(L);
        Value *RBool = convertToBool(R);
        return Builder->CreateAnd(LBool, RBool, "andtmp");
    } else if (Op == "||") {
        // Logical OR: convert both to boolean first
        Value *LBool = convertToBool(L);
        Value *RBool = convertToBool(R);
        return Builder->CreateOr(LBool, RBool, "ortmp");
    } else {
        // ErrorHandler::printError("Invalid binary operator");
        DEBUG_OUT << "BinaryExprAST::codegen: Invalid binary operator '" << Op << "'" << std::endl;
        return nullptr;
    }
}

Value *plusOrConcat(Value *L, Value *R) {  // Unused
    Type *leftType = L->getType();
    Type *rightType = R->getType();

    if (leftType->isPointerTy() && !rightType->isPointerTy()) {
        Value *rightStr = castToString(R);
        return concatFunc(L, rightStr);
    } else if (!leftType->isPointerTy() && rightType->isPointerTy()) {
        Value *leftStr = castToString(L);
        return concatFunc(leftStr, R);
    }

    if (leftType->isPointerTy() && rightType->isPointerTy()) {
        return concatFunc(L, R);
    } else if (leftType->isIntegerTy() && rightType->isIntegerTy()) {
        return Builder->CreateAdd(L, R, "addtmp");
    } else if (leftType->isDoubleTy() && rightType->isDoubleTy()) {
        return Builder->CreateFAdd(L, R, "addtmp");
    } else {
        // ErrorHandler::printError("Invalid types for addition");
        DEBUG_OUT << "plusOrConcat: Invalid types for addition" << std::endl;
        return nullptr;
    }
}

Value *concatFunc(Value *L, Value *R) {  // Unused
    Type *i8PtrType = Type::getInt8PtrTy(*TheContext);
    Type *i32Type = Type::getInt32Ty(*TheContext);

    FunctionCallee strlenFunc = TheModule->getOrInsertFunction("strlen", i32Type, i8PtrType);
    Value *lenL = Builder->CreateCall(strlenFunc, L, "strlenL");
    Value *lenR = Builder->CreateCall(strlenFunc, R, "strlenR");

    Value *totalLen = Builder->CreateAdd(lenL, lenR, "totalLen");
    Value *totalLenPlusOne =
        Builder->CreateAdd(totalLen, ConstantInt::get(i32Type, 1), "totalLenPlusOne");

    FunctionCallee mallocFunc = TheModule->getOrInsertFunction("malloc", i8PtrType, i32Type);
    Value *buffer = Builder->CreateCall(mallocFunc, totalLenPlusOne, "buffer");

    FunctionCallee memcpyFunc =
        TheModule->getOrInsertFunction("memcpy", i8PtrType, i8PtrType, i8PtrType, i32Type);
    Builder->CreateCall(memcpyFunc, {buffer, L, lenL});

    Value *offset = Builder->CreateGEP(Type::getInt8Ty(*TheContext), buffer, lenL, "offset");
    Builder->CreateCall(memcpyFunc, {offset, R, lenR});

    Value *nullTerminator =
        Builder->CreateGEP(Type::getInt8Ty(*TheContext), buffer, totalLen, "nullTerminator");
    Builder->CreateStore(ConstantInt::get(Type::getInt8Ty(*TheContext), 0), nullTerminator);

    return buffer;
}

std::string BinaryExprAST::getText() {  // Ok
    return "BinaryExprAST";
}

InitStatus BinaryExprAST::containsInit() {  // Ok
    return InitStatus::NORMAL;
}

Value *IfExprAST::codegen() {  // Fixed: proper if statement handling
    Function *TheFunction = Builder->GetInsertBlock()->getParent();
    Value *CondV = Cond->codegen();

    if (!CondV) {
        return nullptr;
    }

    // Convert condition to boolean (i1) type
    Value *BoolCond;
    if (CondV->getType()->isDoubleTy()) {
        // For double: compare with 0.0
        Value *Zero = ConstantFP::get(Type::getDoubleTy(*TheContext), 0.0);
        BoolCond = Builder->CreateFCmpUNE(CondV, Zero, "ifcond");
    } else if (CondV->getType()->isIntegerTy()) {
        if (CondV->getType()->isIntegerTy(1)) {
            // Already boolean
            BoolCond = CondV;
        } else {
            // For int: compare with 0
            Value *Zero = ConstantInt::get(CondV->getType(), 0);
            BoolCond = Builder->CreateICmpNE(CondV, Zero, "ifcond");
        }
    } else {
        // ErrorHandler::printError("Invalid condition type for if statement");
        DEBUG_OUT << "IfExprAST::codegen: Invalid condition type for if statement" << std::endl;
        return nullptr;
    }

    // Create basic blocks
    BasicBlock *ThenBB = BasicBlock::Create(*TheContext, "then", TheFunction);
    BasicBlock *MergeBB = BasicBlock::Create(*TheContext, "ifcont", TheFunction);

    // Create conditional branch
    Builder->CreateCondBr(BoolCond, ThenBB, MergeBB);

    // Generate then block
    Builder->SetInsertPoint(ThenBB);
    Block->codegen();            // Execute the then body
    Builder->CreateBr(MergeBB);  // Branch to merge block

    // Continue with merge block
    Builder->SetInsertPoint(MergeBB);

    return nullptr;
}

std::string IfExprAST::getText() {  // Ok
    return "IfExprAST";
}

InitStatus IfExprAST::containsInit() {  // Ok
    return this->Block->containsInit();
}

InitStatus IfExprAST::tryResolveInitStatus() {
    return this->Block->tryResolving();
}

Value *WhileExprAST::codegen() {  // Fixed: proper while loop handling
    Type *Int32Ty = Type::getInt32Ty(*TheContext);
    Function *TheFunction = Builder->GetInsertBlock()->getParent();

    Value *startValue = startVal->codegen();

    if (!startValue) {
        return nullptr;
    }

    // Convert values to int32 if needed
    if (!startValue->getType()->isIntegerTy(32)) {
        if (startValue->getType()->isDoubleTy()) {
            startValue = Builder->CreateFPToSI(startValue, Int32Ty, "start_int");
        }
    }

    // Save previous value if variable already exists
    Value *OldVal = nullptr;
    if (NamedValues.find(VarName) != NamedValues.end()) {
        OldVal = NamedValues[VarName];
    }

    // Create alloca for loop variable
    AllocaInst *Alloca = CreateEntryBlockAlloca(TheFunction, Int32Ty, VarName);

    // Store initial value
    Builder->CreateStore(startValue, Alloca);

    // Add loop variable to symbol table
    NamedValues[VarName] = Alloca;
    counters[VarName] = "int";

    // Create basic blocks
    BasicBlock *CondBB = BasicBlock::Create(*TheContext, "whilecond", TheFunction);
    BasicBlock *LoopBB = BasicBlock::Create(*TheContext, "whilebody", TheFunction);
    BasicBlock *IncBB = BasicBlock::Create(*TheContext, "whileinc", TheFunction);
    BasicBlock *AfterBB = BasicBlock::Create(*TheContext, "afterwhile", TheFunction);

    // Branch to condition block
    Builder->CreateBr(CondBB);

    // Generate condition block
    Builder->SetInsertPoint(CondBB);
    Value *CondV = Cond->codegen();

    if (!CondV) {
        return nullptr;
    }

    // Convert condition to boolean (i1) type
    Value *BoolCond;
    if (CondV->getType()->isDoubleTy()) {
        // For double: compare with 0.0
        Value *Zero = ConstantFP::get(Type::getDoubleTy(*TheContext), 0.0);
        BoolCond = Builder->CreateFCmpUNE(CondV, Zero, "whilecond");
    } else if (CondV->getType()->isIntegerTy()) {
        if (CondV->getType()->isIntegerTy(1)) {
            // Already boolean
            BoolCond = CondV;
        } else {
            // For int: compare with 0
            Value *Zero = ConstantInt::get(CondV->getType(), 0);
            BoolCond = Builder->CreateICmpNE(CondV, Zero, "whilecond");
        }
    } else {
        // ErrorHandler::printError("Invalid condition type for while loop");
        DEBUG_OUT << "WhileExprAST::codegen: Invalid condition type for while loop" << std::endl;
        return nullptr;
    }

    // Create conditional branch: if true go to loop body, else exit
    Builder->CreateCondBr(BoolCond, LoopBB, AfterBB);

    // Generate loop body
    Builder->SetInsertPoint(LoopBB);
    Block->codegen();
    Builder->CreateBr(IncBB);  // Go back to increment block

    // Generate increment block
    Builder->SetInsertPoint(IncBB);
    Value *currentVar = Builder->CreateLoad(Int32Ty, Alloca, VarName + ".val");
    Value *incrementedVar = Builder->CreateAdd(currentVar, Builder->getInt32(1), VarName + ".inc");
    Builder->CreateStore(incrementedVar, Alloca);
    Builder->CreateBr(CondBB);  // Go back to condition check

    // Continue after loop
    Builder->SetInsertPoint(AfterBB);

    // Restore previous value or remove from symbol table
    if (OldVal) {
        NamedValues[VarName] = OldVal;
    } else {
        NamedValues.erase(VarName);
    }
    counters.erase(VarName);

    return nullptr;
}

std::string WhileExprAST::getText() {  // Ok
    return "WhileExprAST";
}

InitStatus WhileExprAST::containsInit() {  // Ok
    return this->Block->containsInit();
}

InitStatus WhileExprAST::tryResolveInitStatus() {
    return this->Block->tryResolving();
}

Value *ForExprAST::codegen() {  // Fixed: proper for loop handling
    Type *Int32Ty = Type::getInt32Ty(*TheContext);
    Function *TheFunction = Builder->GetInsertBlock()->getParent();

    // Generate start and end values
    Value *StartVal = Start->codegen();
    Value *EndVal = End->codegen();

    if (!StartVal || !EndVal) {
        return nullptr;
    }

    // Convert values to int32 if needed
    if (!StartVal->getType()->isIntegerTy(32)) {
        // ErrorHandler::printError("StartVal type is " +
        //                          std::to_string(StartVal->getType()->getTypeID()));

        DEBUG_OUT << "ForExprAST::codegen: StartVal type is "
                  << std::to_string(StartVal->getType()->getTypeID()) << std::endl;

        if (StartVal->getType()->isDoubleTy()) {
            StartVal = Builder->CreateFPToSI(StartVal, Int32Ty, "start_int");
        }
    }
    if (!EndVal->getType()->isIntegerTy(32)) {
        // ErrorHandler::printError("EndVal type is " +
        //                          std::to_string(EndVal->getType()->getTypeID()));

        DEBUG_OUT << "ForExprAST::codegen: EndVal type is "
                  << std::to_string(EndVal->getType()->getTypeID()) << std::endl;

        if (EndVal->getType()->isDoubleTy()) {
            EndVal = Builder->CreateFPToSI(EndVal, Int32Ty, "end_int");
        }
    }

    // Check if both values are constants and validate the range
    if (ConstantInt *StartConst = dyn_cast<ConstantInt>(StartVal)) {
        if (ConstantInt *EndConst = dyn_cast<ConstantInt>(EndVal)) {
            int64_t startInt = StartConst->getSExtValue();
            int64_t endInt = EndConst->getSExtValue();

            if (endInt < startInt) {
                std::string errMsg = "For loop error: end value (" + std::to_string(endInt) +
                                     ") is less than start value (" + std::to_string(startInt) +
                                     ")";
                DEBUG_OUT << "ForExprAST::codegen: " << errMsg << std::endl;
                // ErrorHandler::printError(errMsg);
                return nullptr;
            }
        }
    }

    // Save previous value if variable already exists
    Value *OldVal = nullptr;
    if (NamedValues.find(VarName) != NamedValues.end()) {
        OldVal = NamedValues[VarName];
    }

    // Create alloca for loop variable
    AllocaInst *Alloca = CreateEntryBlockAlloca(TheFunction, Int32Ty, VarName);

    // Store initial value
    Builder->CreateStore(StartVal, Alloca);

    // Add loop variable to symbol table
    NamedValues[VarName] = Alloca;
    counters[VarName] = "int";

    // Create basic blocks
    BasicBlock *condLoopBB = BasicBlock::Create(*TheContext, "forcond", TheFunction);
    BasicBlock *bodyLoopBB = BasicBlock::Create(*TheContext, "forbody", TheFunction);
    BasicBlock *incLoopBB = BasicBlock::Create(*TheContext, "forinc", TheFunction);
    BasicBlock *afterLoopBB = BasicBlock::Create(*TheContext, "afterfor", TheFunction);

    // Branch to condition
    Builder->CreateBr(condLoopBB);

    // Generate condition block
    Builder->SetInsertPoint(condLoopBB);
    Value *currentVar = Builder->CreateLoad(Int32Ty, Alloca, VarName + ".val");
    Value *condition = Builder->CreateICmpSLE(currentVar, EndVal, "forcmp");
    Builder->CreateCondBr(condition, bodyLoopBB, afterLoopBB);

    // Generate body block
    Builder->SetInsertPoint(bodyLoopBB);
    Block->codegen();
    Builder->CreateBr(incLoopBB);

    // Generate increment block
    Builder->SetInsertPoint(incLoopBB);
    Value *currentVarInc = Builder->CreateLoad(Int32Ty, Alloca, VarName + ".val");
    Value *stepVal = Builder->CreateAdd(currentVarInc, Builder->getInt32(1), VarName + ".next");
    Builder->CreateStore(stepVal, Alloca);
    Builder->CreateBr(condLoopBB);

    // Continue after loop
    Builder->SetInsertPoint(afterLoopBB);

    // Restore previous value or remove from symbol table
    if (OldVal) {
        NamedValues[VarName] = OldVal;
    } else {
        NamedValues.erase(VarName);
    }
    counters.erase(VarName);

    return nullptr;
}

std::string ForExprAST::getText() {  // Ok
    return "ForExprAST";
}

InitStatus ForExprAST::containsInit() {  // Ok
    return this->Block->containsInit();
}

InitStatus ForExprAST::tryResolveInitStatus() {
    return this->Block->tryResolving();
}

Value *LetExprAST::codegen() {  // Fixed: proper let block handling with scope management
    Function *TheFunction = Builder->GetInsertBlock()->getParent();

    // Save previous values for scope restoration
    std::map<std::string, Value *> oldNamedValues;

    // Save current state for variables that might be shadowed
    for (size_t i = 0; i < Names.size(); ++i) {
        if (NamedValues.find(Names[i]) != NamedValues.end()) {
            oldNamedValues[Names[i]] = NamedValues[Names[i]];
        }
    }

    // Create allocas and initialize variables
    for (size_t i = 0; i < Names.size(); ++i) {
        // Create alloca at the beginning of function for better optimization
        AllocaInst *Alloca = CreateEntryBlockAlloca(TheFunction, Types[i], Names[i]);

        // Generate initial value
        Value *InitVal = Exprs[i]->codegen();
        if (!InitVal) {
            return LogErrorV(
                ("Failed to generate initial value for variable '" + Names[i] + "'").c_str());
        }

        // Type checking and conversion if needed
        if (InitVal->getType() != Types[i]) {
            // Try to convert types if possible
            if (Types[i]->isDoubleTy() && InitVal->getType()->isIntegerTy()) {
                InitVal = Builder->CreateSIToFP(InitVal, Types[i], Names[i] + "_conv");
            } else if (Types[i]->isIntegerTy() && InitVal->getType()->isDoubleTy()) {
                InitVal = Builder->CreateFPToSI(InitVal, Types[i], Names[i] + "_conv");
            } else if (InitVal->getType() != Types[i]) {
                // ErrorHandler::printError("Type mismatch for variable '" + Names[i] + "'");
                DEBUG_OUT << "LetExprAST::codegen: Type mismatch for variable '" << Names[i] << "'"
                          << std::endl;
                return nullptr;
            }
        }

        // Store initial value and add to symbol table
        Builder->CreateStore(InitVal, Alloca);
        NamedValues[Names[i]] = Alloca;
    }

    // Generate the body of the let block
    Value *BlockResult = Block->codegen();

    // Restore previous scope (cleanup)
    for (size_t i = 0; i < Names.size(); ++i) {
        // Remove from current scope
        NamedValues.erase(Names[i]);

        // Restore previous values if they existed
        if (oldNamedValues.find(Names[i]) != oldNamedValues.end()) {
            NamedValues[Names[i]] = oldNamedValues[Names[i]];
        }
    }

    return BlockResult;
}

std::string LetExprAST::getText() {  // Ok
    return "LetExprAST";
}

InitStatus LetExprAST::containsInit() {  // Ok
    return this->Block->containsInit();
}

InitStatus LetExprAST::tryResolveInitStatus() {
    return this->Block->tryResolving();
}

Value *CallExprAST::codegen() {  // Fixed: proper function call handling with init checks
    Function *CalleeF;

    bool isImported = false;
    std::string typeStr = "struct";
    int defLine = -1;

    if (FunctionProtos.find(Callee) == FunctionProtos.end()) {
        CalleeF = TheModule->getFunction(Callee);
        defLine = subFunctionsPositions[Callee];

        // if (isInitFunc) {
        //   std::string initStr = "init_" + Callee;
        //   if (std::find(subFunctions.begin(), subFunctions.end(), initStr) != subFunctions.end())
        //   {
        //     CalleeF = TheModule->getFunction(initStr);
        //     if (!CalleeF) {
        //       std::string errMsg = "Function '" + initStr + "' not found";
        //       return LogErrorV(errMsg.c_str());
        //     }
        //   } else {
        //     std::string errMsg = "Function '" + initStr + "' not found";
        //     return LogErrorV(errMsg.c_str());
        //   }
        // } else {
        //   std::string otherStr = "other_" + Callee;
        //   if (std::find(subFunctions.begin(), subFunctions.end(), otherStr) !=
        //   subFunctions.end()) {
        //     CalleeF = TheModule->getFunction(otherStr);
        //     if (!CalleeF) {
        //       std::string errMsg = "Function '" + otherStr + "' not found";
        //       return LogErrorV(errMsg.c_str());
        //     }
        //   } else {
        //     std::string errMsg = "Function '" + otherStr + "' not found";
        //     return LogErrorV(errMsg.c_str());
        //   }
        // }
    } else {
        CalleeF = TheModule->getFunction(FunctionProtos[Callee]);
        defLine = FunctionProtoPositions[Callee];
        typeStr = "extern";
        isImported = true;

        // if (initMemoryMap.find(FunctionProtos[Callee]) != initMemoryMap.end()) {
        //   isInitFunc = true;
        // }
    }

    if (!CalleeF) {
        std::string errMsg =
            "Function '" + Callee + "' from " + std::to_string(getPosition()) + " line not found";

        ErrorBase error(ErrorType::SYN2);
        error.setObjectDetail("call_stack_entry", std::make_unique<call_stack_entry>(
                                                      globalFaName, getPosition(), this->callPlace));

        ErrorHandler::printError(errMsg, std::move(error));
        return nullptr;
    }

    if (CalleeF->arg_size() != Args.size()) {
        std::string errMsg = "Function '" + Callee + "' from " + std::to_string(getPosition()) +
                             " line defined in " + std::to_string(defLine) + " line takes " +
                             std::to_string(CalleeF->arg_size()) + " arguments, but " +
                             std::to_string(Args.size()) + " were given";

        ErrorBase error(ErrorType::SYN3);
        error.setObjectDetail("call_stack_entry", std::make_unique<call_stack_entry>(
                                                      globalFaName, getPosition(), this->callPlace));
        error.setObjectDetail("cf",
                              std::make_unique<cf_struct>(Callee, typeStr, globalFaName, defLine));

        ErrorHandler::printError(errMsg, std::move(error));
        return nullptr;
    }

    std::string funcName = CalleeF->getName().str();

    DEBUG_OUT << "CallExprAST::codegen: " << funcName << std::endl;

    if (std::find(importFunctions.begin(), importFunctions.end(), funcName) !=
            importFunctions.end() &&
        CalleeF->isDeclaration()) {
        std::string errMsg = "Function '" + funcName + "' from " + std::to_string(getPosition()) +
                             " line is not defined";

        ErrorBase error(ErrorType::SYN2);
        error.setObjectDetail("call_stack_entry", std::make_unique<call_stack_entry>(
                                                      globalFaName, getPosition(), this->callPlace));

        ErrorHandler::printError(errMsg, std::move(error));
        return nullptr;
    }

    std::vector<Value *> ArgsV;
    int argCounter = 0;

    if (initMemoryMap.find(Callee) != initMemoryMap.end()) {
        isInitFuncAnalysis = true;
    } else {
        isInitFuncAnalysis = false;
    }

    for (const auto &Arg : Args) {
        Value *ArgV = Arg->codegen();
        std::string argName = Arg->getText();

        if (ArgV != nullptr) {
            // Successfully generated argument value
            ArgsV.push_back(ArgV);
        } else {
            // Argument didn't generate a value, check if it's a name object
            DEBUG_OUT << "CallExprAST::codegen: processing name argument " << argName << std::endl;

            // Check if it's a DF name object
            if (NameMap.find(argName) != NameMap.end()) {
                Value *nameObj = NameMap[argName];

                if (isInitFunc) {
                    // For init functions, check initialization rules
                    if (!initsNameMap[argName]) {
                        // Name not initialized yet - this init function should initialize it
                        if (subsNamesMap.find(tempSubName) != subsNamesMap.end()) {
                            std::vector<std::string> names = subsNamesMap[tempSubName];
                            if (std::find(names.begin(), names.end(), argName) != names.end()) {
                                if (isImported) {
                                    initsNameMap[argName] = true;
                                } else {
                                    // Check if this position should be initialized by this function
                                    if (initPosInSubs.find(Callee) != initPosInSubs.end()) {
                                        std::vector<int> positions = initPosInSubs[Callee];
                                        if (std::find(positions.begin(), positions.end(),
                                                      argCounter) != positions.end()) {
                                            initsNameMap[argName] = true;
                                        }
                                    }
                                }
                            }
                        }
                    } else {
                        // Name already initialized - check if this init function should handle it
                        if (!isImported) {
                            if (initPosInSubs.find(Callee) != initPosInSubs.end()) {
                                std::vector<int> positions = initPosInSubs[Callee];
                                if (std::find(positions.begin(), positions.end(), argCounter) !=
                                    positions.end()) {
                                    std::string errMsg = "Name '" + argName +
                                                         "' is already initialized in function '" +
                                                         funcName + "'";
                                    // ErrorHandler::printError(errMsg);
                                    DEBUG_OUT << "CallExprAST::codegen: " << errMsg << std::endl;
                                    return nullptr;
                                }
                            }
                        }
                    }
                } else {
                    // For non-init functions, name must be initialized
                    if (!initsNameMap[argName]) {
                        std::string errMsg = "Name '" + argName +
                                             "' is not initialized for function '" + funcName + "'";
                        // ErrorHandler::printError(errMsg);
                        DEBUG_OUT << "CallExprAST::codegen: " << errMsg << std::endl;
                        return nullptr;
                    }
                }

                ArgsV.push_back(nameObj);
            }
            // Check regular variables
            else if (initsNameMap.find(argName) != initsNameMap.end()) {
                // This is tracked but not a name object - handle legacy logic
                if (!initsNameMap[argName] && isInitFunc) {
                    if (subsNamesMap.find(tempSubName) != subsNamesMap.end()) {
                        std::vector<std::string> names = subsNamesMap[tempSubName];
                        if (std::find(names.begin(), names.end(), argName) != names.end()) {
                            if (isImported) {
                                initsNameMap[argName] = true;
                            } else {
                                if (initPosInSubs.find(Callee) != initPosInSubs.end()) {
                                    std::vector<int> positions = initPosInSubs[Callee];
                                    if (std::find(positions.begin(), positions.end(), argCounter) !=
                                        positions.end()) {
                                        initsNameMap[argName] = true;
                                    }
                                }
                            }
                        }
                    }
                    Value *tmpName = NameMap[argName];
                    ArgsV.push_back(tmpName);
                } else if (initsNameMap[argName] && !isInitFunc) {
                    ArgsV.push_back(NameMap[argName]);
                } else if (!initsNameMap[argName] && !isInitFunc) {
                    std::string errMsg = "'" + argName + "' is not initialized";
                    // ErrorHandler::printError(errMsg);
                    DEBUG_OUT << "CallExprAST::codegen: " << errMsg << std::endl;
                    return nullptr;
                } else {
                    Value *tmpName = NameMap[argName];
                    ArgsV.push_back(tmpName);
                }
            } else {
                std::string errMsg =
                    "Unknown argument '" + argName + "' in function '" + funcName + "'";
                // ErrorHandler::printError(errMsg);
                DEBUG_OUT << "CallExprAST::codegen: " << errMsg << std::endl;
                return nullptr;
            }
        }

        if (!ArgsV.back()) {
            std::string errMsg = "Failed to resolve argument " + std::to_string(argCounter) +
                                 " for function '" + funcName + "'";
            return LogErrorV(errMsg.c_str());
        }

        argCounter++;
    }

    // Set up parameter-to-argument name mapping for array access
    // This allows functions to access the caller's arrays through parameters
    if (!isImported &&
        std::find(subFunctions.begin(), subFunctions.end(), funcName) != subFunctions.end()) {
        // Get parameter names for this function
        if (subPosNameMap.find(Callee) != subPosNameMap.end()) {
            auto &posNameMap = subPosNameMap[Callee];

            // Map each parameter position to its argument name
            for (size_t i = 0; i < Args.size(); ++i) {
                std::string argName = Args[i]->getText();

                // Check if this position has a parameter name
                if (posNameMap.find(static_cast<int>(i)) != posNameMap.end()) {
                    std::string paramName = posNameMap[static_cast<int>(i)];

                    // Only map if the argument is a DF name (not a simple value)
                    if (NameMap.find(argName) != NameMap.end()) {
                        paramToActualNameMap[paramName] = argName;
                        DEBUG_OUT << "CallExprAST::codegen: Mapping parameter '" << paramName
                                  << "' to argument '" << argName << "'" << std::endl;
                    }
                }
            }
        }
    }

    FunctionType *FT = CalleeF->getFunctionType();

    for (size_t i = 0; i < ArgsV.size(); ++i) {
        Type *expectedType = FT->getParamType(i);
        Type *actualType = ArgsV[i]->getType();

        if (expectedType != actualType) {
            Value *convertedArg = nullptr;

            // // int -> double
            // if (expectedType->isDoubleTy() && actualType->isIntegerTy()) {
            //     convertedArg = Builder->CreateSIToFP(ArgsV[i], expectedType, "arg_conv");
            // }
            // // double -> int
            // else if (expectedType->isIntegerTy() && actualType->isDoubleTy()) {
            //     convertedArg = Builder->CreateFPToSI(ArgsV[i], expectedType, "arg_conv");
            // }
            // // int32 -> int64 или наоборот
            // else if (expectedType->isIntegerTy() && actualType->isIntegerTy()) {
            //     convertedArg = Builder->CreateIntCast(ArgsV[i], expectedType, true, "arg_conv");
            // }

            if (convertedArg) {
                ArgsV[i] = convertedArg;
                DEBUG_OUT << "CallExprAST::codegen: Converted argument " << i << " from type "
                          << actualType->getTypeID() << " to " << expectedType->getTypeID()
                          << std::endl;
            } else {
                std::string errMsg =
                    "Type mismatch for argument " + std::to_string(i) + " in function '" + Callee +
                    "' from " + std::to_string(getPosition()) + " line defined in " +
                    std::to_string(defLine) + " line: expected " + getTypeName(expectedType) +
                    " but got " + getTypeName(actualType);

                DEBUG_OUT << "CallExprAST::codegen: " << errMsg << std::endl;

                ErrorBase error(ErrorType::SYN1);
                error.setObjectDetail("call_stack_entry", std::make_unique<call_stack_entry>(
                                                              globalFaName, getPosition(), this->callPlace));
                error.setObjectDetail(
                    "cf", std::make_unique<cf_struct>(Callee, typeStr, globalFaName, defLine));

                ErrorHandler::printError(errMsg, std::move(error));
                return nullptr;
            }
        }
    }

    // maybe check argument types here (additional)
    Builder->CreateCall(CalleeF, ArgsV);

    // Clean up the parameter mapping after the call
    if (!isImported &&
        std::find(subFunctions.begin(), subFunctions.end(), funcName) != subFunctions.end()) {
        if (subPosNameMap.find(Callee) != subPosNameMap.end()) {
            auto &posNameMap = subPosNameMap[Callee];
            for (const auto &pair : posNameMap) {
                paramToActualNameMap.erase(pair.second);
            }
        }
    }

    return nullptr;
}

std::string CallExprAST::getText() {  // Ok
    return "CallExprAST";
}

InitStatus CallExprAST::containsInit() {  // Ok
    auto status = subsStatusMap.find(Callee);

    if (status != subsStatusMap.end()) {
        return status->second;
    } else {
        return InitStatus::FAILED;
    }
}

InitStatus CallExprAST::tryResolveInitStatus() {
    auto status = subsStatusMap.find(Callee);

    if (status != subsStatusMap.end()) {
        return status->second;
    }

    if (std::find(subsWhichTaged.begin(), subsWhichTaged.end(), Callee) == subsWhichTaged.end()) {
        std::string errMsg =
            "Function '" + Callee + "' from " + std::to_string(getPosition()) + " line not found";

        ErrorBase error(ErrorType::SYN2);
        error.setObjectDetail("call_stack_entry", std::make_unique<call_stack_entry>(
                                                      globalFaName, getPosition(), this->callPlace));

        ErrorHandler::printError(errMsg, std::move(error));
        subsWhichTaged.push_back(Callee);
    }

    return InitStatus::FAILED;
}

Value *BlockExprAST::codegen() {  // Fixed: proper variable lifetime management
    // Save previous values for scope restoration
    std::map<std::string, Value *> oldNamedValues;
    std::map<std::string, Value *> oldNameMap;
    std::map<std::string, bool> oldInitsNameMap;

    // Save current state for variables that might be shadowed
    for (const auto &DfDecl : DfDecls) {
        std::string varName = DfDecl->getText();
        if (NamedValues.find(varName) != NamedValues.end()) {
            oldNamedValues[varName] = NamedValues[varName];
        }
        if (NameMap.find(varName) != NameMap.end()) {
            oldNameMap[varName] = NameMap[varName];
        }
        if (initsNameMap.find(varName) != initsNameMap.end()) {
            oldInitsNameMap[varName] = initsNameMap[varName];
        }
    }

    // Create allocas for declared variables in current block
    for (const auto &DfDecl : DfDecls) {
        std::string varName = DfDecl->getText();

        // Use getNameValue which handles array creation for dimension=1
        Value *dfObject = getNameValue(varName);

        // Store in maps (getNameValue already does this, but let's keep it for clarity)
        NamedValues[varName] = dfObject;
        // NameMap and initsNameMap are already set by getNameValue

        // Add to current subroutine's variable list if we're in a subroutine
        if (subsNamesMap.find(tempSubName) == subsNamesMap.end()) {
            subsNamesMap[tempSubName] = {varName};
        } else {
            std::vector<std::string> names = subsNamesMap[tempSubName];
            names.push_back(varName);
            subsNamesMap[tempSubName] = names;
        }
    }

    // Process initialization statements
    for (const auto &InitName : InitNames) {
        InitName->codegen();
    }

    // Process all statements in the block
    for (const auto &Statement : Statements) {
        Statement->codegen();
    }

    // Restore previous scope (cleanup)
    for (const auto &DfDecl : DfDecls) {
        std::string varName = DfDecl->getText();

        // Remove from current scope
        NamedValues.erase(varName);
        NameMap.erase(varName);
        initsNameMap.erase(varName);

        // Restore previous values if they existed
        if (oldNamedValues.find(varName) != oldNamedValues.end()) {
            NamedValues[varName] = oldNamedValues[varName];
        }
        if (oldNameMap.find(varName) != oldNameMap.end()) {
            NameMap[varName] = oldNameMap[varName];
        }
        if (oldInitsNameMap.find(varName) != oldInitsNameMap.end()) {
            initsNameMap[varName] = oldInitsNameMap[varName];
        }
    }

    return nullptr;
}

std::string BlockExprAST::getText() {  // Ok
    return "BlockExprAST";
}

InitStatus BlockExprAST::containsInit() {  // Ok
    if (this->InitNames.size() > 0) {
        return InitStatus::INIT;
    } else {
        return InitStatus::NORMAL;
    }
}

bool BlockExprAST::containsUndef() {  // Ok
    return UndefStatements.size() > 0;
}

InitStatus BlockExprAST::tryResolving() {
    for (auto it = UndefStatements.begin(); it != UndefStatements.end();) {
        InitStatus status = (*it)->tryResolveInitStatus();

        if (status == InitStatus::INIT) {
            this->InitNames.push_back(std::move(*it));
            it = UndefStatements.erase(it);
        } else if (status == InitStatus::NORMAL) {
            this->Statements.push_back(std::move(*it));
            it = UndefStatements.erase(it);
        } else if (status == InitStatus::FAILED) {
            it = UndefStatements.erase(it);
        } else {
            ++it;
        }
    }

    if (UndefStatements.size() > 0) {
        return InitStatus::UNKNOWN;
    } else if (this->InitNames.size() > 0) {
        return InitStatus::INIT;
    } else {
        return InitStatus::NORMAL;
    }
}

Value *SubExprAST::codegen() {  // Fixed: proper argument lifetime management
    Function *F = TheModule->getFunction(Name);
    if (!F) {
        return LogErrorV("Function not found during codegen");
    }

    // Save current context
    tempSubName = Name;
    std::map<std::string, Value *> oldNamedValues;
    std::vector<std::string> oldParamsVec = ParamsVec;

    // Save any existing values that might be shadowed
    for (size_t i = 0; i < Names.size() && i < F->arg_size(); ++i) {
        if (NamedValues.find(Names[i]) != NamedValues.end()) {
            oldNamedValues[Names[i]] = NamedValues[Names[i]];
        }
    }

    // Create entry block and set insertion point
    BasicBlock *BB = BasicBlock::Create(*TheContext, "entry", F);
    Builder->SetInsertPoint(BB);

    // Create allocas for function arguments to enable mutable local variables
    for (auto &arg : F->args()) {
        std::string argName = Names[arg.getArgNo()];

        // Create alloca for the argument
        AllocaInst *alloca = CreateEntryBlockAlloca(F, arg.getType(), argName + ".addr");

        // Store the argument value in the alloca
        Builder->CreateStore(&arg, alloca);

        // Add to symbol table
        NamedValues[argName] = alloca;

        // Mark parameter as available for reference operations
        if (std::find(NamePositions.begin(), NamePositions.end(), arg.getArgNo()) !=
            NamePositions.end()) {
            // This is a reference parameter - store the original argument pointer
            NameMap[argName] = &arg;
            initsNameMap[argName] = true;  // Reference parameters are always "initialized"
        }
    }

    // Update ParamsVec with current function parameters
    for (const auto &pos : NamePositions) {
        if (pos < static_cast<int>(Names.size())) {
            ParamsVec.push_back(Names[pos]);
        }
    }

    // Generate code for function body
    Block->codegen();

    // Create return instruction
    Builder->CreateRetVoid();

    // Verify the function (additional)
    if (g_debugEnabled && llvm::verifyFunction(*F, &llvm::errs())) {
        F->print(llvm::errs());
        return LogErrorV("Function verification failed");
    }

    // Cleanup: restore previous context
    ParamsVec = oldParamsVec;

    // Remove function arguments from symbol table
    for (auto &arg : F->args()) {
        std::string argName = Names[arg.getArgNo()];
        NamedValues.erase(argName);
        NameMap.erase(argName);
        initsNameMap.erase(argName);

        // Remove from dfArrayMap if exists
        dfArrayMap.erase(argName);
    }

    // Restore any previously shadowed values
    for (const auto &pair : oldNamedValues) {
        NamedValues[pair.first] = pair.second;
    }

    return F;
}

std::string SubExprAST::getText() {  // Ok
    return "SubExprAST";
}

InitStatus SubExprAST::containsInit() {  // Ok
    return this->Block->containsInit();
}

InitStatus SubExprAST::tryResolveInitStatus() {
    return this->Block->tryResolving();
}

Value *ImportExprAST::codegen() {  // Ok
    Type *retType = Type::getVoidTy(*TheContext);
    FunctionType *funcType = FunctionType::get(retType, Params, false);

    Function *func_from_c = TheModule->getFunction(CFuncName);

    if (func_from_c) {
        size_t importArgsCount = Params.size();  // Количество в ImportExprAST
        size_t functionArgsCount = func_from_c->arg_size();  // Количество в Function

        if (importArgsCount != functionArgsCount) {
            std::string errMsg = "Function '" + CFuncName + "' from " +
                                 std::to_string(getPosition()) +
                                 " line has different argument count: expected " +
                                 std::to_string(functionArgsCount) +
                                 ", but import declaration has " + std::to_string(importArgsCount);

            ErrorBase error(ErrorType::SYN12);
            error.setObjectDetail("cf", std::make_unique<cf_struct>(LunaCodeID, "extern",
                                                                    globalFaName, getPosition()));

            ErrorHandler::printError(errMsg, std::move(error));
            return nullptr;
        }

        FunctionType *existingFuncType = func_from_c->getFunctionType();

        if (existingFuncType != funcType) {
            std::string errMsg = "Function '" + CFuncName + "' from " +
                                 std::to_string(getPosition()) + " line has a different signature";

            ErrorBase error(ErrorType::SYN12);
            error.setObjectDetail("cf", std::make_unique<cf_struct>(LunaCodeID, "extern",
                                                                    globalFaName, getPosition()));

            ErrorHandler::printError(errMsg, std::move(error));
            return nullptr;
        }

        importFunctions.push_back(CFuncName);
        return nullptr;
    } else {
        std::string errMsg = "Function '" + CFuncName + "' from " + std::to_string(getPosition()) +
                             " line not found in module";

        ErrorBase error(ErrorType::SYN10);
        error.setObjectDetail(
            "cf", std::make_unique<cf_struct>(LunaCodeID, "extern", globalFaName, getPosition()));

        ErrorHandler::printError(errMsg, std::move(error));
        return nullptr;
    }

    // Try to create the function (maybe it is not needed)

    Function *func =
        Function::Create(funcType, Function::ExternalLinkage, CFuncName, TheModule.get());

    for (auto &arg : func->args()) {
        arg.addAttr(Attribute::NoUndef);
    }

    for (size_t i = 0; i < NamePositions.size(); ++i) {
        Argument *arg = func->getArg(NamePositions[i]);
        AttrBuilder AB(*TheContext);
        AB.addAttribute(Attribute::NonNull);
        AB.addAlignmentAttr(8);
        AB.addDereferenceableAttr(36);
        arg->addAttrs(AB);
    }

    importFunctions.push_back(CFuncName);
    return nullptr;
}

std::string ImportExprAST::getText() {  // Ok
    return "ImportExprAST";
}

InitStatus ImportExprAST::containsInit() {  // Ok
    return InitStatus::NORMAL;
}

Value *ProgramExprAST::codegen() {  // Ok
    for (const auto &Import : Imports) {
        Import->codegen();
    }

    for (const auto &Sub : Subs) {
        if (!Sub) {
            return LogErrorV("Null subroutine in program");
        }

        Sub->codegen();
    }

    return nullptr;
}

std::string ProgramExprAST::getText() {  // Ok
    return "ProgramExprAST";
}

InitStatus ProgramExprAST::containsInit() {  // Ok
    return InitStatus::NORMAL;
}

Value *IntCastExprAST::codegen() {  // NotUsed
    Value *V = Expr->codegen();

    if (!V) {
        return nullptr;
    }

    return Builder->CreateFPToSI(V, Type::getInt32Ty(*TheContext), "fptosi");
}

std::string IntCastExprAST::getText() {  // NotUsed
    return "IntCastExprAST";
}

InitStatus IntCastExprAST::containsInit() {  // NotUsed
    return InitStatus::NORMAL;
}

Value *RealCastExprAST::codegen() {  // NotUsed
    Value *V = Expr->codegen();

    if (!V) {
        return nullptr;
    }

    return Builder->CreateSIToFP(V, Type::getDoubleTy(*TheContext), "sitofp");
}

std::string RealCastExprAST::getText() {  // NotUsed
    return "RealCastExprAST";
}

InitStatus RealCastExprAST::containsInit() {  // NotUsed
    return InitStatus::NORMAL;
}

Value *StringCastExprAST::codegen() {  // NotUsed
    Value *V = Expr->codegen();

    if (!V) {
        return nullptr;
    }

    Type *inputType = V->getType();
    Type *i8PtrType = Type::getInt8PtrTy(*TheContext);

    FunctionType *sprintfType =
        FunctionType::get(Type::getInt32Ty(*TheContext), {i8PtrType, i8PtrType, inputType}, true);

    Function *sprintfFunc =
        Function::Create(sprintfType, Function::ExternalLinkage, "sprintf", TheModule.get());

    Value *buffer = Builder->CreateAlloca(ArrayType::get(Type::getInt8Ty(*TheContext), 32));

    Value *formatStr;

    if (inputType->isIntegerTy()) {
        formatStr = Builder->CreateGlobalStringPtr("%d", "intFormat");
    } else if (inputType->isDoubleTy()) {
        formatStr = Builder->CreateGlobalStringPtr("%f", "doubleFormat");
    } else {
        // ErrorHandler::printError("Invalid type for string cast");
        DEBUG_OUT << "StringCastExprAST::codegen: Invalid type for string cast" << std::endl;
        return nullptr;
    }

    Builder->CreateCall(sprintfFunc, {buffer, formatStr, V});

    return buffer;
}

std::string StringCastExprAST::getText() {  // NotUsed
    return "StringCastExprAST";
}

InitStatus StringCastExprAST::containsInit() {  // NotUsed
    return InitStatus::NORMAL;
}

Value *TernaryExprAST::codegen() {  // NotUsed
    Value *CondV = Cond->codegen();

    if (!CondV) {
        return nullptr;
    }

    CondV = Builder->CreateICmpNE(CondV, ConstantInt::get(*TheContext, APInt(1, 0)), "ifcond");

    Function *TheFunction = Builder->GetInsertBlock()->getParent();
    BasicBlock *ThenBB = BasicBlock::Create(*TheContext, "then", TheFunction);
    BasicBlock *ElseBB = BasicBlock::Create(*TheContext, "else", TheFunction);
    BasicBlock *MergeBB = BasicBlock::Create(*TheContext, "ifcont", TheFunction);

    Builder->CreateCondBr(CondV, ThenBB, ElseBB);

    Builder->SetInsertPoint(ThenBB);
    Value *ThenV = True->codegen();

    if (!ThenV) {
        return nullptr;
    }

    Builder->CreateBr(MergeBB);
    // TheFunction->insert(TheFunction->end(), MergeBB);
    TheFunction->getBasicBlockList().push_back(MergeBB);
    Builder->SetInsertPoint(ElseBB);

    Value *ElseV = False->codegen();

    if (!ElseV) {
        return nullptr;
    }

    Builder->CreateBr(MergeBB);
    // TheFunction->insert(TheFunction->end(), MergeBB);
    TheFunction->getBasicBlockList().push_back(MergeBB);
    Builder->SetInsertPoint(MergeBB);

    return nullptr;
}

std::string TernaryExprAST::getText() {  // NotUsed
    return "TernaryExprAST";
}

InitStatus TernaryExprAST::containsInit() {  // NotUsed
    return InitStatus::NORMAL;
}
