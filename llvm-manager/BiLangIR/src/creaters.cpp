#include "creaters.hpp"

#include "codegen.hpp"
#include "debug.hpp"
#include "depends.hpp"
#include "error_handler.hpp"
#include "error_nodes.hpp"
#include "nodes.hpp"

// #include "/home/pyro/Workdir/practice/luna/include/df.h"
// #include "codegen.hpp"
// #include "ucenv.h"

#include <iostream>
#include <memory>
#include <string>
#include <vector>

namespace {
// Кэшируем типы, чтобы не создавать их повторно
static StructType *crSerializableType = nullptr;
static StructType *crDFType = nullptr;

enum class NumberType { INTEGER, FLOAT, STRING };

NumberType determineType(const std::string &str) {
    if (str.empty()) {
        return NumberType::STRING;
    }

    bool hasDecimal = false;
    bool hasDigits = false;
    size_t start = 0;

    if (str[0] == '-' || str[0] == '+') {
        if (str.length() == 1) {
            return NumberType::STRING;
        }

        start = 1;
    }

    for (size_t i = start; i < str.length(); ++i) {
        if (std::isdigit(str[i])) {
            hasDigits = true;
        } else if (str[i] == '.' && !hasDecimal) {
            hasDecimal = true;
        } else {
            return NumberType::STRING;
        }
    }

    if (!hasDigits) {
        return NumberType::STRING;
    }

    return hasDecimal ? NumberType::FLOAT : NumberType::INTEGER;
}

Type *getNameType() {
    if (crDFType != nullptr) {
        return PointerType::get(crDFType, 0);  // Возвращаем указатель на DF
    }

    // Сначала пытаемся найти существующий тип в модуле
    crDFType = StructType::getTypeByName(*TheContext, "class.DF");

    if (crDFType != nullptr) {
        DEBUG_PRINT("Found existing class.DF type in module for create");
        return PointerType::get(crDFType, 0);  // Возвращаем указатель на DF
    }

    // Если тип не найден, создаем его
    DEBUG_PRINT("Creating new class.DF type for create");

    // Вариант с Serializable
    // %class.DF = type <{ %class.Serializable, ptr, i64, ptr, i32, [4 x i8] }>

    // Сначала создаем тип для базового класса Serializable (содержит vtable pointer)

    if (crSerializableType == nullptr) {
        std::vector<Type *> serializableElements = {
            PointerType::get(Type::getInt8Ty(*TheContext), 0)  // vtable pointer
        };
        crSerializableType =
            StructType::create(*TheContext, serializableElements, "class.Serializable");
    }

    // Теперь создаем полный тип для класса DF
    std::vector<Type *> dfElements = {
        crSerializableType,                                 // %class.Serializable base
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

    crDFType = StructType::create(*TheContext, dfElements, "class.DF");
    return PointerType::get(crDFType, 0);  // Возвращаем указатель на DF
}

std::string getValue(const json &data) {
    std::string tmpKey, tmpValue;

    for (auto it = data.begin(); it != data.end(); ++it) {
        tmpKey = it.key();

        if (tmpKey == "value") {
            tmpValue = it.value();
            return tmpValue;
        }
    }

    return "";
};

int getLine(const json &data) {
    std::string tmpKey;
    int tmpValue = -1;

    for (auto it = data.begin(); it != data.end(); ++it) {
        tmpKey = it.key();

        if (tmpKey == "line") {
            tmpValue = it.value();
            return tmpValue;
        }
    }

    return -1;
};

std::unique_ptr<ExprAST> chooseStatement(const json &data, bool initMode, std::string subName,
                                         std::vector<std::string> subArgs) {
    std::unique_ptr<ExprAST> statement;
    std::string tmpStr;

    for (auto it = data.begin(); it != data.end(); ++it) {
        tmpStr = it.key();

        if (tmpStr == "cf_statement") {  // Ok
            statement = createCallStatementNode(it.value(), initMode, subName, subArgs);
        } else if (tmpStr == "for_statement") {  // Ok
            statement = createForStatementNode(it.value(), initMode, subName, subArgs);
        } else if (tmpStr == "let_statement") {  // Ok
            statement = createLetStatementNode(it.value(), initMode, subName, subArgs);
        } else if (tmpStr == "while_statement") {  // Ok
            statement = createWhileStatementNode(it.value(), initMode, subName, subArgs);
        } else if (tmpStr == "if_statement") {  // Ok
            statement = createIfStatementNode(it.value(), initMode, subName, subArgs);
        }
    }

    // return std::move(statement);
    return statement;
}

std::pair<std::unique_ptr<CheckableExprAST>, InitStatus>
altChooseStatement(const json &data, std::string subName, std::vector<std::string> subArgs) {
    std::pair<std::unique_ptr<CheckableExprAST>, InitStatus> altStatement;
    std::string tmpStr;

    for (auto it = data.begin(); it != data.end(); ++it) {
        tmpStr = it.key();

        if (tmpStr == "cf_statement") {  // Ok
            altStatement = altCreateCallStatementNode(it.value());
        } else if (tmpStr == "for_statement") {  // Ok
            altStatement = altCreateForStatementNode(it.value(), subName, subArgs);
        } else if (tmpStr == "let_statement") {  // Ok
            altStatement = altCreateLetStatementNode(it.value(), subName, subArgs);
        } else if (tmpStr == "while_statement") {  // Ok
            altStatement = altCreateWhileStatementNode(it.value(), subName, subArgs);
        } else if (tmpStr == "if_statement") {  // Ok
            altStatement = altCreateIfStatementNode(it.value(), subName, subArgs);
        }
    }

    return altStatement;
}

std::vector<Type *> getParams(const json &data, std::vector<int> &positions) {
    std::vector<Type *> params;
    std::string tmpKey, tmpValue;

    for (auto it = data.begin(); it != data.end(); ++it) {
        auto tmpParam = it.value();

        for (const auto &item : tmpParam) {
            for (auto it1 = item.begin(); it1 != item.end(); ++it1) {
                tmpKey = it1.key();

                if (tmpKey == "type") {
                    tmpValue = getValue(it1.value());

                    if (tmpValue == "int") {
                        params.push_back(Type::getInt32Ty(*TheContext));
                    } else if (tmpValue == "real") {
                        params.push_back(Type::getDoubleTy(*TheContext));
                    } else if (tmpValue == "string") {
                        params.push_back(PointerType::get(Type::getInt8Ty(*TheContext), 0));
                    } else if (tmpValue == "name" || tmpValue == "value") {
                        positions.push_back(params.size());
                        params.push_back(getNameType());
                    }
                }
            }
        }
    }

    return params;
};

std::vector<Type *> getSubParams(const json &data, std::vector<std::string> &names,
                                 std::vector<int> &positions) {
    std::vector<Type *> params;
    std::string tmpKey, tmpValue;

    for (auto it = data.begin(); it != data.end(); ++it) {
        auto tmpParam = it.value();

        for (const auto &item : tmpParam) {
            for (auto it1 = item.begin(); it1 != item.end(); ++it1) {
                tmpKey = it1.key();

                if (tmpKey == "param") {
                    auto tmpParam1 = it1.value();

                    for (auto it2 = tmpParam1.begin(); it2 != tmpParam1.end(); ++it2) {
                        tmpKey = it2.key();

                        if (tmpKey == "type") {
                            tmpValue = getValue(it2.value());

                            if (tmpValue == "int") {
                                params.push_back(Type::getInt32Ty(*TheContext));
                            } else if (tmpValue == "real") {
                                params.push_back(Type::getDoubleTy(*TheContext));
                            } else if (tmpValue == "string") {
                                params.push_back(PointerType::get(Type::getInt8Ty(*TheContext), 0));
                            } else if (tmpValue == "name" || tmpValue == "value") {
                                positions.push_back(params.size());
                                params.push_back(getNameType());
                            }
                        } else if (tmpKey == "name") {
                            names.push_back(getValue(it2.value()));
                        }
                    }
                }
            }
        }
    }

    std::vector<std::string> paramNames;

    for (const auto &pos : positions) {
        paramNames.push_back(names[pos]);
        subDimensionsIncreaseMap[tempSubName][names[pos]] = 0;
        subPosNameMap[tempSubName][pos] = names[pos];
    }

    subNamesParamsMap[tempSubName] = paramNames;

    return params;
}

std::vector<std::unique_ptr<ExprAST>> getNameSeq(const json &data) {
    std::vector<std::unique_ptr<ExprAST>> nameSeq;
    std::string tmpStr;
    std::vector<std::string> subNames;

    for (auto it = data.begin(); it != data.end(); ++it) {
        auto tmpDfDecl = it.value();

        for (const auto &item : tmpDfDecl) {
            for (auto it1 = item.begin(); it1 != item.end(); ++it1) {
                tmpStr = it1.key();

                if (tmpStr == "name") {
                    auto name = getValue(it1.value());
                    int tmpLine = getLine(it1.value());

                    if (NameMap.find(name) != NameMap.end()) {
                        std::string errMsg = "Duplicate name '" + name + "' in " +
                                             std::to_string(tmpLine) + " line found";

                        ErrorBase errorDetails(ErrorType::SYN81);
                        call_stack_entry cse(globalFaName, tmpLine, tempSubName);
                        // call_stack cs({cse});
                        errorDetails.setObjectDetail(
                            "df", std::make_unique<df_struct>(name, cse));

                        ErrorHandler::printError(std::move(errMsg), std::move(errorDetails));
                        continue;
                    }

                    NameMap[name] = nullptr;
                    // initsNameMap[name] = false;
                    subNames.push_back(name);
                    subDimensionsIncreaseMap[tempSubName][name] = 0;

                    nameSeq.push_back(std::make_unique<NameExprAST>(name, tmpLine));
                }
            }
        }
    }

    subsNamesMap[tempSubName] = subNames;

    return nameSeq;
}

std::unique_ptr<ExprAST> chooseID(const json &data) {
    std::unique_ptr<ExprAST> id, expr;
    std::unique_ptr<ExprAST> left, right;
    std::string tmpStr;

    int dataLine = data.value("line", -1);

    for (auto it = data.begin(); it != data.end(); ++it) {
        tmpStr = it.key();
        DEBUG_OUT << "chooseID: Key: " << tmpStr << std::endl;

        if (tmpStr == "line") {
            continue;
        }

        if (tmpStr == "complex_id") {  // OK
            auto tmpArg = it.value();

            for (auto it1 = tmpArg.begin(); it1 != tmpArg.end(); ++it1) {
                tmpStr = it1.key();

                if (tmpStr == "id") {
                    id = chooseID(it1.value());
                } else if (tmpStr == "expr") {
                    expr = chooseID(it1.value());
                }
            }

            int tmpPos = id->getPosition();

            return std::make_unique<ComplexExprAST>(std::move(id), std::move(expr), "not_generated",
                                                    tmpPos);
        } else if (tmpStr == "simple_id") {  // OK
            return std::make_unique<VariableExprAST>(getValue(it.value()), getLine(it.value()));
        } else if (tmpStr == "value") {  // OK
            std::string value = it.value();

            DEBUG_OUT << "chooseID: Value: " << value << std::endl;

            switch (determineType(value)) {
            case NumberType::INTEGER:
                return std::make_unique<IntExprAST>(std::stoi(value), dataLine);
            case NumberType::FLOAT:
                return std::make_unique<RealExprAST>(std::stod(value), dataLine);
            case NumberType::STRING:
                return std::make_unique<StringExprAST>(value, dataLine);
            }
        } else if (tmpStr == "sum") {  // OK
            auto tmpArg = it.value();
            std::string side;

            for (auto it1 = tmpArg.begin(); it1 != tmpArg.end(); ++it1) {
                side = it1.key();

                DEBUG_OUT << "chooseID: Side: " << side << std::endl;

                if (side == "left") {
                    left = chooseID(it1.value());
                } else if (side == "right") {
                    right = chooseID(it1.value());
                }
            }

            int tmpPos = left->getPosition();

            return std::make_unique<BinaryExprAST>("+", std::move(left), std::move(right), tmpPos);
        } else if (tmpStr == "sub") {  // Ok
            auto tmpArg = it.value();
            std::string side;

            for (auto it1 = tmpArg.begin(); it1 != tmpArg.end(); ++it1) {
                side = it1.key();

                DEBUG_OUT << "chooseID: Side: " << side << std::endl;

                if (side == "left") {
                    left = chooseID(it1.value());
                } else if (side == "right") {
                    right = chooseID(it1.value());
                }
            }

            int tmpPos = left->getPosition();

            return std::make_unique<BinaryExprAST>("-", std::move(left), std::move(right), tmpPos);
        } else if (tmpStr == "mul") {  // Ok
            auto tmpArg = it.value();
            std::string side;

            for (auto it1 = tmpArg.begin(); it1 != tmpArg.end(); ++it1) {
                side = it1.key();

                DEBUG_OUT << "chooseID: Side: " << side << std::endl;

                if (side == "left") {
                    left = chooseID(it1.value());
                } else if (side == "right") {
                    right = chooseID(it1.value());
                }
            }

            int tmpPos = left->getPosition();

            return std::make_unique<BinaryExprAST>("*", std::move(left), std::move(right), tmpPos);
        } else if (tmpStr == "div") {  // Ok
            auto tmpArg = it.value();
            std::string side;

            for (auto it1 = tmpArg.begin(); it1 != tmpArg.end(); ++it1) {
                side = it1.key();

                DEBUG_OUT << "chooseID: Side: " << side << std::endl;

                if (side == "left") {
                    left = chooseID(it1.value());
                } else if (side == "right") {
                    right = chooseID(it1.value());
                }
            }

            int tmpPos = left->getPosition();

            return std::make_unique<BinaryExprAST>("/", std::move(left), std::move(right), tmpPos);
        } else if (tmpStr == "mod") {  // Ok
            auto tmpArg = it.value();
            std::string side;

            for (auto it1 = tmpArg.begin(); it1 != tmpArg.end(); ++it1) {
                side = it1.key();

                DEBUG_OUT << "chooseID: Side: " << side << std::endl;

                if (side == "left") {
                    left = chooseID(it1.value());
                } else if (side == "right") {
                    right = chooseID(it1.value());
                }
            }

            int tmpPos = left->getPosition();

            return std::make_unique<BinaryExprAST>("%", std::move(left), std::move(right), tmpPos);
        } else if (tmpStr == "to_int") {
            std::unique_ptr<ExprAST> resExpr = chooseID(it.value());
            int tmpPos = resExpr->getPosition();
            return std::make_unique<IntCastExprAST>(std::move(resExpr), tmpPos);
        } else if (tmpStr == "to_real") {
            std::unique_ptr<ExprAST> resExpr = chooseID(it.value());
            int tmpPos = resExpr->getPosition();
            return std::make_unique<RealCastExprAST>(std::move(resExpr), tmpPos);
        }
    }

    return nullptr;
}

std::unique_ptr<ExprAST> chooseLogicExpr(const json &data) {
    std::unique_ptr<ExprAST> left, right;
    std::string tmpStr;

    int dataLine = data.value("line", -1);

    for (auto it = data.begin(); it != data.end(); ++it) {
        tmpStr = it.key();

        DEBUG_OUT << "chooseLogicExpr: Key: " << tmpStr << std::endl;

        if (tmpStr == "line") {
            continue;
        }

        if (tmpStr == "neq") {  // Ok
            auto tmpArg = it.value();
            std::string side;

            for (auto it1 = tmpArg.begin(); it1 != tmpArg.end(); ++it1) {
                side = it1.key();

                DEBUG_OUT << "chooseLogicExpr: Side: " << side << std::endl;

                if (side == "left") {
                    left = chooseID(it1.value());
                } else if (side == "right") {
                    right = chooseID(it1.value());
                }
            }

            int tmpPos = left->getPosition();

            return std::make_unique<BinaryExprAST>("!=", std::move(left), std::move(right), tmpPos);
        } else if (tmpStr == "dbleq") {  // Ok
            auto tmpArg = it.value();
            std::string side;

            for (auto it1 = tmpArg.begin(); it1 != tmpArg.end(); ++it1) {
                side = it1.key();

                DEBUG_OUT << "chooseLogicExpr: Side: " << side << std::endl;

                if (side == "left") {
                    left = chooseID(it1.value());
                } else if (side == "right") {
                    right = chooseID(it1.value());
                }
            }

            int tmpPos = left->getPosition();

            return std::make_unique<BinaryExprAST>("==", std::move(left), std::move(right), tmpPos);
        } else if (tmpStr == "lt") {  // Ok
            auto tmpArg = it.value();
            std::string side;

            for (auto it1 = tmpArg.begin(); it1 != tmpArg.end(); ++it1) {
                side = it1.key();

                DEBUG_OUT << "chooseLogicExpr: Side: " << side << std::endl;

                if (side == "left") {
                    left = chooseID(it1.value());
                } else if (side == "right") {
                    right = chooseID(it1.value());
                }
            }

            int tmpPos = left->getPosition();

            return std::make_unique<BinaryExprAST>("<", std::move(left), std::move(right), tmpPos);
        } else if (tmpStr == "gt") {  // Ok
            auto tmpArg = it.value();
            std::string side;

            for (auto it1 = tmpArg.begin(); it1 != tmpArg.end(); ++it1) {
                side = it1.key();

                DEBUG_OUT << "chooseLogicExpr: Side: " << side << std::endl;

                if (side == "left") {
                    left = chooseID(it1.value());
                } else if (side == "right") {
                    right = chooseID(it1.value());
                }
            }

            int tmpPos = left->getPosition();

            return std::make_unique<BinaryExprAST>(">", std::move(left), std::move(right), tmpPos);
        } else if (tmpStr == "leq") {  // Ok
            auto tmpArg = it.value();
            std::string side;

            for (auto it1 = tmpArg.begin(); it1 != tmpArg.end(); ++it1) {
                side = it1.key();

                DEBUG_OUT << "chooseLogicExpr: Side: " << side << std::endl;

                if (side == "left") {
                    left = chooseID(it1.value());
                } else if (side == "right") {
                    right = chooseID(it1.value());
                }
            }

            int tmpPos = left->getPosition();

            return std::make_unique<BinaryExprAST>("<=", std::move(left), std::move(right), tmpPos);
        } else if (tmpStr == "geq") {  // Ok
            auto tmpArg = it.value();
            std::string side;

            for (auto it1 = tmpArg.begin(); it1 != tmpArg.end(); ++it1) {
                side = it1.key();

                DEBUG_OUT << "chooseLogicExpr: Side: " << side << std::endl;

                if (side == "left") {
                    left = chooseID(it1.value());
                } else if (side == "right") {
                    right = chooseID(it1.value());
                }
            }

            int tmpPos = left->getPosition();

            return std::make_unique<BinaryExprAST>(">=", std::move(left), std::move(right), tmpPos);
        } else if (tmpStr == "dblamp") {  // Ok
            auto tmpArg = it.value();
            std::string side;

            for (auto it1 = tmpArg.begin(); it1 != tmpArg.end(); ++it1) {
                side = it1.key();

                DEBUG_OUT << "chooseLogicExpr: Side: " << side << std::endl;

                if (side == "left") {
                    left = chooseLogicExpr(it1.value());
                } else if (side == "right") {
                    right = chooseLogicExpr(it1.value());
                }
            }

            int tmpPos = left->getPosition();

            return std::make_unique<BinaryExprAST>("&&", std::move(left), std::move(right), tmpPos);
        } else if (tmpStr == "dblpipe") {  // Ok
            auto tmpArg = it.value();
            std::string side;

            for (auto it1 = tmpArg.begin(); it1 != tmpArg.end(); ++it1) {
                side = it1.key();

                DEBUG_OUT << "chooseLogicExpr: Side: " << side << std::endl;

                if (side == "left") {
                    left = chooseLogicExpr(it1.value());
                } else if (side == "right") {
                    right = chooseLogicExpr(it1.value());
                }
            }

            int tmpPos = left->getPosition();

            return std::make_unique<BinaryExprAST>("||", std::move(left), std::move(right), tmpPos);
        } else if (tmpStr == "value") {
            std::string value = it.value();

            DEBUG_OUT << "chooseLogicExpr: Value: " << value << std::endl;

            if (value == "true" || value == "True" || value == "TRUE" || value == "1") {
                return std::make_unique<IntExprAST>(1, dataLine);
            } else if (value == "false" || value == "False" || value == "FALSE" || value == "0") {
                return std::make_unique<IntExprAST>(0, dataLine);
            } else {
                // ErrorHandler::printError("Invalid boolean value '" + value + "'",
                //                          ErrorBase(ErrorType::MY1));
                return std::make_unique<IntExprAST>(1, dataLine);
            }
        }
    }

    return nullptr;
}

std::unique_ptr<ExprAST> chooseLogicExprIter(const json::const_iterator &it) {
    std::unique_ptr<ExprAST> left, right;
    std::string tmpStr;

    // for (auto it = data.begin(); it != data.end(); ++it) {
    tmpStr = it.key();

    DEBUG_OUT << "chooseLogicExprIter: Key: " << tmpStr << std::endl;

    if (tmpStr == "neq") {  // Ok
        auto tmpArg = it.value();
        std::string side;

        for (auto it1 = tmpArg.begin(); it1 != tmpArg.end(); ++it1) {
            side = it1.key();

            DEBUG_OUT << "chooseLogicExprIter: Side: " << side << std::endl;

            if (side == "left") {
                left = chooseID(it1.value());
            } else if (side == "right") {
                right = chooseID(it1.value());
            }
        }

        int tmpPos = left->getPosition();

        return std::make_unique<BinaryExprAST>("!=", std::move(left), std::move(right), tmpPos);
    } else if (tmpStr == "dbleq") {  // Ok
        auto tmpArg = it.value();
        std::string side;

        for (auto it1 = tmpArg.begin(); it1 != tmpArg.end(); ++it1) {
            side = it1.key();

            DEBUG_OUT << "chooseLogicExprIter: Side: " << side << std::endl;

            if (side == "left") {
                left = chooseID(it1.value());
            } else if (side == "right") {
                right = chooseID(it1.value());
            }
        }

        int tmpPos = left->getPosition();

        return std::make_unique<BinaryExprAST>("==", std::move(left), std::move(right), tmpPos);
    } else if (tmpStr == "lt") {  // Ok
        auto tmpArg = it.value();
        std::string side;

        for (auto it1 = tmpArg.begin(); it1 != tmpArg.end(); ++it1) {
            side = it1.key();

            DEBUG_OUT << "chooseLogicExprIter: Side: " << side << std::endl;

            if (side == "left") {
                left = chooseID(it1.value());
            } else if (side == "right") {
                right = chooseID(it1.value());
            }
        }

        int tmpPos = left->getPosition();

        return std::make_unique<BinaryExprAST>("<", std::move(left), std::move(right), tmpPos);
    } else if (tmpStr == "gt") {  // Ok
        auto tmpArg = it.value();
        std::string side;

        for (auto it1 = tmpArg.begin(); it1 != tmpArg.end(); ++it1) {
            side = it1.key();

            DEBUG_OUT << "chooseLogicExprIter: Side: " << side << std::endl;

            if (side == "left") {
                left = chooseID(it1.value());
            } else if (side == "right") {
                right = chooseID(it1.value());
            }
        }

        int tmpPos = left->getPosition();

        return std::make_unique<BinaryExprAST>(">", std::move(left), std::move(right), tmpPos);
    } else if (tmpStr == "leq") {  // Ok
        auto tmpArg = it.value();
        std::string side;

        for (auto it1 = tmpArg.begin(); it1 != tmpArg.end(); ++it1) {
            side = it1.key();

            DEBUG_OUT << "chooseLogicExprIter: Side: " << side << std::endl;

            if (side == "left") {
                left = chooseID(it1.value());
            } else if (side == "right") {
                right = chooseID(it1.value());
            }
        }

        int tmpPos = left->getPosition();

        return std::make_unique<BinaryExprAST>("<=", std::move(left), std::move(right), tmpPos);
    } else if (tmpStr == "geq") {  // Ok
        auto tmpArg = it.value();
        std::string side;

        for (auto it1 = tmpArg.begin(); it1 != tmpArg.end(); ++it1) {
            side = it1.key();

            DEBUG_OUT << "chooseLogicExprIter: Side: " << side << std::endl;

            if (side == "left") {
                left = chooseID(it1.value());
            } else if (side == "right") {
                right = chooseID(it1.value());
            }
        }

        int tmpPos = left->getPosition();

        return std::make_unique<BinaryExprAST>(">=", std::move(left), std::move(right), tmpPos);
    } else if (tmpStr == "dblamp") {  // Ok
        auto tmpArg = it.value();
        std::string side;

        for (auto it1 = tmpArg.begin(); it1 != tmpArg.end(); ++it1) {
            side = it1.key();

            DEBUG_OUT << "chooseLogicExprIter: Side: " << side << std::endl;

            if (side == "left") {
                left = chooseLogicExpr(it1.value());
            } else if (side == "right") {
                right = chooseLogicExpr(it1.value());
            }
        }

        int tmpPos = left->getPosition();

        return std::make_unique<BinaryExprAST>("&&", std::move(left), std::move(right), tmpPos);
    } else if (tmpStr == "dblpipe") {  // Ok
        auto tmpArg = it.value();
        std::string side;

        for (auto it1 = tmpArg.begin(); it1 != tmpArg.end(); ++it1) {
            side = it1.key();

            DEBUG_OUT << "chooseLogicExprIter: Side: " << side << std::endl;

            if (side == "left") {
                left = chooseLogicExpr(it1.value());
            } else if (side == "right") {
                right = chooseLogicExpr(it1.value());
            }
        }

        int tmpPos = left->getPosition();

        return std::make_unique<BinaryExprAST>("||", std::move(left), std::move(right), tmpPos);
    }
    // }

    return nullptr;
}

std::vector<std::unique_ptr<ExprAST>> getArgs(const json &data) {
    std::vector<std::unique_ptr<ExprAST>> Args;
    int tmpArgPos = 0;

    for (auto it = data.begin(); it != data.end(); ++it) {
        auto tmpArg = it.value();

        for (const auto &item : tmpArg) {
            std::unique_ptr<ExprAST> resArg = chooseID(item);

            if (auto *complexArg = dynamic_cast<ComplexExprAST *>(resArg.get())) {
                subDimensionsIncreaseMap[tempSubName][complexArg->getBaseName()] =
                    std::max(complexArg->getDimension(),
                             subDimensionsIncreaseMap[tempSubName][complexArg->getBaseName()]);
            }

            if (!isExternFunc) {
                if (auto *simpleArg = dynamic_cast<VariableExprAST *>(resArg.get())) {
                    if (std::find(subsNamesMap[tempSubName].begin(),
                                  subsNamesMap[tempSubName].end(),
                                  simpleArg->getText()) != subsNamesMap[tempSubName].end() ||
                        std::find(subNamesParamsMap[tempSubName].begin(),
                                  subNamesParamsMap[tempSubName].end(),
                                  simpleArg->getText()) != subNamesParamsMap[tempSubName].end()) {
                        subDfcallsMap[tempSubName][simpleArg->getText()].push_back(
                            {tmpArgPos, tmpAnalyzingFunc});
                    }
                }
            }

            Args.push_back(std::move(resArg));
            ++tmpArgPos;
        }
    }

    return Args;
}

void createFunction(std::vector<Type *> Params, const std::string &Name,
                    const std::vector<int> &NamePositions, const std::vector<std::string> &Names) {
    Type *retType = Type::getVoidTy(*TheContext);
    FunctionType *FT = FunctionType::get(retType, Params, false);
    Function *F = Function::Create(FT, Function::ExternalLinkage, Name, TheModule.get());

    for (auto &arg : F->args()) {
        arg.setName(Names[arg.getArgNo()]);
        arg.addAttr(Attribute::NoUndef);
    }

    // for (int i = 0; i < NamePositions.size(); ++i) {
    //   Argument *arg = F->getArg(NamePositions[i]);
    //   AttrBuilder AB(*TheContext);
    //   AB.addAttribute(Attribute::NonNull);
    //   AB.addAlignmentAttr(8);
    //   AB.addDereferenceableAttr(36);
    //   arg->addAttrs(AB);
    // }
}

}  // namespace

std::unique_ptr<ExprAST> createProgramNode(const json &data) {
    std::vector<std::unique_ptr<ExprAST>> imports;
    std::vector<std::unique_ptr<SubExprAST>> subs;
    std::vector<std::unique_ptr<SubExprAST>> undefSubs;
    std::vector<std::unique_ptr<SubExprAST>> main;
    std::vector<json> jsonSubs;
    std::string tmpStr;

    std::string fa_name;

    for (auto it = data.begin(); it != data.end(); ++it) {
        auto tmpProg = it.value();

        if (it.key() == "fa_name") {
            fa_name = it.value();
            globalFaName = fa_name;
            continue;
        }

        for (const auto &item : tmpProg) {
            for (auto it1 = item.begin(); it1 != item.end(); ++it1) {
                tmpStr = it1.key();

                if (tmpStr == "import") {
                    auto import = createImportNode(it1.value());

                    if (import == nullptr) {
                        continue;
                    }

                    imports.push_back(std::move(import));
                } else if (tmpStr == "sub") {
                    jsonSubs.push_back(it1.value());
                }
            }
        }

        for (const auto &item : jsonSubs) {
            auto subs_res = altCreateSubNode(item);

            bool isMain = std::get<2>(subs_res);
            auto initStatus = std::get<1>(subs_res);

            if (initStatus == InitStatus::UNKNOWN) {
                undefSubs.push_back(std::move(std::get<0>(subs_res)));
                continue;
            }

            if (isMain) {
                main.push_back(std::move(std::get<0>(subs_res)));
            } else {
                subs.push_back(std::move(std::get<0>(subs_res)));
            }
        }
    }

    while (!undefSubs.empty()) {
        size_t oldSize = undefSubs.size();

        for (auto it = undefSubs.begin(); it != undefSubs.end();) {
            InitStatus initStatus = (*it)->tryResolveInitStatus();

            if (initStatus == InitStatus::INIT || initStatus == InitStatus::NORMAL) {
                std::string subName = (*it)->getSubName();
                subsStatusMap[subName] = initStatus;

                if (subName == "main") {
                    main.push_back(std::move(*it));
                } else {
                    subs.push_back(std::move(*it));
                }

                it = undefSubs.erase(it);
            } else {
                ++it;
            }
        }

        size_t newSize = undefSubs.size();

        if (newSize == oldSize) {
            // ErrorHandler::printError("find circular dependencies between subs.",
            //                          ErrorBase(ErrorType::MY2));
            DEBUG_OUT << "Circular dependencies detected among subs." << std::endl;
            break;  // no progress
        }
    }

    if (main.size() == 1) {
        subs.push_back(std::move(main[0]));
    } else if (main.size() > 1) {
        std::string errMsg = "Multiple main subs found: ";
        std::vector<cf_struct> cfs;

        for (const auto &m : main) {
            errMsg +=
                "'" + m->getSubName() + "' at line " + std::to_string(m->getPosition()) + ", ";

            cf_struct cf(m->getSubName(), "struct", globalFaName, m->getPosition());
            cfs.push_back(cf);
        }

        ErrorBase errorDetails(ErrorType::SYN62);
        errorDetails.setObjectDetail("cfs", std::make_unique<cf_struct_array>(std::move(cfs)));

        ErrorHandler::printError(std::move(errMsg), std::move(errorDetails));
    } else {
        ErrorHandler::printError("No main sub found.", ErrorBase(ErrorType::SYN7));
    }

    return std::make_unique<ProgramExprAST>(fa_name, std::move(imports), std::move(subs));
}

std::unique_ptr<ExprAST> createImportNode(const json &data) {
    std::vector<Type *> argTypes;
    std::vector<int> positions;
    std::string cFuncName;
    std::string lunaCodeId;

    std::string tmpStr;
    int tmpLine = -1;

    for (auto it = data.begin(); it != data.end(); ++it) {
        tmpStr = it.key();

        if (tmpStr == "cxx_code_id") {
            cFuncName = getValue(it.value());
            tmpLine = getLine(it.value());
        } else if (tmpStr == "luna_code_id") {
            lunaCodeId = getValue(it.value());
        } else if (tmpStr == "opt_ext_params") {
            argTypes = getParams(it.value(), positions);
        }
    }

    if (FunctionProtos.find(lunaCodeId) != FunctionProtos.end()) {
        std::string errMsg = "Duplicate function name '" + lunaCodeId + "' found in position " +
                             std::to_string(tmpLine) + " and " +
                             std::to_string(FunctionProtoPositions[lunaCodeId]);

        std::vector<cf_struct> cfs;
        cf_struct cf1(lunaCodeId, "extern", globalFaName, tmpLine);
        cfs.push_back(cf1);
        cf_struct cf2(lunaCodeId, "extern", globalFaName, FunctionProtoPositions[lunaCodeId]);
        cfs.push_back(cf2);
        ErrorBase errorDetails(ErrorType::SYN61);
        errorDetails.setObjectDetail("cfs", std::make_unique<cf_struct_array>(std::move(cfs)));

        ErrorHandler::printError(std::move(errMsg), std::move(errorDetails));
        return nullptr;
    }

    FunctionProtos[lunaCodeId] = cFuncName;
    FunctionProtoPositions[lunaCodeId] = tmpLine;

    DEBUG_OUT << "createImportNode: " << lunaCodeId << " -> " << cFuncName << std::endl;

    return std::make_unique<ImportExprAST>(lunaCodeId, cFuncName, argTypes, positions, tmpLine);
}

std::vector<std::unique_ptr<ExprAST>> createSubNode(const json &data) {
    std::vector<std::unique_ptr<ExprAST>> result;
    std::unique_ptr<BlockExprAST> block;
    std::unique_ptr<BlockExprAST> initBlock;
    std::vector<Type *> subArgs;
    std::vector<std::string> names;
    std::vector<int> positions;
    std::string codeId;
    int tmpLine = -1;

    codeId = getValue(data.at("code_id"));
    tmpLine = getLine(data.at("code_id"));
    subArgs = getSubParams(data.at("opt_params"), names, positions);

    tempSubName = codeId;

    if (codeId == "main") {
        block = createBlockNode(data.at("block"), false, false, true, codeId, names);

        if (FunctionProtos.find(codeId) != FunctionProtos.end()) {
            ErrorBase errorDetails(ErrorType::SYN62);
            return result;
        }

        if (std::find(subFunctions.begin(), subFunctions.end(), codeId) != subFunctions.end()) {
            ErrorHandler::printError("Duplicate function name '" + codeId + "' found.",
                                     ErrorBase(ErrorType::SYN62));
            return result;
        }

        subFunctions.push_back(codeId);
        createFunction(subArgs, codeId, positions, names);
        result.push_back(std::make_unique<SubExprAST>(codeId, subArgs, std::move(block), names,
                                                      positions, tmpLine));
    } else {
        initBlock = createBlockNode(data.at("block"), false, true, false, codeId, names);
        block = createBlockNode(data.at("block"), false, false, false, codeId, names);

        std::string initCodeId = "init_" + codeId;
        std::string otherCodeId = "other_" + codeId;

        if (FunctionProtos.find(initCodeId) != FunctionProtos.end()) {
            ErrorHandler::printError("Duplicate function name '" + initCodeId + "' found.",
                                     ErrorBase(ErrorType::SYN62));
            return result;
        }

        if (FunctionProtos.find(otherCodeId) != FunctionProtos.end()) {
            ErrorHandler::printError("Duplicate function name '" + otherCodeId + "' found.",
                                     ErrorBase(ErrorType::SYN62));
            return result;
        }

        if (std::find(subFunctions.begin(), subFunctions.end(), initCodeId) != subFunctions.end()) {
            ErrorHandler::printError("Duplicate function name '" + initCodeId + "' found.",
                                     ErrorBase(ErrorType::SYN62));
            return result;
        }

        if (std::find(subFunctions.begin(), subFunctions.end(), otherCodeId) !=
            subFunctions.end()) {
            ErrorHandler::printError("Duplicate function name '" + otherCodeId + "' found.",
                                     ErrorBase(ErrorType::SYN62));
            return result;
        }

        subFunctions.push_back(initCodeId);
        subFunctions.push_back(otherCodeId);

        createFunction(subArgs, initCodeId, positions, names);
        createFunction(subArgs, otherCodeId, positions, names);

        result.push_back(std::make_unique<SubExprAST>(initCodeId, subArgs, std::move(initBlock),
                                                      names, positions, tmpLine));
        result.push_back(std::make_unique<SubExprAST>(otherCodeId, subArgs, std::move(block), names,
                                                      positions, tmpLine));
    }

    return result;
}

std::tuple<std::unique_ptr<SubExprAST>, InitStatus, bool> altCreateSubNode(const json &data) {
    std::tuple<std::unique_ptr<SubExprAST>, InitStatus, bool> result;
    std::pair<std::unique_ptr<BlockExprAST>, InitStatus> altBlock;

    std::vector<Type *> subArgs;
    std::vector<std::string> names;
    std::vector<int> positions;
    std::string codeId;
    bool isMain = false;

    int tmpLine = -1;

    codeId = getValue(data.at("code_id"));
    tmpLine = getLine(data.at("code_id"));

    if (FunctionProtos.find(codeId) != FunctionProtos.end()) {
        std::string errMsg = "Duplicate function name '" + codeId + "' found in position " +
                             std::to_string(tmpLine) + " and " +
                             std::to_string(FunctionProtoPositions[codeId]);

        std::vector<cf_struct> cfs;
        cf_struct cf1(codeId, "struct", globalFaName, tmpLine);
        cfs.push_back(cf1);
        cf_struct cf2(codeId, "extern", globalFaName, FunctionProtoPositions[codeId]);
        cfs.push_back(cf2);
        ErrorBase errorDetails(ErrorType::SYN62);

        errorDetails.setObjectDetail("cfs", std::make_unique<cf_struct_array>(std::move(cfs)));
        ErrorHandler::printError(std::move(errMsg), std::move(errorDetails));
        return result;
    }

    if (std::find(subFunctions.begin(), subFunctions.end(), codeId) != subFunctions.end()) {
        std::string errMsg = "Duplicate function name '" + codeId + "' found in position " +
                             std::to_string(tmpLine) + " and " +
                             std::to_string(subFunctionsPositions[codeId]);

        std::vector<cf_struct> cfs;
        cf_struct cf1(codeId, "struct", globalFaName, tmpLine);
        cfs.push_back(cf1);
        cf_struct cf2(codeId, "struct", globalFaName, subFunctionsPositions[codeId]);
        cfs.push_back(cf2);
        ErrorBase errorDetails(ErrorType::SYN62);

        errorDetails.setObjectDetail("cfs", std::make_unique<cf_struct_array>(std::move(cfs)));
        ErrorHandler::printError(std::move(errMsg), std::move(errorDetails));
        return result;
    }

    if (codeId == "main") {
        isMain = true;
    }

    subFunctions.push_back(codeId);
    subFunctionsPositions[codeId] = tmpLine;
    tempSubName = codeId;

    subArgs = getSubParams(data.at("opt_params"), names, positions);
    altBlock = altCreateBlockNode(data.at("block"), codeId, names);

    createFunction(subArgs, codeId, positions, names);
    subsStatusMap[codeId] = altBlock.second;

    std::unique_ptr<SubExprAST> subNode = std::make_unique<SubExprAST>(
        codeId, subArgs, std::move(altBlock.first), names, positions, tmpLine);
    subNode->setSubName(codeId);

    return std::make_tuple(std::move(subNode), altBlock.second, isMain);
}

std::unique_ptr<BlockExprAST> createBlockNode(const json &data, bool notSub, bool initMode,
                                              bool mainBlock, const std::string &codeId,
                                              const std::vector<std::string> &names) {
    std::vector<std::unique_ptr<ExprAST>> dfDecls;
    std::vector<std::unique_ptr<ExprAST>> statements;
    std::vector<std::unique_ptr<ExprAST>> initNames;

    dfDecls = createDfDeclsNode(data.at("dfdecls"));

    if (!notSub && !initMode && !mainBlock) {
        statements = createStatementsNode(data.at("statement_seq"), codeId, names);
    }

    if (!notSub && initMode && !mainBlock) {
        initNames = createInitStatements(data.at("statement_seq"), codeId, names);
    }

    if (!notSub && !initMode && mainBlock) {
        initNames = createInitStatements(data.at("statement_seq"), codeId, names);
        statements = createStatementsNode(data.at("statement_seq"), codeId, names);
    }

    // if (notSub && initMode && initNames.size() == 0) { // Not needed
    //     return nullptr;
    // }

    // if (notSub && !initMode && statements.size() == 0) { // Not needed
    //     return nullptr;
    // }

    // return std::make_unique<BlockExprAST>(std::move(dfDecls), std::move(statements),
    // std::move(initNames));

    return nullptr;  // Placeholder to avoid compilation error
}

std::pair<std::unique_ptr<BlockExprAST>, InitStatus>
altCreateBlockNode(const json &data, const std::string &codeId,
                   const std::vector<std::string> &names) {
    std::vector<std::unique_ptr<ExprAST>> dfDecls;
    std::vector<std::unique_ptr<CheckableExprAST>> statements;
    std::vector<std::unique_ptr<CheckableExprAST>> initNames;
    std::vector<std::unique_ptr<CheckableExprAST>> undefStatements;
    InitStatus status = InitStatus::NORMAL;

    dfDecls = createDfDeclsNode(data.at("dfdecls"));

    json statementSeq = data.at("statement_seq");

    for (const auto &item : statementSeq) {
        auto statement = altChooseStatement(item, codeId, names);

        if (statement.first != nullptr) {
            if (statement.second == InitStatus::UNKNOWN) {
                undefStatements.push_back(std::move(statement.first));
            } else if (statement.second == InitStatus::INIT) {
                initNames.push_back(std::move(statement.first));
            } else if (statement.second == InitStatus::NORMAL) {
                statements.push_back(std::move(statement.first));
            }
        }
    }

    if (undefStatements.size() > 0) {
        status = InitStatus::UNKNOWN;
    } else if (initNames.size() > 0) {
        status = InitStatus::INIT;
    } else {
        status = InitStatus::NORMAL;
    }

    return std::make_pair(std::make_unique<BlockExprAST>(std::move(dfDecls), std::move(statements),
                                                         std::move(initNames),
                                                         std::move(undefStatements)),
                          status);
}

std::vector<std::unique_ptr<ExprAST>> createDfDeclsNode(const json &data) {
    std::vector<std::unique_ptr<ExprAST>> dfDecls;
    std::string tmpKey;

    for (auto it = data.begin(); it != data.end(); ++it) {
        tmpKey = it.key();

        if (tmpKey == "name_seq") {
            dfDecls = getNameSeq(it.value());
        }
    }

    return dfDecls;
}

std::vector<std::unique_ptr<ExprAST>> createStatementsNode(const json &data, std::string subName,
                                                           std::vector<std::string> subArgs) {
    std::vector<std::unique_ptr<ExprAST>> statements;

    for (const auto &item : data) {
        auto statement = chooseStatement(item, false, subName, subArgs);

        if (statement != nullptr) {
            statements.push_back(std::move(statement));
        }
    }

    return statements;
}

std::vector<std::unique_ptr<ExprAST>> createInitStatements(const json &data, std::string subName,
                                                           std::vector<std::string> subArgs) {
    std::vector<std::unique_ptr<ExprAST>> initNames;

    for (const auto &item : data) {
        auto statement = chooseStatement(item, true, subName, subArgs);

        if (statement != nullptr) {
            initNames.push_back(std::move(statement));
        }
    }

    return initNames;
}

std::unique_ptr<ExprAST> createCallStatementNode(const json &data, bool initMode,
                                                 std::string subName,
                                                 std::vector<std::string> subArgs) {
    std::vector<std::unique_ptr<ExprAST>> Args;
    std::string codeId;
    std::string tmpStr;
    std::string keyStr;

    bool keyFind = false;

    int dataLine = -1;

    for (auto it = data.begin(); it != data.end(); ++it) {
        tmpStr = it.key();

        if (tmpStr == "code_id") {
            codeId = getValue(it.value());
            dataLine = getLine(it.value());
        } else if (tmpStr == "opt_exprs") {
            Args = getArgs(it.value());
        }
    }

    if (FunctionProtos.find(codeId) != FunctionProtos.end()) {
        keyStr = FunctionProtos[codeId];
        keyFind = true;
    }

    // if (!keyFind && initMode) {
    //     std::string initCodeId = "init_" + codeId;
    //     if (std::find(subFunctions.begin(), subFunctions.end(), initCodeId) !=
    //     subFunctions.end()) {
    //         keyStr = initCodeId;
    //         keyFind = true;
    //     } else {
    //         std::cerr << "Error: Function '" << codeId << "' not found in init mode." <<
    //         std::endl; return nullptr;
    //     }
    // } else if (!keyFind && !initMode) {
    //     std::string otherCodeId = "other_" + codeId;
    //     if (std::find(subFunctions.begin(), subFunctions.end(), otherCodeId) !=
    //     subFunctions.end()) {
    //         keyStr = otherCodeId;
    //         keyFind = true;
    //     } else {
    //         std::cerr << "Error: Function '" << codeId << "' not found." << std::endl;
    //         return nullptr;
    //     }
    // }

    if (initMode && keyFind && initMemoryMap.find(keyStr) == initMemoryMap.end()) {
        return nullptr;
    }

    if (!initMode && keyFind && initMemoryMap.find(keyStr) != initMemoryMap.end()) {
        return nullptr;
    }

    if (initMode && subName != "" && keyFind) {
        std::vector<int> initPositions;

        for (auto &arg : Args) {
            std::string argName = arg->getText();

            if (std::find(subArgs.begin(), subArgs.end(), argName) != subArgs.end()) {
                auto it = std::find(subArgs.begin(), subArgs.end(), argName);
                int index = std::distance(subArgs.begin(), it);
                initPositions.push_back(index);
            }
        }

        initPosInSubs[subName] = initPositions;
    }

    return std::make_unique<CallExprAST>(codeId, std::move(Args), tempSubName, initMode, dataLine);
}

std::pair<std::unique_ptr<CheckableExprAST>, InitStatus>
altCreateCallStatementNode(const json &data) {
    std::vector<std::unique_ptr<ExprAST>> Args;
    std::vector<json> argJsons;
    std::string codeId;
    std::string tmpStr;

    std::string keyStr;
    int dataLine = -1;

    for (auto it = data.begin(); it != data.end(); ++it) {
        tmpStr = it.key();

        if (tmpStr == "code_id") {
            codeId = getValue(it.value());
            dataLine = getLine(it.value());
        } else if (tmpStr == "opt_exprs") {
            // Args = getArgs(it.value());
            argJsons.push_back(it.value());
        }
    }

    if (FunctionProtos.find(codeId) != FunctionProtos.end()) {
        isExternFunc = true;
    } else {
        isExternFunc = false;
        tmpAnalyzingFunc = codeId;
    }

    if (!argJsons.empty()) {
        Args = getArgs(argJsons[0]);
    }

    if (FunctionProtos.find(codeId) != FunctionProtos.end()) {
        keyStr = FunctionProtos[codeId];

        if (initMemoryMap.find(keyStr) != initMemoryMap.end()) {
            return {std::make_unique<CallExprAST>(codeId, std::move(Args), tempSubName, false, dataLine),
                    InitStatus::INIT};
        } else {
            return {std::make_unique<CallExprAST>(codeId, std::move(Args), tempSubName, false, dataLine),
                    InitStatus::NORMAL};
        }
    } else {
        if (subsStatusMap.find(codeId) != subsStatusMap.end()) {
            return {std::make_unique<CallExprAST>(codeId, std::move(Args), tempSubName, false, dataLine),
                    subsStatusMap[codeId]};
        } else {
            return {std::make_unique<CallExprAST>(codeId, std::move(Args), tempSubName, false, dataLine),
                    InitStatus::UNKNOWN};
        }
    }

    return {std::make_unique<CallExprAST>(codeId, std::move(Args), tempSubName, false, dataLine),
            InitStatus::UNKNOWN};
}

std::unique_ptr<ExprAST> createForStatementNode(const json &data, bool initMode,
                                                std::string subName,
                                                std::vector<std::string> subArgs) {
    std::unique_ptr<ExprAST> start, end, step;
    std::unique_ptr<BlockExprAST> block;
    std::string varName;
    std::string tmpStr;

    for (auto it = data.begin(); it != data.end(); ++it) {
        tmpStr = it.key();

        if (tmpStr == "name") {
            varName = getValue(it.value());
        } else if (tmpStr == "left") {
            start = chooseID(it.value());
        } else if (tmpStr == "right") {
            end = chooseID(it.value());
        } else if (tmpStr == "block") {
            block = createBlockNode(it.value(), false, initMode, false, subName, subArgs);
        }
    }

    if (block == nullptr) {
        return nullptr;
    }

    int tmpPos = start->getPosition();

    return std::make_unique<ForExprAST>(varName, std::move(start), std::move(end), std::move(block),
                                        tmpPos);
}

std::pair<std::unique_ptr<CheckableExprAST>, InitStatus>
altCreateForStatementNode(const json &data, std::string subName, std::vector<std::string> subArgs) {
    std::unique_ptr<ExprAST> start, end;
    std::pair<std::unique_ptr<BlockExprAST>, InitStatus> altBlock;
    std::string varName;
    std::string tmpStr;

    for (auto it = data.begin(); it != data.end(); ++it) {
        tmpStr = it.key();

        if (tmpStr == "name") {
            varName = getValue(it.value());
        } else if (tmpStr == "left") {
            start = chooseID(it.value());
        } else if (tmpStr == "right") {
            end = chooseID(it.value());
        } else if (tmpStr == "block") {
            altBlock = altCreateBlockNode(it.value(), subName, subArgs);
        }
    }

    if (altBlock.first == nullptr) {
        return {nullptr, InitStatus::FAILED};
    }

    int tmpPos = start->getPosition();

    return {std::make_unique<ForExprAST>(varName, std::move(start), std::move(end),
                                         std::move(altBlock.first), tmpPos),
            altBlock.second};
}

std::unique_ptr<ExprAST> createLetStatementNode(const json &data, bool initMode,
                                                std::string subName,
                                                std::vector<std::string> subArgs) {
    std::vector<std::string> names;
    std::vector<std::unique_ptr<ExprAST>> exprs;
    std::unique_ptr<BlockExprAST> block;
    std::vector<Type *> types;
    std::string tmpStr, tmpStr1, tmpStr2;

    for (auto it = data.begin(); it != data.end(); ++it) {
        tmpStr = it.key();

        DEBUG_OUT << "createLetStatementNode: " << tmpStr << std::endl;

        if (tmpStr == "block") {
            block = createBlockNode(it.value(), false, initMode, false, subName, subArgs);
        } else if (tmpStr == "assign_seq") {
            auto assignSeq = it.value();

            for (const auto &item : assignSeq) {
                for (auto it1 = item.begin(); it1 != item.end(); ++it1) {
                    tmpStr1 = it1.key();

                    DEBUG_OUT << "createLetStatementNode: " << tmpStr1 << std::endl;

                    if (tmpStr1 == "assign") {
                        auto tmpAssign = it1.value();

                        for (auto it2 = tmpAssign.begin(); it2 != tmpAssign.end(); ++it2) {
                            tmpStr2 = it2.key();

                            if (tmpStr2 == "name") {
                                names.push_back(getValue(it2.value()));
                            } else if (tmpStr2 == "expr") {
                                exprs.push_back(chooseID(it2.value()));

                                // Additional:
                                std::string tmpVal = getValue(it2.value());
                                DEBUG_OUT << "createLetStatementNode: TmpVal: " << tmpVal
                                          << std::endl;

                                switch (determineType(tmpVal)) {
                                case NumberType::INTEGER:
                                    types.push_back(Type::getInt32Ty(*TheContext));
                                    break;
                                case NumberType::FLOAT:
                                    types.push_back(Type::getDoubleTy(*TheContext));
                                    break;
                                case NumberType::STRING:
                                    types.push_back(
                                        PointerType::get(Type::getInt8Ty(*TheContext), 0));
                                    break;
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    if (block == nullptr) {
        return nullptr;
    }

    int tmpPos = exprs.empty() ? -1 : exprs[0]->getPosition();

    return std::make_unique<LetExprAST>(std::move(names), std::move(exprs), std::move(block),
                                        std::move(types), tmpPos);
}

std::pair<std::unique_ptr<CheckableExprAST>, InitStatus>
altCreateLetStatementNode(const json &data, std::string subName, std::vector<std::string> subArgs) {
    std::vector<std::string> names;
    std::vector<std::unique_ptr<ExprAST>> exprs;
    std::pair<std::unique_ptr<BlockExprAST>, InitStatus> altBlock;
    std::vector<Type *> types;
    std::string tmpStr, tmpStr1, tmpStr2;

    for (auto it = data.begin(); it != data.end(); ++it) {
        tmpStr = it.key();

        DEBUG_OUT << "altCreateLetStatementNode: " << tmpStr << std::endl;

        if (tmpStr == "block") {
            altBlock = altCreateBlockNode(it.value(), subName, subArgs);
        } else if (tmpStr == "assign_seq") {
            auto assignSeq = it.value();

            for (const auto &item : assignSeq) {
                for (auto it1 = item.begin(); it1 != item.end(); ++it1) {
                    tmpStr1 = it1.key();

                    DEBUG_OUT << "altCreateLetStatementNode: " << tmpStr1 << std::endl;

                    if (tmpStr1 == "assign") {
                        auto tmpAssign = it1.value();

                        for (auto it2 = tmpAssign.begin(); it2 != tmpAssign.end(); ++it2) {
                            tmpStr2 = it2.key();

                            if (tmpStr2 == "name") {
                                names.push_back(getValue(it2.value()));
                            } else if (tmpStr2 == "expr") {
                                exprs.push_back(chooseID(it2.value()));

                                // Additional:
                                std::string tmpVal = getValue(it2.value());
                                DEBUG_OUT << "altCreateLetStatementNode: TmpVal: " << tmpVal
                                          << std::endl;

                                switch (determineType(tmpVal)) {
                                case NumberType::INTEGER:
                                    types.push_back(Type::getInt32Ty(*TheContext));
                                    break;
                                case NumberType::FLOAT:
                                    types.push_back(Type::getDoubleTy(*TheContext));
                                    break;
                                case NumberType::STRING:
                                    types.push_back(
                                        PointerType::get(Type::getInt8Ty(*TheContext), 0));
                                    break;
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    if (altBlock.first == nullptr) {
        return {nullptr, InitStatus::FAILED};
    }

    int tmpPos = exprs.empty() ? -1 : exprs[0]->getPosition();

    return {std::make_unique<LetExprAST>(std::move(names), std::move(exprs),
                                         std::move(altBlock.first), std::move(types), tmpPos),
            altBlock.second};
}

std::unique_ptr<ExprAST> createWhileStatementNode(const json &data, bool initMode,
                                                  std::string subName,
                                                  std::vector<std::string> subArgs) {
    std::unique_ptr<ExprAST> cond;
    std::unique_ptr<BlockExprAST> block;
    std::unique_ptr<ExprAST> startVal;
    std::unique_ptr<ExprAST> outDfId;
    std::string varName;
    std::string tmpStr;

    for (auto it = data.begin(); it != data.end(); ++it) {
        tmpStr = it.key();

        if (tmpStr == "block") {
            block = createBlockNode(it.value(), false, initMode, false, subName, subArgs);
        } else if (tmpStr == "left") {
            varName = getValue(it.value());
        } else if (tmpStr == "id") {
            outDfId = chooseID(it.value());
        } else if (tmpStr == "right") {
            startVal = chooseID(it.value());
        } else if (tmpStr == "expr") {
            cond = chooseLogicExpr(it.value());
        }
    }

    if (block == nullptr) {
        return nullptr;
    }

    int tmpPos = cond->getPosition();

    return std::make_unique<WhileExprAST>(varName, std::move(outDfId), std::move(cond),
                                          std::move(block), std::move(startVal), tmpPos);
}

std::pair<std::unique_ptr<CheckableExprAST>, InitStatus>
altCreateWhileStatementNode(const json &data, std::string subName,
                            std::vector<std::string> subArgs) {
    std::unique_ptr<ExprAST> cond;
    std::pair<std::unique_ptr<BlockExprAST>, InitStatus> altBlock;
    std::unique_ptr<ExprAST> startVal;
    std::unique_ptr<ExprAST> outDfId;
    std::string varName;
    std::string tmpStr;

    for (auto it = data.begin(); it != data.end(); ++it) {
        tmpStr = it.key();

        if (tmpStr == "block") {
            altBlock = altCreateBlockNode(it.value(), subName, subArgs);
        } else if (tmpStr == "left") {
            varName = getValue(it.value());
        } else if (tmpStr == "id") {
            outDfId = chooseID(it.value());
        } else if (tmpStr == "right") {
            startVal = chooseID(it.value());
        } else if (tmpStr == "expr") {
            cond = chooseLogicExpr(it.value());
        }
    }

    if (altBlock.first == nullptr) {
        return {nullptr, InitStatus::FAILED};
    }

    int tmpPos = cond->getPosition();

    return {std::make_unique<WhileExprAST>(varName, std::move(outDfId), std::move(cond),
                                           std::move(altBlock.first), std::move(startVal), tmpPos),
            InitStatus::INIT};
}

std::unique_ptr<ExprAST> createIfStatementNode(const json &data, bool initMode, std::string subName,
                                               std::vector<std::string> subArgs) {
    std::unique_ptr<ExprAST> cond;
    std::unique_ptr<BlockExprAST> thenBlock;
    std::string tmpStr;

    for (auto it = data.begin(); it != data.end(); ++it) {
        tmpStr = it.key();

        DEBUG_OUT << "createIfStatementNode: " << tmpStr << std::endl;

        if (tmpStr == "block") {
            thenBlock = createBlockNode(it.value(), false, initMode, false, subName, subArgs);
        } else {
            cond = chooseLogicExprIter(it);
        }
    }

    if (thenBlock == nullptr || cond == nullptr) {
        LogErrorV("If statement requires a condition and a block.");
        return nullptr;
    }

    int tmpPos = cond->getPosition();

    return std::make_unique<IfExprAST>(std::move(cond), std::move(thenBlock), tmpPos);
}

std::pair<std::unique_ptr<CheckableExprAST>, InitStatus>
altCreateIfStatementNode(const json &data, std::string subName, std::vector<std::string> subArgs) {
    std::unique_ptr<ExprAST> cond;
    std::pair<std::unique_ptr<BlockExprAST>, InitStatus> altBlock;
    std::string tmpStr;

    for (auto it = data.begin(); it != data.end(); ++it) {
        tmpStr = it.key();

        DEBUG_OUT << "altCreateIfStatementNode: " << tmpStr << std::endl;

        if (tmpStr == "block") {
            altBlock = altCreateBlockNode(it.value(), subName, subArgs);
        } else {
            cond = chooseLogicExprIter(it);
        }
    }

    if (altBlock.first == nullptr || cond == nullptr) {
        LogErrorV("If statement requires a condition and a block.");
        return {nullptr, InitStatus::FAILED};
    }

    int tmpPos = cond->getPosition();

    return {std::make_unique<IfExprAST>(std::move(cond), std::move(altBlock.first), tmpPos),
            altBlock.second};
}
