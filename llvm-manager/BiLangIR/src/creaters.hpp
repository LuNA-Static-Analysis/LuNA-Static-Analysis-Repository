#pragma once

#include "nodes.hpp"

std::unique_ptr<ExprAST> createProgramNode(const json &data);
std::unique_ptr<ExprAST> createImportNode(const json &data);

std::vector<std::unique_ptr<ExprAST>> createSubNode(const json &data);
std::unique_ptr<BlockExprAST> createBlockNode(const json &data, bool notSub, bool initMode,
                                              bool mainBlock, const std::string &codeId = "",
                                              const std::vector<std::string> &names = {});
std::vector<std::unique_ptr<ExprAST>> createDfDeclsNode(const json &data);
std::vector<std::unique_ptr<ExprAST>> createStatementsNode(const json &data, std::string subName,
                                                           std::vector<std::string> subArgs);
std::vector<std::unique_ptr<ExprAST>> createInitStatements(const json &data, std::string subName,
                                                           std::vector<std::string> subArgs);
std::unique_ptr<ExprAST> createCallStatementNode(const json &data, bool initMode,
                                                 std::string subName,
                                                 std::vector<std::string> subArgs);
std::unique_ptr<ExprAST> createForStatementNode(const json &data, bool initMode,
                                                std::string subName,
                                                std::vector<std::string> subArgs);
std::unique_ptr<ExprAST> createLetStatementNode(const json &data, bool initMode,
                                                std::string subName,
                                                std::vector<std::string> subArgs);
std::unique_ptr<ExprAST> createWhileStatementNode(const json &data, bool initMode,
                                                  std::string subName,
                                                  std::vector<std::string> subArgs);
std::unique_ptr<ExprAST> createIfStatementNode(const json &data, bool initMode, std::string subName,
                                               std::vector<std::string> subArgs);

std::tuple<std::unique_ptr<SubExprAST>, InitStatus, bool> altCreateSubNode(const json &data);
std::pair<std::unique_ptr<BlockExprAST>, InitStatus>
altCreateBlockNode(const json &data, const std::string &codeId,
                   const std::vector<std::string> &names);
std::pair<std::unique_ptr<CheckableExprAST>, InitStatus>
altCreateCallStatementNode(const json &data);
std::pair<std::unique_ptr<CheckableExprAST>, InitStatus>
altCreateForStatementNode(const json &data, std::string subName, std::vector<std::string> subArgs);
std::pair<std::unique_ptr<CheckableExprAST>, InitStatus>
altCreateLetStatementNode(const json &data, std::string subName, std::vector<std::string> subArgs);
std::pair<std::unique_ptr<CheckableExprAST>, InitStatus>
altCreateWhileStatementNode(const json &data, std::string subName,
                            std::vector<std::string> subArgs);
std::pair<std::unique_ptr<CheckableExprAST>, InitStatus>
altCreateIfStatementNode(const json &data, std::string subName, std::vector<std::string> subArgs);
