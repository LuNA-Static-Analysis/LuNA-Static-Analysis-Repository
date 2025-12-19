// #include "codegen.hpp"

#include "creaters.hpp"
#include "debug.hpp"
#include "depends.hpp"
#include "error_handler.hpp"
#include "error_nodes.hpp"

#include <cstdlib>
#include <cstring>
#include <dirent.h>
#include <exception>
#include <fstream>
#include <iostream>
#include <json.hpp>
#include <llvm/IR/Attributes.h>
#include <set>
#include <string>
#include <sys/stat.h>
#include <sys/types.h>
#include <vector>

// Global debug flag definition
bool g_debugEnabled = false;

std::unique_ptr<LLVMContext> TheContext;
std::unique_ptr<IRBuilder<>> Builder;
std::unique_ptr<Module> TheModule;

std::map<std::string, Value *> NamedValues;         // var name -> llvm value of this var (maybe)
std::map<std::string, std::string> FunctionProtos;  // luna func -> cpp func
std::map<std::string, int> FunctionProtoPositions;  // luna func -> position of its proto
std::map<std::string, std::string> initMemoryMap;   // cpp function name -> type of value it sets
std::vector<std::string> ParamsVec;                 // function parameters (maybe)

std::map<std::string, Value *> NameMap;            // df name -> llvm value of this name
std::map<std::string, bool> initsNameMap;          // df name -> is initialized
std::vector<std::string> importFunctions;          // names of cpp import functions
std::vector<std::string> subFunctions;             // names of "sub" functions
std::map<std::string, int> subFunctionsPositions;  // "sub" function name -> its position
std::string tempSubName;                           // current name of "sub" function
std::map<std::string, InitStatus> subsStatusMap;   // "sub" function name -> its status
std::vector<std::string> subsWhichTaged;           // "sub" which already in list of errors

std::map<std::string, std::vector<int>>
    initPosInSubs;  // "sub" function name -> positions of init args
std::map<std::string, std::vector<std::string>>
    subsNamesMap;  // "sub" function name -> df names of this "sub"

std::map<std::string, std::map<std::string, int>>
    dfDimensionsMap;  // "sub" name -> (df name -> its dimension)
std::map<std::string, std::vector<std::string>>
    subNamesParamsMap;  // "sub" name -> its "name" params
std::map<std::string, std::map<std::string, int>>
    subDimensionsIncreaseMap;  // "sub" name -> (df name -> its dimension increase in this "sub")
std::map<std::string, std::map<std::string, std::vector<std::pair<int, std::string>>>>
    subDfcallsMap;  // "sub" name -> (df name -> vector of (param pos, called sub name))
std::map<std::string, std::map<int, std::string>>
    subPosNameMap;  // "sub" name -> (param pos -> param name)
std::map<std::string, Value *>
    dfArrayMap;  // df name -> pointer to array of DF objects (for dimension = 1)
std::map<std::string, std::string> paramToActualNameMap;  // param name -> actual argument name
std::map<std::string, Value *>
    paramArrayAddressMap;  // param name -> array base address passed as argument

std::map<std::string, std::pair<std::string, int>> globalConstantsMap; // const name -> (file, line)
std::map<std::string, std::pair<std::string, int>> functionsMap;       // func name -> (file, line)
std::map<std::string, std::pair<std::string, int>> declarationsMap;    // func decl name -> (file, line)

std::string globalFaName;
bool isInitFuncAnalysis;
bool isExternFunc;
std::string tmpAnalyzingFunc;

void dumpDefinitions(const std::string& path);

void getInitMemoryMap(const Module &M) {
    std::string needName = "2DF8setValue";

    for (auto &F : M) {
        if (F.isDeclaration()) {
            continue;
        }

        for (auto &BB : F) {
            for (auto &I : BB) {
                if (isa<CallInst>(I)) {
                    const CallInst *CI = cast<CallInst>(&I);
                    Function *Callee = CI->getCalledFunction();

                    if (Callee) {
                        std::string funcName = Callee->getName().str();
                        size_t pos = funcName.find(needName);

                        if (pos == std::string::npos) {
                            continue;
                        }

                        size_t templatePos = pos + needName.size();
                        size_t templateEnd = funcName.find("E", templatePos + 1);

                        std::string templateParam =
                            funcName.substr(templatePos + 1, templateEnd - templatePos - 1);

                        if (templateParam == "i") {
                            initMemoryMap[F.getName().str()] = "int";
                            DEBUG_OUT << "Found init function: " << F.getName().str() << " for int"
                                      << std::endl;
                        } else if (templateParam == "d") {
                            initMemoryMap[F.getName().str()] = "double";
                            DEBUG_OUT << "Found init function: " << F.getName().str()
                                      << " for double" << std::endl;
                        } else {
                            initMemoryMap[F.getName().str()] = "string";
                            DEBUG_OUT << "Found init function: " << F.getName().str()
                                      << " for string" << std::endl;
                        }
                    }
                }
            }
        }
    }
}

int checkIncreaseDimension(const std::string &tmpSub, const std::string &item) {
    if (subDfcallsMap[tmpSub].find(item) != subDfcallsMap[tmpSub].end()) {
        for (const auto &callInfo : subDfcallsMap[tmpSub][item]) {
            std::string calledSub = callInfo.second;
            int paramPos = callInfo.first;

            DEBUG_OUT << "    Called sub: " << calledSub << " for param pos " << paramPos
                      << std::endl;

            std::string paramName = "";

            if (subPosNameMap[calledSub].find(paramPos) != subPosNameMap[calledSub].end()) {
                paramName = subPosNameMap[calledSub][paramPos];
            } else {
                DEBUG_OUT << "    No param name found for this call." << std::endl;
                continue;
            }

            if (subDimensionsIncreaseMap[calledSub].find(paramName) !=
                subDimensionsIncreaseMap[calledSub].end()) {
                int dimIncrease = subDimensionsIncreaseMap[calledSub][paramName];

                DEBUG_OUT << "    Dimension increase in called sub: " << dimIncrease << std::endl;

                if (dimIncrease > 0) {
                    dfDimensionsMap[tmpSub][item] += dimIncrease;

                    DEBUG_OUT << "    Updated dimension of " << item << " in " << tmpSub << " to "
                              << dfDimensionsMap[tmpSub][item] << std::endl;

                    return dfDimensionsMap[tmpSub][item];
                } else {
                    return checkIncreaseDimension(calledSub, paramName);
                }
            }
        }
    } else {
        DEBUG_OUT << "    No calls found for this DF name." << std::endl;
    }

    return 0;
}

// Print usage information
void printUsage(const char *programName, bool toStdout = false) {
    std::ostream &out = toStdout ? std::cout : std::cerr;

    out << "Usage: " << programName
        << " [OPTIONS] <json_file> <ll_file|ll_dir> [<ll_file|ll_dir> ...]" << std::endl
        << std::endl
        << "Arguments:" << std::endl
        << "  <json_file>           Path to the JSON AST file" << std::endl
        << "  <ll_file|ll_dir>      LLVM IR file(s) or directory(ies) containing .ll files"
        << std::endl
        << "                        Directories are searched recursively" << std::endl
        << std::endl
        << "Options:" << std::endl
        << "  -h, --help            Show this help message and exit" << std::endl
        << "  -d, --debug           Enable debug output" << std::endl
        << "  -e, --error-mode <mode>" << std::endl
        << "                        Set error reporting mode: 'simple' or 'system'" << std::endl
        << "                        Default: system" << std::endl
        << "  -o, --error-output <file>" << std::endl
        << "                        Write errors to specified file" << std::endl
        << "  -p, --project-dir <dir>" << std::endl
        << "                        Project directory (creates .BiLangIR subdirectory for output)"
        << std::endl;
}

// Helper function to normalize path (resolve to absolute path)
std::string normalizePath(const std::string &path) {
    char *realPath = realpath(path.c_str(), nullptr);

    if (realPath) {
        std::string result(realPath);
        free(realPath);
        return result;
    }

    return path;  // Return original if normalization fails
}

// Helper function to check if a path is a directory
bool isDirectory(const std::string &path) {
    struct stat info;

    if (stat(path.c_str(), &info) != 0) {
        return false;
    }

    return (info.st_mode & S_IFDIR) != 0;
}

// Helper function to recursively find all .ll files in a directory
void findLLFiles(const std::string &dirPath, std::set<std::string> &llFilesSet) {
    DIR *dir = opendir(dirPath.c_str());

    if (!dir) {
        std::cerr << "Warning: Failed to open directory: " << dirPath << std::endl;
        return;
    }

    struct dirent *entry;

    while ((entry = readdir(dir)) != nullptr) {
        std::string name = entry->d_name;

        // Skip . and ..
        if (name == "." || name == "..") {
            continue;
        }

        std::string fullPath = dirPath;

        if (fullPath.back() != '/') {
            fullPath += "/";
        }

        fullPath += name;

        if (isDirectory(fullPath)) {
            // Recursively search subdirectories
            findLLFiles(fullPath, llFilesSet);
        } else {
            // Check if file has .ll extension
            if (name.size() >= 3 && name.substr(name.size() - 3) == ".ll") {
                std::string normalizedPath = normalizePath(fullPath);

                if (llFilesSet.insert(normalizedPath).second) {
                    DEBUG_OUT << "Found .ll file: " << normalizedPath << std::endl;
                } else {
                    DEBUG_OUT << "Skipping duplicate file: " << normalizedPath << std::endl;
                }
            }
        }
    }

    closedir(dir);
}

void initAllData() {
    NamedValues = std::map<std::string, Value *>();
    FunctionProtos = std::map<std::string, std::string>();
    FunctionProtoPositions = std::map<std::string, int>();
    initMemoryMap = std::map<std::string, std::string>();
    ParamsVec = std::vector<std::string>();

    NameMap = std::map<std::string, Value *>();
    initsNameMap = std::map<std::string, bool>();
    importFunctions = std::vector<std::string>();
    subFunctions = std::vector<std::string>();
    subFunctionsPositions = std::map<std::string, int>();
    initPosInSubs = std::map<std::string, std::vector<int>>();
    subsNamesMap = std::map<std::string, std::vector<std::string>>();
    tempSubName = "";
    subsStatusMap = std::map<std::string, InitStatus>();
    subsWhichTaged = std::vector<std::string>();

    dfDimensionsMap = std::map<std::string, std::map<std::string, int>>();
    subNamesParamsMap = std::map<std::string, std::vector<std::string>>();
    subDimensionsIncreaseMap = std::map<std::string, std::map<std::string, int>>();
    subDfcallsMap =
        std::map<std::string, std::map<std::string, std::vector<std::pair<int, std::string>>>>();
    subPosNameMap = std::map<std::string, std::map<int, std::string>>();

    globalConstantsMap = std::map<std::string, std::pair<std::string, int>>();
    functionsMap = std::map<std::string, std::pair<std::string, int>>();
    declarationsMap = std::map<std::string, std::pair<std::string, int>>();

    dfArrayMap = std::map<std::string, Value *>();
    paramToActualNameMap = std::map<std::string, std::string>();
    paramArrayAddressMap = std::map<std::string, Value *>();
    globalFaName = "";
}

void addGlobalConstError(const GlobalVariable& GV, const std::string& llFile) {
    std::string fileName = llFile;
    int line = 0;
            
    // Try to get debug info from metadata
    SmallVector<DIGlobalVariableExpression *, 1> GVs;
    GV.getDebugInfo(GVs);
    if (!GVs.empty() && GVs[0]) {
        DIGlobalVariable *DIGV = GVs[0]->getVariable();
        if (DIGV && DIGV->getFile()) {
            fileName = DIGV->getFile()->getFilename().str();
            line = DIGV->getLine();
        }
    }

    std::string errMsg = "Global constant '" + GV.getName().str() + "' defined in multiple locations";
    ErrorBase error(ErrorType::SYN13);
    error.setObjectDetail("first",
                          std::make_unique<global_const>(GV.getName().str(), globalConstantsMap[GV.getName().str()]));
    error.setObjectDetail("second",
                          std::make_unique<global_const>(GV.getName().str(), std::make_pair(fileName, line)));
    ErrorHandler::printError(errMsg, std::move(error));
}

void addFunctionError(const Function& F, const std::string& llFile, ErrorType errType = ErrorType::SYN14, const std::string& baseFileName = "") {
    if (errType != ErrorType::SYN14) {
        return;
    }

    std::string fileName = llFile;
    bool isDefinition = false;
    int line = 0;
    
    if (!F.isDeclaration()) {
        isDefinition = true;
        DISubprogram *SP = F.getSubprogram();
        if (SP && SP->getFile()) {
            fileName = SP->getFile()->getFilename().str();
            line = SP->getLine();
        }
    } else {
        if (!baseFileName.empty()) {
            fileName = baseFileName;
        }
    }

    std::string errMsg;

    if (errType == ErrorType::SYN14) {
        errMsg = "Function '" + F.getName().str() + "' defined in multiple locations";
    } else {
        errMsg = "Function '" + F.getName().str() + "' has conflicting types in different locations";
    }

    std::pair<std::string, int> funcLocation;

    if (functionsMap.find(F.getName().str()) != functionsMap.end()) {
        funcLocation = functionsMap[F.getName().str()];
    } else if (declarationsMap.find(F.getName().str()) != declarationsMap.end()) {
        funcLocation = declarationsMap[F.getName().str()];
    } else {
        funcLocation = std::make_pair("unknown", 0);
    }

    ErrorBase error(errType);

    if (errType == ErrorType::SYN14) {
        error.setObjectDetail("first",
                                std::make_unique<cf_struct>(F.getName().str(), "extern", funcLocation));
        error.setObjectDetail("second",
                                std::make_unique<cf_struct>(F.getName().str(), "extern", std::make_pair(fileName, line)));
    } else {
        if (isDefinition) {
            error.setObjectDetail("definition",
                                  std::make_unique<cf_struct>(F.getName().str(), "extern", std::make_pair(fileName, line)));
            error.setObjectDetail("usage",
                                  std::make_unique<cf_struct>(F.getName().str(), "extern", funcLocation));
        } else {
            error.setObjectDetail("definition",
                                  std::make_unique<cf_struct>(F.getName().str(), "extern", funcLocation));
            error.setObjectDetail("usage",
                                  std::make_unique<cf_struct>(F.getName().str(), "extern", std::make_pair(fileName, line)));
        }
    }

    ErrorHandler::printError(errMsg, std::move(error));
}

bool validateAndLinkModule(Linker &L, std::unique_ptr<Module> module, const std::string &llFile) {
    bool hasConflicts = false;
    std::string baseFileName = llFile;
    bool needChangeName = true;

    // Check for global variable conflicts
    for (auto &GV : module->globals()) {
        SmallVector<DIGlobalVariableExpression *, 1> GVs;
        GV.getDebugInfo(GVs);
        if (needChangeName && !GVs.empty() && GVs[0]) {
            DIGlobalVariable *DIGV = GVs[0]->getVariable();
            if (DIGV && DIGV->getFile()) {
                baseFileName = DIGV->getFile()->getFilename().str();
                needChangeName = false;
            }
        }

        GlobalVariable *existingGV = TheModule->getGlobalVariable(GV.getName());
        if (existingGV && !existingGV->isDeclaration() && !GV.isDeclaration()) {
            addGlobalConstError(GV, llFile);
            DEBUG_OUT << "Conflict: global variable '" << GV.getName().str()
                      << "' already defined in main module" << std::endl;
            hasConflicts = true;
        }
    }

    // Check for duplicate function definitions before linking
    for (auto &F : *module) {
        if (!F.isDeclaration()) {
            DISubprogram *SP = F.getSubprogram();
            if (needChangeName && SP && SP->getFile()) {
                baseFileName = SP->getFile()->getFilename().str();
                needChangeName = false;
            }

            Function *existingFunc = TheModule->getFunction(F.getName());
            if (existingFunc && !existingFunc->isDeclaration()) {
                addFunctionError(F, llFile, ErrorType::SYN14);
                DEBUG_OUT << "Conflict: function '" << F.getName().str()
                          << "' already defined in main module" << std::endl;
                hasConflicts = true;
            }
        }
    }

    for (auto &F : *module) {
        Function *existingFunc = TheModule->getFunction(F.getName());
        if (existingFunc) {
            if (F.getFunctionType() != existingFunc->getFunctionType()) {
                // addFunctionError(F, llFile, ErrorType::SYN15, baseFileName);
                // DEBUG_OUT << "Type conflict for function: " << F.getName().str() << std::endl;
                hasConflicts = true;
            }
        }
    }

    // If there are semantic conflicts, don't attempt to link
    if (hasConflicts) {
        return false;
    }

    // Collect information about global constants and functions from the valid module
    for (auto &GV : module->globals()) {
        // if (GV.isConstant()) {
            std::string fileName = llFile;
            int line = 0;
            
            // Try to get debug info from metadata
            SmallVector<DIGlobalVariableExpression *, 1> GVs;
            GV.getDebugInfo(GVs);
            if (!GVs.empty() && GVs[0]) {
                DIGlobalVariable *DIGV = GVs[0]->getVariable();
                if (DIGV && DIGV->getFile()) {
                    fileName = DIGV->getFile()->getFilename().str();
                    line = DIGV->getLine();
                }
            }
            
            globalConstantsMap[GV.getName().str()] = std::make_pair(fileName, line);
            DEBUG_OUT << "Added global constant: " << GV.getName().str() << " from " << fileName << ":" << line << std::endl;
        // }
    }

    for (auto &F : *module) {
        if (!F.isDeclaration()) {
            std::string fileName = llFile;
            int line = 0;
            
            // Try to get debug info from subprogram metadata
            DISubprogram *SP = F.getSubprogram();
            if (SP && SP->getFile()) {
                fileName = SP->getFile()->getFilename().str();
                line = SP->getLine();
            }
            
            functionsMap[F.getName().str()] = std::make_pair(fileName, line);
            DEBUG_OUT << "Added function: " << F.getName().str() << " from " << fileName << ":" << line << std::endl;
        } else {
            // Collect declarations
            declarationsMap[F.getName().str()] = std::make_pair(baseFileName, 0);
            DEBUG_OUT << "Added declaration: " << F.getName().str() << " from " << baseFileName << ":0" << std::endl;
        }
    }

    getInitMemoryMap(*module);

    bool linkErr = L.linkInModule(std::move(module));

    if (linkErr) {
        std::cerr << "LLVM linker error: Failed to link module from " << llFile << std::endl;
        std::cerr << "  This is an internal LLVM error, possibly due to incompatible IR versions"
                  << std::endl;

        return false;
    }

    DEBUG_OUT << "Successfully loaded and linked: " << llFile << std::endl;
    return true;
}

int main(int argc, char *argv[]) {
    ErrorMode errorMode = ErrorMode::SYSTEM;
    std::string errorOutputFile = "";
    std::string projectDir = "";
    std::string jsonFile = "";
    std::vector<std::string> llFiles;

    // Check for --help, --debug, --error-mode, --error-output, and --project-dir flags
    for (int i = 1; i < argc; i++) {
        std::string arg = argv[i];

        if (arg == "--help" || arg == "-h") {
            printUsage(argv[0], true);
            return EXIT_SUCCESS;
        } else if (arg == "--debug" || arg == "-d") {
            g_debugEnabled = true;
            DEBUG_PRINT("Debug mode enabled");
        } else if (arg == "--error-mode" || arg == "-e") {
            if (i + 1 < argc) {
                std::string modeStr = argv[i + 1];

                if (modeStr == "simple") {
                    errorMode = ErrorMode::SIMPLE;
                } else if (modeStr == "system") {
                    errorMode = ErrorMode::SYSTEM;
                } else {
                    std::cerr << "Unknown error mode: " << modeStr << ". Using 'system' by default."
                              << std::endl;
                }

                i++;  // Пропускаем следующий аргумент (значение флага)
            } else {
                std::cerr << "--error-mode requires an argument (simple or system)" << std::endl;
                return EXIT_FAILURE;
            }
        } else if (arg == "--error-output" || arg == "-o") {
            if (i + 1 < argc) {
                errorOutputFile = argv[i + 1];
                i++;  // Пропускаем следующий аргумент (значение флага)
            } else {
                std::cerr << "--error-output requires a file path argument" << std::endl;
                return EXIT_FAILURE;
            }
        } else if (arg == "--project-dir" || arg == "-p") {
            if (i + 1 < argc) {
                projectDir = argv[i + 1];
                i++;  // Пропускаем следующий аргумент (значение флага)
            } else {
                std::cerr << "--project-dir requires a directory path argument" << std::endl;
                return EXIT_FAILURE;
            }
        }
    }

    // Устанавливаем режим вывода ошибок
    ErrorHandler::setMode(errorMode);

    // Use set to automatically handle duplicates
    std::set<std::string> llFilesSet;

    // Collect non-flag arguments (JSON file and LL files)
    for (int i = 1; i < argc; i++) {
        std::string arg = argv[i];

        if (arg == "--help" || arg == "-h") {
            continue;
        } else if (arg == "--debug" || arg == "-d") {
            continue;
        } else if (arg == "--error-mode" || arg == "-e") {
            i++;  // Пропускаем значение флага
            continue;
        } else if (arg == "--error-output" || arg == "-o") {
            i++;  // Пропускаем значение флага
            continue;
        } else if (arg == "--project-dir" || arg == "-p") {
            i++;  // Пропускаем значение флага
            continue;
        } else {
            // First non-flag argument is JSON file, rest are LL files or directories
            if (jsonFile.empty()) {
                jsonFile = arg;
            } else {
                // Check if argument is a directory or a file
                if (isDirectory(arg)) {
                    DEBUG_OUT << "Processing directory: " << arg << std::endl;
                    findLLFiles(arg, llFilesSet);
                } else {
                    std::string normalizedPath = normalizePath(arg);

                    if (llFilesSet.insert(normalizedPath).second) {
                        DEBUG_OUT << "Added .ll file: " << normalizedPath << std::endl;
                    } else {
                        DEBUG_OUT << "Skipping duplicate file: " << normalizedPath << std::endl;
                    }
                }
            }
        }
    }

    // Convert set to vector
    llFiles.assign(llFilesSet.begin(), llFilesSet.end());

    if (jsonFile.empty() || llFiles.empty()) {
        printUsage(argv[0]);
        return EXIT_SUCCESS;
    }

    TheContext = std::make_unique<LLVMContext>();
    Builder = std::make_unique<IRBuilder<>>(*TheContext);
    TheModule = std::make_unique<Module>("luna", *TheContext);

    initAllData();

    // Parse and link all LL files
    std::unique_ptr<SMDiagnostic> TheErr = std::make_unique<SMDiagnostic>();
    Linker L(*TheModule);
    int successfullyLinked = 0;
    int totalFiles = llFiles.size();

    for (const auto &llFile : llFiles) {
        std::unique_ptr<Module> module = parseIRFile(llFile, *TheErr, *TheContext);

        if (!module) {
            std::cerr << "Failed to parse IR file: " << llFile << std::endl;
            std::cerr << "Syntax error at line " << TheErr->getLineNo() << ", column "
                      << TheErr->getColumnNo() << ": " << TheErr->getMessage().str() << std::endl;
            std::cerr << "Skipping this file and continuing with others..." << std::endl
                      << std::endl;
            continue;
        }

        if (!validateAndLinkModule(L, std::move(module), llFile)) {
            std::cerr << "Failed to link module from: " << llFile << std::endl;
            std::cerr << "Skipping this file and continuing with others..." << std::endl
                      << std::endl;
            continue;
        }

        successfullyLinked++;
    }

    // Check if at least one module was successfully linked
    if (successfullyLinked == 0) {
        std::cerr << "Fatal error: No modules were successfully linked!" << std::endl;
        std::cerr << "Cannot proceed without at least one valid module." << std::endl;
        return EXIT_FAILURE;
    }

    if (successfullyLinked < totalFiles) {
        std::cerr << "Warning: Only " << successfullyLinked << " out of " << totalFiles
                  << " modules were successfully linked." << std::endl;
        std::cerr << "Proceeding with available modules..." << std::endl << std::endl;
    } else {
        DEBUG_OUT << "All " << totalFiles << " modules successfully linked." << std::endl;
    }

    DEBUG_OUT << "Functions and their types:" << std::endl;
    for (const auto &item : initMemoryMap) {
        DEBUG_OUT << item.first << "::" << item.second << std::endl;
    }
    DEBUG_OUT << "------------------------" << std::endl;

    std::ifstream file(jsonFile);

    if (!file.is_open()) {
        std::cerr << "Failed to open file: " << jsonFile << std::endl;
        return EXIT_FAILURE;
    }

    json data;

    try {
        data = json::parse(file);
    } catch (nlohmann::detail::parse_error e) {
        std::cerr << "Invalid json: " << e.what() << std::endl;
        return EXIT_FAILURE;
    } catch (std::exception e) {
        std::cerr << "Error catched: " << e.what() << std::endl;
        return EXIT_FAILURE;
    }

    auto progNode = createProgramNode(data);

    for (const auto &tmpSub : subFunctions) {
        DEBUG_OUT << "Dfs in sub function: " << tmpSub << std::endl;

        for (const auto &item : subsNamesMap[tmpSub]) {
            DEBUG_OUT << "  DF name: " << item << std::endl;

            dfDimensionsMap[tmpSub][item] += subDimensionsIncreaseMap[tmpSub][item];

            if (dfDimensionsMap[tmpSub][item] == 0) {
                checkIncreaseDimension(tmpSub, item);
            }

            DEBUG_OUT << "    Dimension: " << dfDimensionsMap[tmpSub][item] << std::endl;
        }
    }

    progNode->codegen();

    file.close();

    std::error_code EC;
    std::string outFileName = "output.ll";
    std::string outDefinitionsFile = "definitions.txt";

    // Если указана директория проекта, создаём .BiLangIR и сохраняем туда
    if (!projectDir.empty()) {
        // Убираем trailing slash, если есть
        if (projectDir.back() == '/') {
            projectDir.pop_back();
        }

        std::string biLangIRDir = projectDir + "/.BiLangIR";

        // Проверяем существование директории .BiLangIR
        struct stat info;
        if (stat(biLangIRDir.c_str(), &info) != 0) {
            // Директория не существует, создаём её
            if (mkdir(biLangIRDir.c_str(), 0755) != 0) {
                std::cerr << "Failed to create directory: " << biLangIRDir << std::endl;
                return EXIT_FAILURE;
            }
            DEBUG_OUT << "Created directory: " << biLangIRDir << std::endl;
        } else if (!(info.st_mode & S_IFDIR)) {
            std::cerr << biLangIRDir << " exists but is not a directory" << std::endl;
            return EXIT_FAILURE;
        }

        outFileName = biLangIRDir + "/output.ll";
        outDefinitionsFile = biLangIRDir + "/definitions.txt";
    }

    raw_fd_ostream os(outFileName, EC, sys::fs::OF_None);

    if (EC) {
        std::cerr << "Failed to open file: " << outFileName << std::endl;
        return EXIT_FAILURE;
    }

    os << *TheModule;
    os.close();

    DEBUG_OUT << "Output written to: " << outFileName << std::endl;

    dumpDefinitions(outDefinitionsFile);

    if (ErrorHandler::getMode() == ErrorMode::SYSTEM) {
        if (!errorOutputFile.empty()) {
            ErrorHandler::errorsReport(output::FILE, errorOutputFile);
        } else {
            ErrorHandler::errorsReport(output::CONSOLE);
        }
    }

    return EXIT_SUCCESS;
}

void dumpDefinitions(const std::string& path) {
    nlohmann::json definitionsArray = nlohmann::json::array();
    nlohmann::json globalsArray = nlohmann::json::array();

    for (const auto& item : functionsMap) {
        nlohmann::json defJson;

        defJson["name"] = item.first;
        defJson["type"] = "extern";
        defJson["file"] = item.second.first;
        defJson["line"] = item.second.second;

        definitionsArray.push_back(defJson);
    }

    for (const auto& item : subFunctionsPositions) {
        nlohmann::json defJson;

        defJson["name"] = item.first;
        defJson["type"] = "struct";
        defJson["file"] = globalFaName; 
        defJson["line"] = item.second;

        definitionsArray.push_back(defJson);
    }

    for (const auto& item : globalConstantsMap) {
        nlohmann::json globJson;

        globJson["name"] = item.first;
        globJson["file"] = item.second.first;
        globJson["line"] = item.second.second;

        globalsArray.push_back(globJson);
    }

    std::ofstream outFile(path);
    if (!outFile.is_open()) {
        std::cerr << "Failed to open file for writing: " << path << std::endl;
        return;
    }

    nlohmann::json output;
    output["functions"] = definitionsArray;
    output["globals"] = globalsArray;
    outFile << output.dump(4);
    outFile.close();
}
