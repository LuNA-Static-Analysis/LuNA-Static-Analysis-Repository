#include "dependes.hpp"

#include "analyses/BasicInfo.hpp"
#include "analyses/TaintAnalysis.hpp"
#include "analyses/ConstantPropagation.hpp"
#include "analyses/UninitializedVariables.hpp"
#include "analyses/TypeAnalysis.hpp"
#include "analyses/CallGraph.hpp"
#include "analyses/AliasInfo.hpp"
#include "analyses/CodeComplexity.hpp"
#include "html_reporter.hpp"

#include <cstdlib>
#include <vector>

#include <llvm/IR/Instructions.h>
#include <llvm/Support/Casting.h>

#include <phasar/PhasarLLVM/HelperAnalyses.h>

using namespace std::string_literals;

enum class AnalysisType {
    // TAINT,
    CONSTANT_PROPAGATION,
    UNINITIALIZED_VARIABLES,
    TYPE_ANALYSIS,
    BASIC_INFO,
    CALLGRAPH,
    ALIAS,
    COMPLEXITY,
    ALL
};

const unordered_map<string, AnalysisType> ANALYSIS_MAP = {
    // {"taint", AnalysisType::TAINT},
    {"const", AnalysisType::CONSTANT_PROPAGATION},
    {"uninit", AnalysisType::UNINITIALIZED_VARIABLES},
    {"types", AnalysisType::TYPE_ANALYSIS},
    {"info", AnalysisType::BASIC_INFO},
    {"callgraph", AnalysisType::CALLGRAPH},
    {"alias", AnalysisType::ALIAS},
    {"complexity", AnalysisType::COMPLEXITY},
    {"all", AnalysisType::ALL}
};

void printUsage(const string& programName) {
    cout << "Usage: " << programName << " <llvm_ir_file> [options] [analyses...]\n";
    cout << "   or: " << programName << " --error-checking <helper_file> <llvm_ir_file> <output_file>\n\n";
    
    cout << "Options:\n";
    cout << "  --my-analysis      - only my analysis implementation\n";
    cout << "  --phasar           - only PhASAR analysis (default)\n";
    cout << "  --both             - Both types of analysis\n";
    cout << "  --logging          - Enable detailed logging\n";
    cout << "  --include-private  - Include private/mangled functions (with _)\n";
    cout << "  --html-report      - Generate HTML report instead of console output\n";
    cout << "  --error-checking   - Error checking mode (requires helper file and output file path)\n";
    cout << "  -h, --help         - Show this help message\n\n";

    cout << "Available analyses:\n";
    // cout << "  taint      - Taint analysis (tracking tainted data)\n";
    cout << "  const      - Constant propagation\n";
    cout << "  uninit     - Uninitialized variables\n";
    cout << "  types      - Type analysis\n";
    cout << "  info       - Basic information about IR\n";
    cout << "  callgraph  - Call graph analysis\n";
    cout << "  alias      - Alias analysis\n";
    cout << "  complexity - Code complexity analysis\n";
    cout << "  all        - All available analyses (default)\n";
}

bool fileExists(const string& filepath) {
    return filesystem::exists(filepath) && filesystem::is_regular_file(filepath);
}

bool isOptionFlag(const string& arg) {
    return arg == "--my-analysis" || 
           arg == "--phasar" || 
           arg == "--both" || 
           arg == "--logging" ||
           arg == "--include-private" ||
           arg == "--html-report" ||
           arg == "-h" ||
           arg == "--help";
}

void separateArguments(const vector<string>& args, 
                       vector<string>& optionArgs, 
                       vector<string>& analysisArgs) {
    for (const string& arg : args) {
        if (isOptionFlag(arg)) {
            optionArgs.push_back(arg);
        } else {
            analysisArgs.push_back(arg);
        }
    }
}

Options parseOptions(const vector<string>& optionArgs) {
    Options opts;

    opts.choice = AnalysisChoice::PhasarAnalysis;  // По умолчанию
    opts.logging = false;                          // По умолчанию выключено
    opts.includePrivateFunctions = false;          // По умолчанию пропускаем
    opts.outputFormat = OutputFormat::CONSOLE;     // По умолчанию вывод в консоль
    
    for (const string& arg : optionArgs) {
        if (arg == "--my-analysis") {
            opts.choice = AnalysisChoice::MyRealizedAnalysis;
        } else if (arg == "--phasar") {
            opts.choice = AnalysisChoice::PhasarAnalysis;
        } else if (arg == "--both") {
            opts.choice = AnalysisChoice::Both;
        } else if (arg == "--logging") {
            opts.logging = true;
        } else if (arg == "--include-private") {
            opts.includePrivateFunctions = true;
        } else if (arg == "--html-report") {
            opts.outputFormat = OutputFormat::HTML;
        } 
    }
    
    return opts;
}

vector<AnalysisType> parseAnalyses(const vector<string>& analysisArgs) {
    vector<AnalysisType> analyses;
    unordered_set<AnalysisType> uniqueAnalyses;
    
    for (const string& arg : analysisArgs) {
        string lowerArg = arg;
        transform(lowerArg.begin(), lowerArg.end(), lowerArg.begin(), ::tolower);
        
        auto it = ANALYSIS_MAP.find(lowerArg);
        if (it != ANALYSIS_MAP.end()) {
            if (it->second == AnalysisType::ALL) {
                // Если указан "all", возвращаем все анализы
                return {
                    AnalysisType::BASIC_INFO,
                    AnalysisType::CALLGRAPH,
                    AnalysisType::ALIAS,
                    // AnalysisType::TAINT,
                    AnalysisType::CONSTANT_PROPAGATION,
                    AnalysisType::UNINITIALIZED_VARIABLES,
                    AnalysisType::TYPE_ANALYSIS,
                    AnalysisType::COMPLEXITY
                };
            } else if (uniqueAnalyses.find(it->second) == uniqueAnalyses.end()) {
                analyses.push_back(it->second);
                uniqueAnalyses.insert(it->second);
            }
        } else {
            cerr << "Warning: Unknown analysis '" << arg << "', skipping.\n";
        }
    }
    
    return analyses;
}

void runErrorCheckingMode(const string& helperFile, const string& llvmIRFile, const string& outputFile) {
    if (!fileExists(helperFile)) {
        cerr << "Error: Helper file '" << helperFile << "' not found.\n";
        throw runtime_error("Helper file not found");
    }
    
    if (!fileExists(llvmIRFile)) {
        cerr << "Error: LLVM IR file '" << llvmIRFile << "' not found.\n";
        throw runtime_error("LLVM IR file not found");
    }

    Options opts;

    opts.useErrorCheckingAlgorithm = true;
    opts.helperFile = helperFile;
    opts.outputFile = outputFile;

    LLVMProjectIRDB IRDB(llvmIRFile);

    runCallGraphAnalysis(IRDB, opts);
    runBasicInfo(IRDB, opts);

    ErrorHandler::errorsReport(output::FILE, opts.outputFile);
}

int main(int argc, char* argv[]) {
    if (argc < 2) {
        printUsage(argv[0]);
        return EXIT_SUCCESS;
    }
    
    // Проверка на запрос справки
    for (int i = 1; i < argc; ++i) {
        string arg = argv[i];
        if (arg == "-h" || arg == "--help") {
            printUsage(argv[0]);
            return EXIT_SUCCESS;
        }
    }
    
    // Проверка на режим error-checking
    if (argc >= 5 && string(argv[1]) == "--error-checking") {
        string helperFile = argv[2];
        string llvmIRFile = argv[3];
        string outputFile = argv[4];
        
        try {
            runErrorCheckingMode(helperFile, llvmIRFile, outputFile);
            return EXIT_SUCCESS;
        } catch (const exception& e) {
            cerr << "Critical Error in error-checking mode: " << e.what() << "\n";
            return EXIT_FAILURE;
        }
    }
    
    string llvmIRFile = argv[1];
    
    if (!fileExists(llvmIRFile)) {
        cerr << "Error: File '" << llvmIRFile << "' not found or is not a regular file.\n";
        return EXIT_FAILURE;
    }
    
    vector<string> args;
    for (int i = 2; i < argc; ++i) {
        args.push_back(argv[i]);
    }
    
    vector<string> optionArgs;
    vector<string> analysisArgs;
    
    separateArguments(args, optionArgs, analysisArgs);
    
    if (analysisArgs.empty()) {
        analysisArgs.push_back("all");
    }
    
    Options opts = parseOptions(optionArgs);
    
    vector<AnalysisType> analyses = parseAnalyses(analysisArgs);
    
    if (analyses.empty()) {
        cerr << "Error: No valid analyses specified.\n";
        printUsage(argv[0]);
        return EXIT_FAILURE;
    }
    
    // Initialize HTML reporter if needed
    HTMLReporter htmlReporter("PhASAR Analysis Report");
    
    // Add general info to HTML report
    if (opts.outputFormat == OutputFormat::HTML) {
        htmlReporter.addSection("Analysis Configuration", 
            "<strong>Input File:</strong> " + llvmIRFile + "<br/>" +
            "<strong>Analysis Mode:</strong> " + 
            (opts.choice == AnalysisChoice::MyRealizedAnalysis ? "My Implementation" : 
             opts.choice == AnalysisChoice::PhasarAnalysis ? "PhASAR" : "Both") + "<br/>" +
            "<strong>Include Private Functions:</strong> " + (opts.includePrivateFunctions ? "Yes" : "No")
        );
    }
    
    cout << "Initializing PhASAR for file: " << llvmIRFile << "\n";

    try {
        LLVMProjectIRDB IRDB(llvmIRFile);
        
        // Collect analysis names for report
        vector<string> analysisNames;
        for (AnalysisType analysis : analyses) {
            switch (analysis) {
                case AnalysisType::BASIC_INFO:
                    analysisNames.push_back("Basic Info");
                    break;
                case AnalysisType::CALLGRAPH:
                    analysisNames.push_back("Call Graph");
                    break;
                case AnalysisType::ALIAS:
                    analysisNames.push_back("Alias Analysis");
                    break;
                case AnalysisType::CONSTANT_PROPAGATION:
                    analysisNames.push_back("Constant Propagation");
                    break;
                case AnalysisType::UNINITIALIZED_VARIABLES:
                    analysisNames.push_back("Uninitialized Variables");
                    break;
                case AnalysisType::TYPE_ANALYSIS:
                    analysisNames.push_back("Type Analysis");
                    break;
                case AnalysisType::COMPLEXITY:
                    analysisNames.push_back("Code Complexity");
                    break;
                case AnalysisType::ALL:
                    break;
                default:
                    break;
            }
        }
        
        if (opts.outputFormat == OutputFormat::HTML && !analysisNames.empty()) {
            string analysesList = "<ul>";
            for (const auto& name : analysisNames) {
                analysesList += "<li>" + name + "</li>";
            }
            analysesList += "</ul>";
            htmlReporter.addSection("Executed Analyses", analysesList);
        }
        
        for (AnalysisType analysis : analyses) {
            switch (analysis) {
                case AnalysisType::BASIC_INFO:
                    runBasicInfo(IRDB, opts);
                    break;
                case AnalysisType::CALLGRAPH:
                    runCallGraphAnalysis(IRDB, opts);
                    break;
                case AnalysisType::ALIAS:
                    runAliasAnalysis(IRDB, opts);
                    break;
                // case AnalysisType::TAINT: 
                //     runTaintAnalysis(IRDB, opts);
                //     break;
                case AnalysisType::CONSTANT_PROPAGATION:
                    runConstantPropagation(IRDB, opts);
                    break;
                case AnalysisType::UNINITIALIZED_VARIABLES:
                    runUninitializedVariables(IRDB, opts);
                    break;
                case AnalysisType::TYPE_ANALYSIS:
                    runTypeAnalysis(IRDB, opts);
                    break;
                case AnalysisType::COMPLEXITY:
                    runCodeComplexityAnalysis(IRDB, opts);
                    break;
                case AnalysisType::ALL:
                    // Уже обработано в parseAnalyses
                    break;
                default:
                    cerr << "Error: Unknown analysis type.\n";
                    break;
            }
        }
        
        cout << "\n=== ALL ANALYSES COMPLETED ===\n";
        
        // Generate final report if needed
        if (opts.outputFormat == OutputFormat::HTML) {
            htmlReporter.addSuccess("All analyses completed successfully!");
            htmlReporter.saveToFile("phasar_analysis_report.html");
        }
    } catch (const exception& e) {
        cerr << "Critical Error: " << e.what() << "\n";
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}
