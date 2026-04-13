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
    cout << "  --basic                - only basic implementation\n";
    cout << "  --detailed             - only detailed implementation\n";
    cout << "  --both                 - Both types of analysis (default)\n";
    cout << "  --logging              - Enable detailed logging\n";
    cout << "  --include-private      - Include private/mangled functions (with _)\n";
    cout << "  --html-report          - Generate HTML report instead of console output\n";
    cout << "  --artifacts-dir=<path> - Directory to save generated artifacts (default: ./PhASAR-artifacts)\n";
    cout << "  --css-file=<path>      - Path to CSS file for HTML report (default: $ADAPT_HOME/PhASAR-advisor/src/report_style.css)\n";
    cout << "  --error-checking       - Error checking mode (requires helper file and output file path)\n";
    cout << "  -h, --help             - Show this help message\n\n";

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
    return arg == "--basic" || 
           arg == "--detailed" || 
           arg == "--both" || 
           arg == "--logging" ||
           arg == "--include-private" ||
           arg == "--html-report" ||
           arg.starts_with("--artifacts-dir=") ||
           arg.starts_with("--css-file=") ||
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

    opts.choice = AnalysisChoice::Both;            // По умолчанию выполняем оба анализа
    opts.logging = false;                          // По умолчанию выключено
    opts.includePrivateFunctions = false;          // По умолчанию пропускаем
    opts.outputFormat = OutputFormat::CONSOLE;     // По умолчанию вывод в консоль
    
    for (const string& arg : optionArgs) {
        if (arg == "--basic") {
            opts.choice = AnalysisChoice::BasicAnalysis;
        } else if (arg == "--detailed") {
            opts.choice = AnalysisChoice::DetailedAnalysis;
        } else if (arg == "--both") {
            opts.choice = AnalysisChoice::Both;
        } else if (arg == "--logging") {
            opts.logging = true;
        } else if (arg == "--include-private") {
            opts.includePrivateFunctions = true;
        } else if (arg == "--html-report") {
            opts.outputFormat = OutputFormat::HTML;
        } else if (arg.starts_with("--artifacts-dir=")) {
            opts.artifactsDir = arg.substr(16); // length of "--artifacts-dir="
        } else if (arg.starts_with("--css-file=")) {
            opts.cssFilePath = arg.substr(11); // length of "--css-file="
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

    HTMLReporter htmlReporter("PhASAR Analysis Report", opts.cssFilePath);

    runCallGraphAnalysis(IRDB, opts, htmlReporter);
    runBasicInfo(IRDB, opts, htmlReporter);

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
    
    string relativePath = llvmIRFile;
    llvmIRFile = filesystem::absolute(relativePath).string();
    
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

    error_code ec;
            
    if (!filesystem::exists(opts.artifactsDir)) {
        if (!filesystem::create_directories(opts.artifactsDir, ec) && ec.value() != 0) {
            cerr << "Error: Could not create directory '" << opts.artifactsDir << "': " << ec.message() << "\n";
            exit(EXIT_FAILURE);
        }
    } else if (!filesystem::is_directory(opts.artifactsDir)) {
        cerr << "Error: Path '" << opts.artifactsDir << "' exists but is not a directory.\n";
        exit(EXIT_FAILURE);
    }

    if (!fileExists(opts.cssFilePath)) {
        cerr << "Error: CSS file '" << opts.cssFilePath << "' not found.\n";
        exit(EXIT_FAILURE);
    }
    
    vector<AnalysisType> analyses = parseAnalyses(analysisArgs);
    
    if (analyses.empty()) {
        cerr << "Error: No valid analyses specified.\n";
        printUsage(argv[0]);
        return EXIT_FAILURE;
    }
    
    // Initialize HTML reporter if needed
    HTMLReporter htmlReporter("PhASAR Analysis Report", opts.cssFilePath);
    
    // Add general info to HTML report
    if (opts.outputFormat == OutputFormat::HTML) {
        htmlReporter.addSection("Analysis Configuration", 
            "<strong>Input File:</strong> " + llvmIRFile + "<br/>" +
            "<strong>Analysis Mode:</strong> " + 
            (opts.choice == AnalysisChoice::BasicAnalysis ? "Basic Implementation" : 
             opts.choice == AnalysisChoice::DetailedAnalysis ? "Detailed Implementation" : "Both") + "<br/>" +
            "<strong>Include Private Functions:</strong> " + (opts.includePrivateFunctions ? "Yes" : "No")
        );
    }
    
    cout << "Initializing PhASAR for file: " << llvmIRFile << "\n";

    try {
        LLVMProjectIRDB IRDB(llvmIRFile);

        cout << "LLVM IR loaded successfully.\n";
        
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
                    cout << "\n=== Running Basic Info Analysis ===\n";
                    runBasicInfo(IRDB, opts, htmlReporter);
                    break;
                case AnalysisType::CALLGRAPH:
                    cout << "\n=== Running Call Graph Analysis ===\n";
                    runCallGraphAnalysis(IRDB, opts, htmlReporter);
                    break;
                case AnalysisType::ALIAS:
                    cout << "\n=== Running Alias Analysis ===\n";
                    runAliasAnalysis(IRDB, opts, htmlReporter);
                    break;
                // case AnalysisType::TAINT: 
                //     cout << "\n=== Running Taint Analysis ===\n";
                //     runTaintAnalysis(IRDB, opts, htmlReporter);
                //     break;
                case AnalysisType::CONSTANT_PROPAGATION:
                    cout << "\n=== Running Constant Propagation Analysis ===\n";
                    runConstantPropagation(IRDB, opts, htmlReporter);
                    break;
                case AnalysisType::UNINITIALIZED_VARIABLES:
                    cout << "\n=== Running Uninitialized Variables Analysis ===\n";
                    runUninitializedVariables(IRDB, opts, htmlReporter);
                    break;
                case AnalysisType::TYPE_ANALYSIS:
                    cout << "\n=== Running Type Analysis ===\n";
                    runTypeAnalysis(IRDB, opts, htmlReporter);
                    break;
                case AnalysisType::COMPLEXITY:
                    cout << "\n=== Running Code Complexity Analysis ===\n";
                    runCodeComplexityAnalysis(IRDB, opts, htmlReporter);
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
            htmlReporter.saveToFile(opts.artifactsDir + "/phasar_analysis_report.html");
        }
    } catch (const exception& e) {
        cerr << "Critical Error: " << e.what() << "\n";
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}
