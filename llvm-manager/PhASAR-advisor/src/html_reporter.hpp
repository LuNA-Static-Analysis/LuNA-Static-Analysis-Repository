#ifndef HTML_REPORTER_HPP
#define HTML_REPORTER_HPP

#include <fstream>
#include <string>
#include <vector>
#include <ctime>
#include <sstream>
#include <iomanip>
#include <iostream>

class HTMLReporter {
private:
    std::string htmlContent;
    std::vector<std::string> sections;
    std::string title;

public:
    HTMLReporter(const std::string& reportTitle = "PhASAR Analysis Report")
        : title(reportTitle) {
        initializeHTML();
    }

    void initializeHTML() {
        htmlContent = R"(<!DOCTYPE html>
<html lang="ru">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>)" + title + R"(</title>
    <style>
        * {
            margin: 0;
            padding: 0;
            box-sizing: border-box;
        }
        
        body {
            font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            color: #333;
            line-height: 1.6;
            padding: 20px;
        }
        
        .container {
            max-width: 1200px;
            margin: 0 auto;
            background: white;
            border-radius: 10px;
            box-shadow: 0 10px 40px rgba(0, 0, 0, 0.1);
            overflow: hidden;
        }
        
        .header {
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            color: white;
            padding: 40px;
            text-align: center;
        }
        
        .header h1 {
            font-size: 2.5em;
            margin-bottom: 10px;
            text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.3);
        }
        
        .header p {
            font-size: 1.1em;
            opacity: 0.9;
        }
        
        .content {
            padding: 40px;
        }
        
        .timestamp {
            color: #666;
            font-size: 0.9em;
            margin-bottom: 30px;
            padding-bottom: 15px;
            border-bottom: 1px solid #eee;
        }
        
        .section {
            margin-bottom: 40px;
            border-left: 4px solid #667eea;
            padding-left: 20px;
        }
        
        .section h2 {
            color: #667eea;
            margin-bottom: 20px;
            font-size: 1.8em;
        }
        
        .section h3 {
            color: #764ba2;
            margin-top: 15px;
            margin-bottom: 10px;
            font-size: 1.3em;
        }
        
        .section p {
            margin-bottom: 15px;
            color: #555;
        }
        
        .info-box {
            background: #f8f9fa;
            border-left: 4px solid #667eea;
            padding: 15px;
            margin: 10px 0;
            border-radius: 4px;
        }
        
        .info-box strong {
            color: #667eea;
        }
        
        .stats {
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
            gap: 20px;
            margin: 20px 0;
        }
        
        .stat-card {
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            color: white;
            padding: 20px;
            border-radius: 8px;
            text-align: center;
            box-shadow: 0 4px 15px rgba(102, 126, 234, 0.3);
        }
        
        .stat-card .number {
            font-size: 2em;
            font-weight: bold;
            margin-bottom: 10px;
        }
        
        .stat-card .label {
            font-size: 0.9em;
            opacity: 0.9;
        }
        
        table {
            width: 100%;
            border-collapse: collapse;
            margin: 20px 0;
        }
        
        table th {
            background: #667eea;
            color: white;
            padding: 12px;
            text-align: left;
            font-weight: bold;
        }
        
        table td {
            padding: 12px;
            border-bottom: 1px solid #eee;
        }
        
        table tr:hover {
            background: #f5f5f5;
        }
        
        .code-block {
            background: #2d2d2d;
            color: #f8f8f2;
            padding: 15px;
            border-radius: 4px;
            overflow-x: auto;
            margin: 15px 0;
            font-family: 'Courier New', monospace;
            font-size: 0.9em;
            line-height: 1.4;
        }
        
        .footer {
            background: #f8f9fa;
            padding: 20px 40px;
            text-align: center;
            color: #666;
            border-top: 1px solid #eee;
            font-size: 0.9em;
        }
        
        .warning {
            background: #fff3cd;
            border-left: 4px solid #ffc107;
            color: #856404;
            padding: 15px;
            border-radius: 4px;
            margin: 15px 0;
        }
        
        .error {
            background: #f8d7da;
            border-left: 4px solid #dc3545;
            color: #721c24;
            padding: 15px;
            border-radius: 4px;
            margin: 15px 0;
        }
        
        .success {
            background: #d4edda;
            border-left: 4px solid #28a745;
            color: #155724;
            padding: 15px;
            border-radius: 4px;
            margin: 15px 0;
        }
    </style>
</head>
<body>
    <div class="container">
        <div class="header">
            <h1>)" + title + R"(</h1>
            <p>PhASAR Dynamic Analysis Report</p>
        </div>
        <div class="content">
            <div class="timestamp">
)";
        
        // Add timestamp
        auto now = std::time(nullptr);
        auto tm = *std::localtime(&now);
        std::ostringstream oss;
        oss << std::put_time(&tm, "%Y-%m-%d %H:%M:%S");
        htmlContent += "                Generated: " + oss.str() + "\n";
        htmlContent += "            </div>\n";
    }

    void addSection(const std::string& title, const std::string& content) {
        std::string section = R"(            <div class="section">
                <h2>)" + title + R"(</h2>
                <div>)" + content + R"(</div>
            </div>
)";
        sections.push_back(section);
    }

    void addStatistics(const std::vector<std::pair<std::string, std::string>>& stats) {
        std::string statsHTML = "            <div class=\"stats\">\n";
        for (const auto& stat : stats) {
            statsHTML += "                <div class=\"stat-card\">\n";
            statsHTML += "                    <div class=\"number\">" + stat.second + "</div>\n";
            statsHTML += "                    <div class=\"label\">" + stat.first + "</div>\n";
            statsHTML += "                </div>\n";
        }
        statsHTML += "            </div>\n";
        sections.push_back(statsHTML);
    }

    void addInfoBox(const std::string& label, const std::string& value) {
        std::string info = "            <div class=\"info-box\">\n";
        info += "                <strong>" + label + ":</strong> " + value + "\n";
        info += "            </div>\n";
        sections.push_back(info);
    }

    void addCodeBlock(const std::string& code) {
        std::string block = "            <div class=\"code-block\">\n";
        block += code + "\n";
        block += "            </div>\n";
        sections.push_back(block);
    }

    void addWarning(const std::string& message) {
        std::string warning = "            <div class=\"warning\">\n";
        warning += "                <strong>Warning:</strong> " + message + "\n";
        warning += "            </div>\n";
        sections.push_back(warning);
    }

    void addError(const std::string& message) {
        std::string error = "            <div class=\"error\">\n";
        error += "                <strong>Error:</strong> " + message + "\n";
        error += "            </div>\n";
        sections.push_back(error);
    }

    void addSuccess(const std::string& message) {
        std::string success = "            <div class=\"success\">\n";
        success += "                <strong>Success:</strong> " + message + "\n";
        success += "            </div>\n";
        sections.push_back(success);
    }

    void addTable(const std::vector<std::string>& headers, 
                  const std::vector<std::vector<std::string>>& rows) {
        std::string table = "            <table>\n";
        table += "                <thead>\n<tr>\n";
        for (const auto& header : headers) {
            table += "                    <th>" + header + "</th>\n";
        }
        table += "                </tr>\n</thead>\n<tbody>\n";
        for (const auto& row : rows) {
            table += "                <tr>\n";
            for (const auto& cell : row) {
                table += "                    <td>" + cell + "</td>\n";
            }
            table += "                </tr>\n";
        }
        table += "                </tbody>\n            </table>\n";
        sections.push_back(table);
    }

    bool saveToFile(const std::string& filepath) {
        std::ofstream file(filepath);
        if (!file.is_open()) {
            std::cerr << "Error: Could not open file '" << filepath << "' for writing.\n";
            return false;
        }

        file << htmlContent;
        for (const auto& section : sections) {
            file << section;
        }

        // Add footer
        auto now = std::time(nullptr);
        auto tm = *std::localtime(&now);
        std::ostringstream oss;
        oss << std::put_time(&tm, "%Y-%m-%d %H:%M:%S");
        
        file << R"(        </div>
        <div class="footer">
            <p>Report generated by PhASAR-advisor at )" + oss.str() + R"(</p>
        </div>
    </div>
</body>
</html>
)";

        file.close();
        std::cout << "HTML report saved to: " << filepath << "\n";
        return true;
    }

    std::string getHTML() const {
        std::string result = htmlContent;
        for (const auto& section : sections) {
            result += section;
        }
        result += R"(        </div>
    </div>
</body>
</html>
)";
        return result;
    }
};

#endif // HTML_REPORTER_HPP
