#ifndef HTML_REPORTER_HPP
#define HTML_REPORTER_HPP

#include <fstream>
#include <string>
#include <vector>
#include <ctime>
#include <sstream>
#include <iomanip>
#include <iostream>
#include <filesystem>

class HTMLReporter {
private:
    std::string htmlContent;
    std::vector<std::string> sections;
    std::string title;
    std::string cssFilePath;

    std::string readCSS() const {
        std::ifstream file(cssFilePath);
        if (!file.is_open()) {
            return "/* Could not open CSS file: " + cssFilePath + " */\n"
                   "body { font-family: sans-serif; padding: 20px; }";
        }
        std::stringstream buffer;
        buffer << file.rdbuf();
        return buffer.str();
    }

public:
    HTMLReporter(const std::string& reportTitle = "PhASAR Analysis Report", 
                 const std::string& stylePath = "report_style.css")
        : title(reportTitle), cssFilePath(stylePath) {
        initializeHTML();
    }

    void initializeHTML() {
        htmlContent = "<!DOCTYPE html>\n"
                      "<html lang=\"ru\">\n"
                      "<head>\n"
                      "    <meta charset=\"UTF-8\">\n"
                      "    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n"
                      "    <title>" + title + "</title>\n"
                      "    <style>\n" + readCSS() + "\n"
                      "    </style>\n"
                      "</head>\n"
                      "<body>\n"
                      "    <div class=\"container\">\n"
                      "        <div class=\"header\">\n"
                      "            <h1>" + title + "</h1>\n"
                      "            <p>PhASAR Dynamic Analysis Report</p>\n"
                      "        </div>\n"
                      "        <div class=\"content\">\n"
                      "            <div class=\"timestamp\">\n";
        
        // Add timestamp
        auto now = std::time(nullptr);
        auto tm = *std::localtime(&now);
        std::ostringstream oss;
        oss << std::put_time(&tm, "%Y-%m-%d %H:%M:%S");
        htmlContent += "                Generated: " + oss.str() + "\n";
        htmlContent += "            </div>\n";
    }

    void addSection(const std::string& title, const std::string& content) {
        std::string section = R"(            <details class="section" open>
                <summary><h2>)" + title + R"(</h2></summary>
                <div class="section-content" style="overflow-x: auto; max-width: 100%;">)" + content + R"(</div>
            </details>
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

    void addImage(const std::string& altText, const std::string& imagePath) {
        std::string img = "                <img src=\"" + imagePath + "\" alt=\"" + altText + "\" style=\"max-width: 100%; height: auto; border-radius: 4px; box-shadow: 0 4px 8px rgba(0,0,0,0.1); margin: 15px 0;\">\n";
        
        if (!sections.empty() && sections.back().find("</details>") != std::string::npos) {
            std::string& lastSection = sections.back();
            size_t insertPos = lastSection.rfind("</details>");
            lastSection.insert(insertPos, img);
        } else {
            sections.push_back("            <div class=\"image-container\">\n" + img + "            </div>\n");
        }
    }

    void addLinkedImage(const std::string& altText, const std::string& imagePath, bool showPreview = true) {
        std::string content;
        if (showPreview) {
            content = "                <a href=\"" + imagePath + "\" target=\"_blank\" rel=\"noopener noreferrer\" title=\"Open in new window\">\n"
                      "                    <img src=\"" + imagePath + "\" alt=\"" + altText + "\" style=\"max-width: 100%; height: auto; border-radius: 4px; box-shadow: 0 4px 8px rgba(0,0,0,0.1); margin: 15px 0;\">\n"
                      "                </a>\n";
        } else {
            content = "                <div style=\"margin: 15px 0;\">\n"
                      "                    <strong>" + altText + ":</strong> <a href=\"" + imagePath + "\" target=\"_blank\" rel=\"noopener noreferrer\" title=\"Open in new window\">" + imagePath + "</a>\n"
                      "                </div>\n";
        }
        
        if (!sections.empty() && sections.back().find("</details>") != std::string::npos) {
            std::string& lastSection = sections.back();
            size_t insertPos = lastSection.rfind("</details>");
            lastSection.insert(insertPos, content);
        } else {
            sections.push_back("            <div class=\"image-container\">\n" + content + "            </div>\n");
        }
    }

    bool saveToFile(const std::string& filepath) {
        std::ofstream file(filepath, std::ios::out | std::ios::trunc);
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
        
        file << R"HTML(        </div>
        <div class="footer">
            <p>Report generated by PhASAR-advisor at )HTML" + oss.str() + R"HTML(</p>
        </div>
        
        <!-- Table of Contents -->
        <div id="toc-container">
            <div id="toc-panel">
                <h3>Оглавление</h3>
                <ul id="toc-list"></ul>
            </div>
            <div id="toc-resizer"></div>
            <button id="toc-toggle-btn" title="Скрыть/Показать меню">❮</button>
        </div>

        <script>
            document.addEventListener("DOMContentLoaded", function() {
                document.body.style.transition = "margin-left 0.3s ease";
                const tocContainer = document.getElementById("toc-container");
                const tocPanel = document.getElementById("toc-panel");
                const resizer = document.getElementById("toc-resizer");
                
                let isResizing = false;
                
                resizer.addEventListener("mousedown", (e) => {
                    isResizing = true;
                    document.body.style.userSelect = "none";
                    document.body.style.cursor = "ew-resize";
                    document.body.style.transition = "none"; // Отключаем плавность при ресайзе
                    tocContainer.style.transition = "none";
                });
                
                document.addEventListener("mousemove", (e) => {
                    if (!isResizing || !tocVisible) return;
                    let newWidth = e.clientX;
                    if (newWidth < 150) newWidth = 150;
                    let maxW = window.innerWidth * 0.5;
                    if (newWidth > maxW) newWidth = maxW;
                    tocPanel.style.width = newWidth + "px";
                });
                
                document.addEventListener("mouseup", () => {
                    if (isResizing) {
                        isResizing = false;
                        document.body.style.userSelect = "";
                        document.body.style.cursor = "";
                        document.body.style.transition = "margin-left 0.3s ease"; // Возвращаем анимацию
                        tocContainer.style.transition = "transform 0.3s ease";
                    }
                });
                
                // Кнопка скрытия/показа меню (ищет её по ID)
                const tocToggleBtn = document.getElementById("toc-toggle-btn");
                let tocVisible = true;
                
                if (tocToggleBtn && tocContainer) {
                    tocToggleBtn.addEventListener("click", () => {
                        tocVisible = !tocVisible;
                        if (tocVisible) {
                            tocContainer.style.transform = "translateX(0)";
                            tocToggleBtn.innerText = "❮";
                            document.body.style.marginLeft = (tocPanel.offsetWidth + resizer.offsetWidth + 40) + "px";
                        } else {
                            tocContainer.style.transform = "translateX(-100%)";
                            tocToggleBtn.innerText = "❯";
                            document.body.style.marginLeft = "40px";
                        }
                    });
                }
                
                // Настраиваем слежение за изменением ширины панели
                new ResizeObserver(() => {
                    if (tocVisible) {
                        document.body.style.marginLeft = (tocPanel.offsetWidth + resizer.offsetWidth + 40) + "px";
                    }
                }).observe(tocPanel);
                
                const tocList = document.getElementById("toc-list");
                const sections = document.querySelectorAll("details.section");
                
                sections.forEach((sec, index) => {
                    const h2 = sec.querySelector("h2");
                    if (!h2) return;
                    
                    const titleText = h2.innerText;
                    const id = "sec-" + index;
                    sec.id = id; // Даем секции ID для навигации
                    
                    const li = document.createElement("li");
                    
                    const a = document.createElement("a");
                    a.href = "#" + id;
                    a.innerText = titleText;
                    
                    // При клике: разворачиваем секцию и делаем плавный скролл
                    a.addEventListener("click", (e) => {
                        e.preventDefault();
                        sec.setAttribute("open", "");
                        sec.scrollIntoView({behavior: "smooth", block: "start"});
                    });
                    
                    li.appendChild(a);
                    tocList.appendChild(li);
                });
            });
        </script>

        <!-- Theme Toggle Button -->
        <button id="theme-toggle-btn" title="Переключить тему">🌙</button>

        <script>
            // Theme Toggle Logic
            const themeBtn = document.getElementById("theme-toggle-btn");
            const savedTheme = localStorage.getItem("report_theme");
            
            const enableDark = () => {
                document.body.classList.add("dark-mode");
                themeBtn.innerText = "☀️";
                localStorage.setItem("report_theme", "dark");
            };
            
            const enableLight = () => {
                document.body.classList.remove("dark-mode");
                themeBtn.innerText = "🌙";
                localStorage.setItem("report_theme", "light");
            };
            
            if (savedTheme === "dark") enableDark();
            
            themeBtn.addEventListener("click", () => {
                if (document.body.classList.contains("dark-mode")) enableLight();
                else enableDark();
            });
        </script>

        <!-- Floating Controls -->
        <div class="floating-controls">
            <div class="fc-group">
                <button class="fc-btn fc-btn-expand" onclick="document.querySelectorAll('details').forEach(d => d.setAttribute('open', ''))" title="Развернуть все секции">&#x25BC;</button>
                <button class="fc-btn fc-btn-collapse" onclick="document.querySelectorAll('details').forEach(d => d.removeAttribute('open'))" title="Свернуть все секции">&#x25B2;</button>
            </div>
            <button class="fc-btn-up" onclick="window.scrollTo({top: 0, behavior: 'smooth'})" title="Прокрутить наверх">&#x2191;</button>
        </div>
    </div>
</body>
</html>
)HTML";

        file.close();
        std::cout << "HTML report saved to: " << std::filesystem::absolute(filepath).string() << "\n";
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
