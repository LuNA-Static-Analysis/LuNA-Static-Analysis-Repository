#ifndef ERROR_HANDLER_HPP
#define ERROR_HANDLER_HPP

#include "error_nodes.hpp"

#include <fstream>
#include <iostream>
#include <string>
#include <vector>

enum class ErrorMode {
    SIMPLE,  // Вывод одной строкой с цветом
    SYSTEM   // Системный вывод
};

enum class output { FILE, CONSOLE };

class ErrorHandler {
  private:
    static ErrorMode currentMode;

    // ANSI цветовые коды
    static constexpr const char *RED = "\033[1;31m";
    static constexpr const char *RESET = "\033[0m";

    static inline std::vector<ErrorBase> errors;

  public:
    static void setMode(ErrorMode mode) { currentMode = mode; }

    static ErrorMode getMode() { return currentMode; }

    static void printError(const std::string &message, ErrorBase errorDetails) {
        if (currentMode == ErrorMode::SIMPLE) {
            std::cout << RED << "[FOUND ERROR]" << RESET << " " << message << std::endl;
        } else if (currentMode == ErrorMode::SYSTEM) {
            errors.push_back(std::move(errorDetails));
        }
    }

    static void errorsReport(output outMode, std::string filePath = "") {
        nlohmann::json j = nlohmann::json::array();

        for (const auto &err : errors) {
            nlohmann::json tmpError = err.to_json();
            if (tmpError.empty()) {
                continue;
            }

            j.push_back(tmpError);
        }

        if (outMode == output::CONSOLE) {
            std::cout << j.dump(4) << std::endl;
        } else if (outMode == output::FILE && !filePath.empty()) {
            nlohmann::json existingErrors = nlohmann::json::array();

            // Попытка прочитать существующий файл
            std::ifstream inFile(filePath);
            if (inFile.is_open()) {
                try {
                    nlohmann::json fileContent;
                    inFile >> fileContent;

                    // Проверяем, что это массив
                    if (fileContent.is_array()) {
                        existingErrors = fileContent;
                    }
                } catch (const nlohmann::json::exception &e) {
                    // Если файл пустой или невалидный JSON, начинаем с пустого массива
                    existingErrors = nlohmann::json::array();
                }
                inFile.close();
            }

            // Добавляем новые ошибки в существующий массив, если их там ещё нет
            for (const auto &newError : j) {
                bool isDuplicate = false;

                // Проверяем, есть ли уже такая ошибка в массиве
                for (const auto &existingError : existingErrors) {
                    if (existingError == newError) {
                        isDuplicate = true;
                        break;
                    }
                }

                // Добавляем только если это не дубликат
                if (!isDuplicate) {
                    existingErrors.push_back(newError);
                }
            }

            // Записываем обновлённый массив обратно в файл
            std::ofstream outFile(filePath);
            if (outFile.is_open()) {
                outFile << existingErrors.dump(4) << std::endl;
                outFile.close();
            } else {
                std::cerr << "Could not open file for writing errors: " << filePath << std::endl;
            }
        }
    }
};

inline ErrorMode ErrorHandler::currentMode = ErrorMode::SYSTEM;

#endif  // ERROR_HANDLER_HPP
