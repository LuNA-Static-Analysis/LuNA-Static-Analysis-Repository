#ifndef DEBUG_HPP
#define DEBUG_HPP

#include <iostream>
#include <string>

// ANSI color codes
#define DEBUG_COLOR "\033[1;35m"  // Bright magenta/purple
#define RESET_COLOR "\033[0m"

// Global debug flag
extern bool g_debugEnabled;

// Debug output macro
#define DEBUG_PRINT(msg)                                                                           \
    do {                                                                                           \
        if (g_debugEnabled) {                                                                      \
            std::cout << DEBUG_COLOR << "[DEBUG] " << RESET_COLOR << msg << std::endl;            \
        }                                                                                          \
    } while (0)

// Debug output with stream-style syntax
#define DEBUG_OUT                                                                                  \
    if (g_debugEnabled)                                                                            \
    std::cout << DEBUG_COLOR << "[DEBUG] " << RESET_COLOR

#endif  // DEBUG_HPP
