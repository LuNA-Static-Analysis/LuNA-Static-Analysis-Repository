#pragma once

#include <string>
#include <stdexcept>

class AdaptAssertionError : public std::runtime_error
{
public:
    AdaptAssertionError(const std::string &msg) : std::runtime_error(msg)
    {}
};

extern "C" {
    void _adapt_assert(int condition, char const *condition_string);
}
