#include <stdlib.h>
#include <stdio.h>
#include <ucenv.h>
#include <stdexcept>
#include <atomic>
#include <cstdint>
#include <iomanip>
#include <random>
#include <sstream>
#include <iostream>
#include <fstream>
#include <string>

using namespace luna::ucenv;

namespace {
    std::string make_filename() {
        static const std::uint64_t id0 = [] {
            std::random_device rd;
            return (std::uint64_t(rd()) << 32) | rd();
        }();

        static const std::uint64_t id1 = [] {
            std::random_device rd;
            return (std::uint64_t(rd()) << 32) | rd();
        }();

        static std::atomic<std::uint64_t> counter{0};

        const auto n = counter.fetch_add(1, std::memory_order_relaxed);

        std::ostringstream ss;
        ss << "adapt_error_"
           << std::hex
           << std::setw(16) << std::setfill('0') << id0
           << std::setw(16) << std::setfill('0') << id1
           << "_"
           << std::dec << n
           << ".dat";

        return ss.str();
    }
}

extern "C" {
    void _adapt_assert(int condition, char const *condition_string) {
        if (condition) {
            return;
        }

        {
            std::ostringstream ss;
            ss << "Assertion failed: "
               << condition_string
               << "\n";
            std::cerr << ss.str();
        }

        {
            std::ofstream error_file(make_filename());
            error_file << "--- ADAPT ERROR BEGIN ---"
                       << "\n"
                       << condition_string
                       << "\n"
                       << "--- ADAPT ERROR END ---"
                       << std::endl;
        }
        throw std::runtime_error("adapt: generated assertion failed");
        // exit(EXIT_FAILURE);
    }
}
