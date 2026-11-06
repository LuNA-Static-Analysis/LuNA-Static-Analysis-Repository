#pragma once

#include <cstdint>
#include "ucenv.h"

using namespace luna::ucenv;

static inline uint64_t _adapt_hash(const InputDF &df)
{
    if (df.get_type() != ValueType::TYPE_VALUE) {
        return 0;
    }

    const void *data = df.get_data();
    const std::size_t len = df.get_size();

    const uint8_t* p = (const uint8_t*)data;

    uint64_t hash = 14695981039346656037ULL; // offset basis
    const uint64_t prime = 1099511628211ULL;

    for (std::size_t i = 0; i < len; i++)
    {
        hash ^= p[i];
        hash *= prime;
    }

    return hash;
}