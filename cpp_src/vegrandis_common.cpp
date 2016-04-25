/* The MIT License (MIT)

   Copyright (c) 2016 Guilherme Andrade <vegrandis(at)gandrade(dot)net>

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in all
   copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE.

   */

#include "vegrandis_common.h"
#include <cstring>

namespace memory_order {
    constexpr const char* RELAXED = "memory_order_relaxed";
    constexpr const char* CONSUME = "memory_order_consume";
    constexpr const char* ACQUIRE = "memory_order_acquire";
    constexpr const char* RELEASE = "memory_order_release";
    constexpr const char* ACQ_REL = "memory_order_acq_rel";
    constexpr const char* SEQ_CST = "memory_order_seq_cst";
}

bool extract_memory_order(ErlNifEnv* env, ERL_NIF_TERM term, std::memory_order& out) {
    char order_str[32] = {'\0'};
    if (!enif_get_atom(env, term, order_str, 32, ERL_NIF_LATIN1))
        return false;

    if (strcmp(order_str, memory_order::RELAXED) == 0) {
        out = std::memory_order::memory_order_relaxed;
        return true;
    }
    if (strcmp(order_str, memory_order::CONSUME) == 0) {
        out = std::memory_order::memory_order_consume;
        return true;
    }
    if (strcmp(order_str, memory_order::ACQUIRE) == 0) {
        out = std::memory_order::memory_order_acquire;
        return true;
    }
    if (strcmp(order_str, memory_order::RELEASE) == 0) {
        out = std::memory_order::memory_order_release;
        return true;
    }
    if (strcmp(order_str, memory_order::ACQ_REL) == 0) {
        out = std::memory_order::memory_order_acq_rel;
        return true;
    }
    if (strcmp(order_str, memory_order::SEQ_CST) == 0) {
        out = std::memory_order::memory_order_seq_cst;
        return true;
    }
    return false;
}

