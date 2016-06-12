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

#include "vegrandis_atomic_var.h"
#include "vegrandis_common.h"
#include "erl_nif.h"
#include <cstring>
#include <functional>
#include <limits>
#include <type_traits>

AtomicVariable* new_atomic_variable(const char* type_str) {
    // dirty hack to avoid linking errors due to use of C++'s stdlib
    if (strcmp(type_str, "term") == 0)
        return new_erlang_atomic_variable();
    else
        return new_native_atomic_variable(type_str);
}
