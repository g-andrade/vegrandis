#pragma once

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

#include "erl_nif.h"
#include <cassert>

class ErlangTerm {
public:
    inline ErlangTerm() noexcept {
        env_ = enif_alloc_env();
        assert(env_ != nullptr);
        hasOwnEnv_ = true;
        term_ = enif_make_atom(env_, "undefined");
    }

    inline ErlangTerm(ErlNifEnv* env, const ErlangTerm& other) noexcept {
        env_ = env;
        term_ = other.Get();
    }

    inline ErlangTerm(const ErlangTerm& other) noexcept {
        env_ = enif_alloc_env();
        assert(env_ != nullptr);
        hasOwnEnv_ = true;
        term_ = enif_make_copy(env_, other.Get());
    }

    inline ErlangTerm(ErlNifEnv* env, ERL_NIF_TERM term) noexcept {
        env_ = env;
        term_ = term;
    }

    inline ErlangTerm(ERL_NIF_TERM term) noexcept {
        env_ = enif_alloc_env();
        assert(env_ != nullptr);
        hasOwnEnv_ = true;
        term_ = enif_make_copy(env_, term);
    }

    ~ErlangTerm() {
        if (hasOwnEnv_)
            enif_free_env(env_);
    }

    inline ERL_NIF_TERM Get(ErlNifEnv* targetEnv) const {
        return enif_make_copy(targetEnv, term_);
    }

    inline bool Compare(const ErlangTerm& other) const {
        return (enif_compare(term_, other.Get()) < 0);
    }

    inline bool Match(const ErlangTerm& other) const {
        return enif_is_identical(term_, other.Get());
    }

private:
    inline ERL_NIF_TERM Get() const {
        return term_;
    }

    ErlNifEnv* env_ = nullptr;
    bool hasOwnEnv_ = false;
    ERL_NIF_TERM term_;
};
