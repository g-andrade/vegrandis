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

#include "vegrandis_erlang_term.h"
#include "erl_nif.h"
#include <atomic>

class AtomicVariable {
    public:
        virtual ~AtomicVariable() {}

        virtual ERL_NIF_TERM is_lock_free(ErlNifEnv* env) = 0;

        virtual ERL_NIF_TERM store(ErlNifEnv* env, ERL_NIF_TERM value_term) = 0;
        virtual ERL_NIF_TERM store(
                ErlNifEnv* env, ERL_NIF_TERM value_term, ERL_NIF_TERM memory_order_term
                ) = 0;

        virtual ERL_NIF_TERM load(ErlNifEnv* env) = 0;
        virtual ERL_NIF_TERM load(ErlNifEnv* env, ERL_NIF_TERM memory_order_term) = 0;

        virtual ERL_NIF_TERM exchange(ErlNifEnv* env, ERL_NIF_TERM desired_term) = 0;
        virtual ERL_NIF_TERM exchange(
                ErlNifEnv* env, ERL_NIF_TERM desired_term, ERL_NIF_TERM memory_order_term
                ) = 0;

        virtual ERL_NIF_TERM compare_exchange_weak(
                ErlNifEnv* env, ERL_NIF_TERM expected_term, ERL_NIF_TERM desired_term) = 0;
        virtual ERL_NIF_TERM compare_exchange_weak(
                ErlNifEnv* env, ERL_NIF_TERM expected_term, ERL_NIF_TERM desired_term,
                ERL_NIF_TERM succ_memory_order_term, ERL_NIF_TERM fail_memory_order_term) = 0;

        virtual ERL_NIF_TERM compare_exchange_strong(
                ErlNifEnv* env, ERL_NIF_TERM expected_term, ERL_NIF_TERM desired_term) = 0;
        virtual ERL_NIF_TERM compare_exchange_strong(
                ErlNifEnv* env, ERL_NIF_TERM expected_term, ERL_NIF_TERM desired_term,
                ERL_NIF_TERM succ_memory_order_term, ERL_NIF_TERM fail_memory_order_term) = 0;

        virtual ERL_NIF_TERM fetch_add(ErlNifEnv* env, ERL_NIF_TERM arg_term) = 0;
        virtual ERL_NIF_TERM fetch_add(
                ErlNifEnv* env, ERL_NIF_TERM arg_term, ERL_NIF_TERM memory_order_term) = 0;

        virtual ERL_NIF_TERM fetch_sub(ErlNifEnv* env, ERL_NIF_TERM arg_term) = 0;
        virtual ERL_NIF_TERM fetch_sub(
                ErlNifEnv* env, ERL_NIF_TERM arg_term, ERL_NIF_TERM memory_order_term) = 0;

        virtual ERL_NIF_TERM fetch_and(ErlNifEnv* env, ERL_NIF_TERM arg_term) = 0;
        virtual ERL_NIF_TERM fetch_and(
                ErlNifEnv* env, ERL_NIF_TERM arg_term, ERL_NIF_TERM memory_order_term) = 0;

        virtual ERL_NIF_TERM fetch_or(ErlNifEnv* env, ERL_NIF_TERM arg_term) = 0;
        virtual ERL_NIF_TERM fetch_or(
                ErlNifEnv* env, ERL_NIF_TERM arg_term, ERL_NIF_TERM memory_order_term) = 0;

        virtual ERL_NIF_TERM fetch_xor(ErlNifEnv* env, ERL_NIF_TERM arg_term) = 0;
        virtual ERL_NIF_TERM fetch_xor(
                ErlNifEnv* env, ERL_NIF_TERM arg_term, ERL_NIF_TERM memory_order_term) = 0;

        virtual ERL_NIF_TERM add_fetch(ErlNifEnv* env, ERL_NIF_TERM arg_term)= 0;
        virtual ERL_NIF_TERM sub_fetch(ErlNifEnv* env, ERL_NIF_TERM arg_term)= 0;
        virtual ERL_NIF_TERM and_fetch(ErlNifEnv* env, ERL_NIF_TERM arg_term)= 0;
        virtual ERL_NIF_TERM or_fetch(ErlNifEnv* env, ERL_NIF_TERM arg_term)= 0;
        virtual ERL_NIF_TERM xor_fetch(ErlNifEnv* env, ERL_NIF_TERM arg_term)= 0;
};


template<typename T>
class NativeAtomicVariable : public AtomicVariable {
    private:
        std::atomic<T> var_;

    public:
        NativeAtomicVariable(): var_(0) {};
        ~NativeAtomicVariable() {};

        ERL_NIF_TERM is_lock_free(ErlNifEnv* env);

        ERL_NIF_TERM store(ErlNifEnv* env, ERL_NIF_TERM value_term);
        ERL_NIF_TERM store(
                ErlNifEnv* env, ERL_NIF_TERM value_term, ERL_NIF_TERM memory_order_term
                );

        ERL_NIF_TERM load(ErlNifEnv* env);
        ERL_NIF_TERM load(ErlNifEnv* env, ERL_NIF_TERM memory_order_term);

        ERL_NIF_TERM exchange(ErlNifEnv* env, ERL_NIF_TERM desired_term);
        ERL_NIF_TERM exchange(
                ErlNifEnv* env, ERL_NIF_TERM desired_term, ERL_NIF_TERM memory_order_term
                );

        ERL_NIF_TERM compare_exchange_weak(
                ErlNifEnv* env, ERL_NIF_TERM expected_term, ERL_NIF_TERM desired_term
                );
        ERL_NIF_TERM compare_exchange_weak(
                ErlNifEnv* env, ERL_NIF_TERM expected_term, ERL_NIF_TERM desired_term,
                ERL_NIF_TERM succ_memory_order_term, ERL_NIF_TERM fail_memory_order_term);

        ERL_NIF_TERM compare_exchange_strong(
                ErlNifEnv* env, ERL_NIF_TERM expected_term, ERL_NIF_TERM desired_term
                );
        ERL_NIF_TERM compare_exchange_strong(
                ErlNifEnv* env, ERL_NIF_TERM expected_term, ERL_NIF_TERM desired_term,
                ERL_NIF_TERM succ_memory_order_term, ERL_NIF_TERM fail_memory_order_term);

        ERL_NIF_TERM fetch_add(ErlNifEnv* env, ERL_NIF_TERM arg_term);
        ERL_NIF_TERM fetch_add(ErlNifEnv* env, ERL_NIF_TERM arg_term, ERL_NIF_TERM memory_order_term);

        ERL_NIF_TERM fetch_sub(ErlNifEnv* env, ERL_NIF_TERM arg_term);
        ERL_NIF_TERM fetch_sub(ErlNifEnv* env, ERL_NIF_TERM arg_term, ERL_NIF_TERM memory_order_term);

        ERL_NIF_TERM fetch_and(ErlNifEnv* env, ERL_NIF_TERM arg_term);
        ERL_NIF_TERM fetch_and(ErlNifEnv* env, ERL_NIF_TERM arg_term, ERL_NIF_TERM memory_order_term);

        ERL_NIF_TERM fetch_or(ErlNifEnv* env, ERL_NIF_TERM arg_term);
        ERL_NIF_TERM fetch_or(ErlNifEnv* env, ERL_NIF_TERM arg_term, ERL_NIF_TERM memory_order_term);

        ERL_NIF_TERM fetch_xor(ErlNifEnv* env, ERL_NIF_TERM arg_term);
        ERL_NIF_TERM fetch_xor(ErlNifEnv* env, ERL_NIF_TERM arg_term, ERL_NIF_TERM memory_order_term);

        ERL_NIF_TERM add_fetch(ErlNifEnv* env, ERL_NIF_TERM arg_term);
        ERL_NIF_TERM sub_fetch(ErlNifEnv* env, ERL_NIF_TERM arg_term);
        ERL_NIF_TERM and_fetch(ErlNifEnv* env, ERL_NIF_TERM arg_term);
        ERL_NIF_TERM or_fetch(ErlNifEnv* env, ERL_NIF_TERM arg_term);
        ERL_NIF_TERM xor_fetch(ErlNifEnv* env, ERL_NIF_TERM arg_term);
};

class ErlangAtomicVariable : public AtomicVariable {
    private:
        std::atomic<ErlangTerm*> var_;

    public:
        ErlangAtomicVariable();
        ErlangAtomicVariable(ERL_NIF_TERM value);
        ~ErlangAtomicVariable();

        ERL_NIF_TERM is_lock_free(ErlNifEnv* env);

        ERL_NIF_TERM store(ErlNifEnv* env, ERL_NIF_TERM value_term);
        ERL_NIF_TERM store(
                ErlNifEnv* env, ERL_NIF_TERM value_term, ERL_NIF_TERM memory_order_term
                );

        ERL_NIF_TERM load(ErlNifEnv* env);
        ERL_NIF_TERM load(ErlNifEnv* env, ERL_NIF_TERM memory_order_term);

        ERL_NIF_TERM exchange(ErlNifEnv* env, ERL_NIF_TERM desired_term);
        ERL_NIF_TERM exchange(
                ErlNifEnv* env, ERL_NIF_TERM desired_term, ERL_NIF_TERM memory_order_term
                );

        ERL_NIF_TERM compare_exchange_weak(
                ErlNifEnv* env, ERL_NIF_TERM expected_term, ERL_NIF_TERM desired_term
                );
        ERL_NIF_TERM compare_exchange_weak(
                ErlNifEnv* env, ERL_NIF_TERM expected_term, ERL_NIF_TERM desired_term,
                ERL_NIF_TERM succ_memory_order_term, ERL_NIF_TERM fail_memory_order_term);

        ERL_NIF_TERM compare_exchange_strong(
                ErlNifEnv* env, ERL_NIF_TERM expected_term, ERL_NIF_TERM desired_term
                );
        ERL_NIF_TERM compare_exchange_strong(
                ErlNifEnv* env, ERL_NIF_TERM expected_term, ERL_NIF_TERM desired_term,
                ERL_NIF_TERM succ_memory_order_term, ERL_NIF_TERM fail_memory_order_term);

        ERL_NIF_TERM fetch_add(ErlNifEnv* env, ERL_NIF_TERM arg_term);
        ERL_NIF_TERM fetch_add(ErlNifEnv* env, ERL_NIF_TERM arg_term, ERL_NIF_TERM memory_order_term);

        ERL_NIF_TERM fetch_sub(ErlNifEnv* env, ERL_NIF_TERM arg_term);
        ERL_NIF_TERM fetch_sub(ErlNifEnv* env, ERL_NIF_TERM arg_term, ERL_NIF_TERM memory_order_term);

        ERL_NIF_TERM fetch_and(ErlNifEnv* env, ERL_NIF_TERM arg_term);
        ERL_NIF_TERM fetch_and(ErlNifEnv* env, ERL_NIF_TERM arg_term, ERL_NIF_TERM memory_order_term);

        ERL_NIF_TERM fetch_or(ErlNifEnv* env, ERL_NIF_TERM arg_term);
        ERL_NIF_TERM fetch_or(ErlNifEnv* env, ERL_NIF_TERM arg_term, ERL_NIF_TERM memory_order_term);

        ERL_NIF_TERM fetch_xor(ErlNifEnv* env, ERL_NIF_TERM arg_term);
        ERL_NIF_TERM fetch_xor(ErlNifEnv* env, ERL_NIF_TERM arg_term, ERL_NIF_TERM memory_order_term);

        ERL_NIF_TERM add_fetch(ErlNifEnv* env, ERL_NIF_TERM arg_term);
        ERL_NIF_TERM sub_fetch(ErlNifEnv* env, ERL_NIF_TERM arg_term);
        ERL_NIF_TERM and_fetch(ErlNifEnv* env, ERL_NIF_TERM arg_term);
        ERL_NIF_TERM or_fetch(ErlNifEnv* env, ERL_NIF_TERM arg_term);
        ERL_NIF_TERM xor_fetch(ErlNifEnv* env, ERL_NIF_TERM arg_term);
};

extern AtomicVariable* new_atomic_variable(const char* type_str);
extern AtomicVariable* new_native_atomic_variable(const char* type_str);
extern AtomicVariable* new_erlang_atomic_variable();
