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
#include "vegrandis_erlang_term.h"
#include "erl_nif.h"
#include <cstring>
#include <functional>
#include <limits>
#include <type_traits>

AtomicVariable* new_erlang_atomic_variable() {
    return new ErlangAtomicVariable();
}

ErlangAtomicVariable::ErlangAtomicVariable() {
    var_.store(new ErlangTerm());
}

ErlangAtomicVariable::ErlangAtomicVariable(ERL_NIF_TERM value_term) {
    var_.store(new ErlangTerm(value_term));
}

ErlangAtomicVariable::~ErlangAtomicVariable() {
    ErlangTerm* prev = var_.exchange(nullptr);
    assert(prev != nullptr);
    delete prev;
}

ERL_NIF_TERM ErlangAtomicVariable::is_lock_free(ErlNifEnv* env) {
    return (var_.is_lock_free() ? enif_make_atom(env, "true") : enif_make_atom(env, "false"));
}

ERL_NIF_TERM ErlangAtomicVariable::store(ErlNifEnv* env, ERL_NIF_TERM value_term) {
    ErlangTerm* prev = var_.exchange(new ErlangTerm(value_term));
    assert(prev != nullptr);
    delete prev;
    return enif_make_atom(env, "ok");
}

ERL_NIF_TERM ErlangAtomicVariable::store(
        ErlNifEnv* env, ERL_NIF_TERM value_term, ERL_NIF_TERM memory_order_term)
{
    std::memory_order memory_order;
    if (not extract_memory_order(env, memory_order_term, memory_order))
        return enif_make_badarg(env);
    ErlangTerm* prev = var_.exchange(new ErlangTerm(value_term), memory_order);
    if (prev != nullptr)
        delete prev;
    return enif_make_atom(env, "ok");
}

ERL_NIF_TERM ErlangAtomicVariable::load(ErlNifEnv* env) {
    return var_.load()->Get(env);
}

ERL_NIF_TERM ErlangAtomicVariable::load(ErlNifEnv* env, ERL_NIF_TERM memory_order_term) {
    std::memory_order memory_order;
    if (not extract_memory_order(env, memory_order_term, memory_order))
        return enif_make_badarg(env);
    return var_.load(memory_order)->Get(env);
}

ERL_NIF_TERM ErlangAtomicVariable::exchange(ErlNifEnv* env, ERL_NIF_TERM desired_term) {
    ErlangTerm* prev = var_.exchange(new ErlangTerm(desired_term));
    ERL_NIF_TERM term = prev->Get(env);
    delete prev;
    return term;
}

ERL_NIF_TERM ErlangAtomicVariable::exchange(
        ErlNifEnv* env, ERL_NIF_TERM desired_term, ERL_NIF_TERM memory_order_term)
{
    std::memory_order memory_order;
    if (not extract_memory_order(env, memory_order_term, memory_order))
        return enif_make_badarg(env);
    return enif_make_badarg(env);
    ErlangTerm* prev = var_.exchange(new ErlangTerm(desired_term), memory_order);
    ERL_NIF_TERM term = prev->Get(env);
    delete prev;
    return term;
}

ERL_NIF_TERM ErlangAtomicVariable::compare_exchange_weak(
        ErlNifEnv* env, ERL_NIF_TERM /*expected_term*/, ERL_NIF_TERM /*desired_term*/)
{
    return enif_make_badarg(env);
}

ERL_NIF_TERM ErlangAtomicVariable::compare_exchange_weak(
        ErlNifEnv* env, ERL_NIF_TERM /*expected_term*/, ERL_NIF_TERM /*desired_term*/,
        ERL_NIF_TERM /*succ_memory_order_term*/, ERL_NIF_TERM /*fail_memory_order_term*/)
{
    return enif_make_badarg(env);
}

ERL_NIF_TERM ErlangAtomicVariable::compare_exchange_strong(
        ErlNifEnv* env, ERL_NIF_TERM /*expected_term*/, ERL_NIF_TERM /*desired_term*/)
{
    return enif_make_badarg(env);
}

ERL_NIF_TERM ErlangAtomicVariable::compare_exchange_strong(
        ErlNifEnv* env, ERL_NIF_TERM /*expected_term*/, ERL_NIF_TERM /*desired_term*/,
        ERL_NIF_TERM /*succ_memory_order_term*/, ERL_NIF_TERM /*fail_memory_order_term*/)
{
    return enif_make_badarg(env);
}

ERL_NIF_TERM ErlangAtomicVariable::fetch_add(ErlNifEnv* env, ERL_NIF_TERM /*arg_term*/) {
    return enif_make_badarg(env);
}

ERL_NIF_TERM ErlangAtomicVariable::fetch_add(
        ErlNifEnv* env, ERL_NIF_TERM /*arg_term*/, ERL_NIF_TERM /*memory_order_term*/)
{
    return enif_make_badarg(env);
}

ERL_NIF_TERM ErlangAtomicVariable::fetch_sub(ErlNifEnv* env, ERL_NIF_TERM /*arg_term*/) {
    return enif_make_badarg(env);
}

ERL_NIF_TERM ErlangAtomicVariable::fetch_sub(
        ErlNifEnv* env, ERL_NIF_TERM /*arg_term*/, ERL_NIF_TERM /*memory_order_term*/)
{
    return enif_make_badarg(env);
}

ERL_NIF_TERM ErlangAtomicVariable::fetch_and(ErlNifEnv* env, ERL_NIF_TERM /*arg_term*/) {
    return enif_make_badarg(env);
}

ERL_NIF_TERM ErlangAtomicVariable::fetch_and(
        ErlNifEnv* env, ERL_NIF_TERM /*arg_term*/, ERL_NIF_TERM /*memory_order_term*/)
{
    return enif_make_badarg(env);
}

ERL_NIF_TERM ErlangAtomicVariable::fetch_or(ErlNifEnv* env, ERL_NIF_TERM /*arg_term*/) {
    return enif_make_badarg(env);
}

ERL_NIF_TERM ErlangAtomicVariable::fetch_or(
        ErlNifEnv* env, ERL_NIF_TERM /*arg_term*/, ERL_NIF_TERM /*memory_order_term*/)
{
    return enif_make_badarg(env);
}

ERL_NIF_TERM ErlangAtomicVariable::fetch_xor(ErlNifEnv* env, ERL_NIF_TERM /*arg_term*/) {
    return enif_make_badarg(env);
}

ERL_NIF_TERM ErlangAtomicVariable::fetch_xor(
        ErlNifEnv* env, ERL_NIF_TERM /*arg_term*/, ERL_NIF_TERM /*memory_order_term*/)
{
    return enif_make_badarg(env);
}

ERL_NIF_TERM ErlangAtomicVariable::add_fetch(ErlNifEnv* env, ERL_NIF_TERM /*arg_term*/) {
    return enif_make_badarg(env);
}

ERL_NIF_TERM ErlangAtomicVariable::sub_fetch(ErlNifEnv* env, ERL_NIF_TERM /*arg_term*/) {
    return enif_make_badarg(env);
}

ERL_NIF_TERM ErlangAtomicVariable::and_fetch(ErlNifEnv* env, ERL_NIF_TERM /*arg_term*/) {
    return enif_make_badarg(env);
}

ERL_NIF_TERM ErlangAtomicVariable::or_fetch(ErlNifEnv* env, ERL_NIF_TERM /*arg_term*/) {
    return enif_make_badarg(env);
}

ERL_NIF_TERM ErlangAtomicVariable::xor_fetch(ErlNifEnv* env, ERL_NIF_TERM /*arg_term*/) {
    return enif_make_badarg(env);
}
