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
#include <cstring>
#include <functional>
#include <limits>
#include <type_traits>

template<typename ExtractionType, typename ReturnType>
static inline bool extract_number(
        ErlNifEnv* env, ERL_NIF_TERM term,
        std::function<int(ErlNifEnv*, ERL_NIF_TERM, ExtractionType*)> extractor,
        ReturnType* out);

template<typename ReturnType>
static inline bool extract_number(ErlNifEnv* env, ERL_NIF_TERM term, ReturnType* out);

template<typename T>
static inline ERL_NIF_TERM pack_number(ErlNifEnv* env, T number);

namespace atomic_types {
    // standard derived types for std::atomic
    constexpr const char* CHAR = "char";
    constexpr const char* SCHAR = "schar";
    constexpr const char* UCHAR = "uchar";
    constexpr const char* SHORT = "short";
    constexpr const char* USHORT = "ushort";
    constexpr const char* INT = "int";
    constexpr const char* UINT = "uint";
    constexpr const char* LONG = "long";
    constexpr const char* ULONG = "ulong";
    constexpr const char* LLONG = "llong";
    constexpr const char* ULLONG = "ullong";
    constexpr const char* CHAR16 = "char16";
    constexpr const char* CHAR32 = "char32";
    constexpr const char* WCHAR = "wchar";
    constexpr const char* INT_LEAST8 = "int_least8";
    constexpr const char* UINT_LEAST8 = "uint_least8";
    constexpr const char* INT_LEAST16 = "int_least16";
    constexpr const char* UINT_LEAST16 = "uint_least16";
    constexpr const char* INT_LEAST32 = "int_least32";
    constexpr const char* UINT_LEAST32 = "uint_least32";
    constexpr const char* INT_LEAST64 = "int_least64";
    constexpr const char* UINT_LEAST64 = "uint_least64";
    constexpr const char* INT_FAST8 = "int_fast8";
    constexpr const char* UINT_FAST8 = "uint_fast8";
    constexpr const char* INT_FAST16 = "int_fast16";
    constexpr const char* UINT_FAST16 = "uint_fast16";
    constexpr const char* INT_FAST32 = "int_fast32";
    constexpr const char* UINT_FAST32 = "uint_fast32";
    constexpr const char* INT_FAST64 = "int_fast64";
    constexpr const char* UINT_FAST64 = "uint_fast64";
    constexpr const char* INTMAX = "intmax";
    constexpr const char* UINTMAX = "uintmax";

    // useful extras
    constexpr const char* INT8 = "int8";
    constexpr const char* UINT8 = "uint8";
    constexpr const char* INT16 = "int16";
    constexpr const char* UINT16 = "uint16";
    constexpr const char* INT32 = "int32";
    constexpr const char* UINT32 = "uint32";
    constexpr const char* INT64 = "int64";
    constexpr const char* UINT64 = "uint64";
}

template<typename T>
ERL_NIF_TERM SpecializedAtomicVariable<T>::is_lock_free(ErlNifEnv* env) {
    return (var.is_lock_free() ? enif_make_atom(env, "true") : enif_make_atom(env, "false"));
}

template<typename T>
ERL_NIF_TERM SpecializedAtomicVariable<T>::store(ErlNifEnv* env, ERL_NIF_TERM value_term) {
    T value;
    if (not extract_number<T>(env, value_term, &value))
        return enif_make_badarg(env);
    var.store(value);
    return enif_make_atom(env, "ok");
}

    template<typename T>
ERL_NIF_TERM SpecializedAtomicVariable<T>::store(
        ErlNifEnv* env, ERL_NIF_TERM value_term, ERL_NIF_TERM memory_order_term)
{
    T value;
    std::memory_order memory_order;
    if (not extract_number<T>(env, value_term, &value))
        return enif_make_badarg(env);
    if (not extract_memory_order(env, memory_order_term, memory_order))
        return enif_make_badarg(env);
    var.store(value, memory_order);
    return enif_make_atom(env, "ok");
}

template<typename T>
ERL_NIF_TERM SpecializedAtomicVariable<T>::load(ErlNifEnv* env) {
    T value = var.load();
    return pack_number<T>(env, value);
}

template<typename T>
ERL_NIF_TERM SpecializedAtomicVariable<T>::load(ErlNifEnv* env, ERL_NIF_TERM memory_order_term) {
    std::memory_order memory_order;
    if (not extract_memory_order(env, memory_order_term, memory_order))
        return enif_make_badarg(env);
    T value = var.load(memory_order);
    return pack_number<T>(env, value);
}

template<typename T>
ERL_NIF_TERM SpecializedAtomicVariable<T>::exchange(ErlNifEnv* env, ERL_NIF_TERM desired_term) {
    T desired;
    if (not extract_number<T>(env, desired_term, &desired))
        return enif_make_badarg(env);
    T previous = var.exchange(desired);
    return pack_number<T>(env, previous);
}

    template<typename T>
ERL_NIF_TERM SpecializedAtomicVariable<T>::exchange(
        ErlNifEnv* env, ERL_NIF_TERM desired_term, ERL_NIF_TERM memory_order_term)
{
    T desired;
    std::memory_order memory_order;
    if (not extract_number<T>(env, desired_term, &desired))
        return enif_make_badarg(env);
    if (not extract_memory_order(env, memory_order_term, memory_order))
        return enif_make_badarg(env);
    T previous = var.exchange(desired, memory_order);
    return pack_number<T>(env, previous);
}

    template<typename T>
ERL_NIF_TERM SpecializedAtomicVariable<T>::compare_exchange_weak(
        ErlNifEnv* env, ERL_NIF_TERM expected_term, ERL_NIF_TERM desired_term)
{
    T expected, desired;
    if (not extract_number<T>(env, expected_term, &expected))
        return enif_make_badarg(env);
    if (not extract_number<T>(env, desired_term, &desired))
        return enif_make_badarg(env);

    if (var.compare_exchange_weak(expected, desired))
        return enif_make_atom(env, "true");
    else
        return enif_make_tuple2(
                env,
                enif_make_atom(env, "false"),
                pack_number<T>(env, expected));
}

    template<typename T>
ERL_NIF_TERM SpecializedAtomicVariable<T>::compare_exchange_weak(
        ErlNifEnv* env, ERL_NIF_TERM expected_term, ERL_NIF_TERM desired_term,
        ERL_NIF_TERM succ_memory_order_term, ERL_NIF_TERM fail_memory_order_term)
{
    T expected, desired;
    std::memory_order succ_memory_order, fail_memory_order;
    if (not extract_number<T>(env, expected_term, &expected))
        return enif_make_badarg(env);
    if (not extract_number<T>(env, desired_term, &desired))
        return enif_make_badarg(env);
    if (not extract_memory_order(env, succ_memory_order_term, succ_memory_order))
        return enif_make_badarg(env);
    if (not extract_memory_order(env, fail_memory_order_term, fail_memory_order))
        return enif_make_badarg(env);

    if (var.compare_exchange_weak(expected, desired, succ_memory_order, fail_memory_order))
        return enif_make_atom(env, "true");
    else
        return enif_make_tuple2(
                env,
                enif_make_atom(env, "false"),
                pack_number<T>(env, expected));
}

    template<typename T>
ERL_NIF_TERM SpecializedAtomicVariable<T>::compare_exchange_strong(
        ErlNifEnv* env, ERL_NIF_TERM expected_term, ERL_NIF_TERM desired_term)
{
    T expected, desired;
    if (not extract_number<T>(env, expected_term, &expected))
        return enif_make_badarg(env);
    if (not extract_number<T>(env, desired_term, &desired))
        return enif_make_badarg(env);

    if (var.compare_exchange_strong(expected, desired))
        return enif_make_atom(env, "true");
    else
        return enif_make_tuple2(
                env,
                enif_make_atom(env, "false"),
                pack_number<T>(env, expected));
}

    template<typename T>
ERL_NIF_TERM SpecializedAtomicVariable<T>::compare_exchange_strong(
        ErlNifEnv* env, ERL_NIF_TERM expected_term, ERL_NIF_TERM desired_term,
        ERL_NIF_TERM succ_memory_order_term, ERL_NIF_TERM fail_memory_order_term)
{
    T expected, desired;
    std::memory_order succ_memory_order, fail_memory_order;
    if (not extract_number<T>(env, expected_term, &expected))
        return enif_make_badarg(env);
    if (not extract_number<T>(env, desired_term, &desired))
        return enif_make_badarg(env);
    if (not extract_memory_order(env, succ_memory_order_term, succ_memory_order))
        return enif_make_badarg(env);
    if (not extract_memory_order(env, fail_memory_order_term, fail_memory_order))
        return enif_make_badarg(env);

    if (var.compare_exchange_strong(expected, desired, succ_memory_order, fail_memory_order))
        return enif_make_atom(env, "true");
    else
        return enif_make_tuple2(
                env,
                enif_make_atom(env, "false"),
                pack_number<T>(env, expected));
}

template<typename T>
ERL_NIF_TERM SpecializedAtomicVariable<T>::fetch_add(ErlNifEnv* env, ERL_NIF_TERM arg_term) {
    T arg;
    if (not extract_number<T>(env, arg_term, &arg))
        return enif_make_badarg(env);
    return pack_number<T>(env, var.fetch_add(arg));
}

    template<typename T>
ERL_NIF_TERM SpecializedAtomicVariable<T>::fetch_add(
        ErlNifEnv* env, ERL_NIF_TERM arg_term, ERL_NIF_TERM memory_order_term)
{
    T arg;
    std::memory_order memory_order;
    if (not extract_number<T>(env, arg_term, &arg))
        return enif_make_badarg(env);
    if (not extract_memory_order(env, memory_order_term, memory_order))
        return enif_make_badarg(env);
    return pack_number<T>(env, var.fetch_add(arg, memory_order));
}

template<typename T>
ERL_NIF_TERM SpecializedAtomicVariable<T>::fetch_sub(ErlNifEnv* env, ERL_NIF_TERM arg_term) {
    T arg;
    if (not extract_number<T>(env, arg_term, &arg))
        return enif_make_badarg(env);
    return pack_number<T>(env, var.fetch_sub(arg));
}

    template<typename T>
ERL_NIF_TERM SpecializedAtomicVariable<T>::fetch_sub(
        ErlNifEnv* env, ERL_NIF_TERM arg_term, ERL_NIF_TERM memory_order_term)
{
    T arg;
    std::memory_order memory_order;
    if (not extract_number<T>(env, arg_term, &arg))
        return enif_make_badarg(env);
    if (not extract_memory_order(env, memory_order_term, memory_order))
        return enif_make_badarg(env);
    return pack_number<T>(env, var.fetch_sub(arg, memory_order));
}

template<typename T>
ERL_NIF_TERM SpecializedAtomicVariable<T>::fetch_and(ErlNifEnv* env, ERL_NIF_TERM arg_term) {
    T arg;
    if (not extract_number<T>(env, arg_term, &arg))
        return enif_make_badarg(env);
    return pack_number<T>(env, var.fetch_and(arg));
}

    template<typename T>
ERL_NIF_TERM SpecializedAtomicVariable<T>::fetch_and(
        ErlNifEnv* env, ERL_NIF_TERM arg_term, ERL_NIF_TERM memory_order_term)
{
    T arg;
    std::memory_order memory_order;
    if (not extract_number<T>(env, arg_term, &arg))
        return enif_make_badarg(env);
    if (not extract_memory_order(env, memory_order_term, memory_order))
        return enif_make_badarg(env);
    return pack_number<T>(env, var.fetch_and(arg, memory_order));
}

template<typename T>
ERL_NIF_TERM SpecializedAtomicVariable<T>::fetch_or(ErlNifEnv* env, ERL_NIF_TERM arg_term) {
    T arg;
    if (not extract_number<T>(env, arg_term, &arg))
        return enif_make_badarg(env);
    return pack_number<T>(env, var.fetch_or(arg));
}

    template<typename T>
ERL_NIF_TERM SpecializedAtomicVariable<T>::fetch_or(
        ErlNifEnv* env, ERL_NIF_TERM arg_term, ERL_NIF_TERM memory_order_term)
{
    T arg;
    std::memory_order memory_order;
    if (not extract_number<T>(env, arg_term, &arg))
        return enif_make_badarg(env);
    if (not extract_memory_order(env, memory_order_term, memory_order))
        return enif_make_badarg(env);
    return pack_number<T>(env, var.fetch_or(arg, memory_order));
}

template<typename T>
ERL_NIF_TERM SpecializedAtomicVariable<T>::fetch_xor(ErlNifEnv* env, ERL_NIF_TERM arg_term) {
    T arg;
    if (not extract_number<T>(env, arg_term, &arg))
        return enif_make_badarg(env);
    return pack_number<T>(env, var.fetch_xor(arg));
}

    template<typename T>
ERL_NIF_TERM SpecializedAtomicVariable<T>::fetch_xor(
        ErlNifEnv* env, ERL_NIF_TERM arg_term, ERL_NIF_TERM memory_order_term)
{
    T arg;
    std::memory_order memory_order;
    if (not extract_number<T>(env, arg_term, &arg))
        return enif_make_badarg(env);
    if (not extract_memory_order(env, memory_order_term, memory_order))
        return enif_make_badarg(env);
    return pack_number<T>(env, var.fetch_xor(arg, memory_order));
}

template<typename T>
ERL_NIF_TERM SpecializedAtomicVariable<T>::add_fetch(ErlNifEnv* env, ERL_NIF_TERM arg_term) {
    T arg;
    if (not extract_number<T>(env, arg_term, &arg))
        return enif_make_badarg(env);
    return pack_number<T>(env, var += arg);
}

template<typename T>
ERL_NIF_TERM SpecializedAtomicVariable<T>::sub_fetch(ErlNifEnv* env, ERL_NIF_TERM arg_term) {
    T arg;
    if (not extract_number<T>(env, arg_term, &arg))
        return enif_make_badarg(env);
    return pack_number<T>(env, var -= arg);
}

template<typename T>
ERL_NIF_TERM SpecializedAtomicVariable<T>::and_fetch(ErlNifEnv* env, ERL_NIF_TERM arg_term) {
    T arg;
    if (not extract_number<T>(env, arg_term, &arg))
        return enif_make_badarg(env);
    return pack_number<T>(env, var &= arg);
}

template<typename T>
ERL_NIF_TERM SpecializedAtomicVariable<T>::or_fetch(ErlNifEnv* env, ERL_NIF_TERM arg_term) {
    T arg;
    if (not extract_number<T>(env, arg_term, &arg))
        return enif_make_badarg(env);
    return pack_number<T>(env, var |= arg);
}

template<typename T>
ERL_NIF_TERM SpecializedAtomicVariable<T>::xor_fetch(ErlNifEnv* env, ERL_NIF_TERM arg_term) {
    T arg;
    if (not extract_number<T>(env, arg_term, &arg))
        return enif_make_badarg(env);
    return pack_number<T>(env, var ^= arg);
}

/**************************************************
 * What follows will make you want to cringe
 */

AtomicVariable* new_atomic_variable(const char* type_str) {
    // standard derived types for std::atomic
    if (strcmp(type_str, atomic_types::CHAR) == 0)
        return new SpecializedAtomicVariable<char>();
    if (strcmp(type_str, atomic_types::SCHAR) == 0)
        return new SpecializedAtomicVariable<signed char>();
    if (strcmp(type_str, atomic_types::UCHAR) == 0)
        return new SpecializedAtomicVariable<unsigned char>();
    if (strcmp(type_str, atomic_types::SHORT) == 0)
        return new SpecializedAtomicVariable<short>();
    if (strcmp(type_str, atomic_types::USHORT) == 0)
        return new SpecializedAtomicVariable<unsigned short>();
    if (strcmp(type_str, atomic_types::INT) == 0)
        return new SpecializedAtomicVariable<int>();
    if (strcmp(type_str, atomic_types::UINT) == 0)
        return new SpecializedAtomicVariable<unsigned int>();
    if (strcmp(type_str, atomic_types::LONG) == 0)
        return new SpecializedAtomicVariable<long>();
    if (strcmp(type_str, atomic_types::ULONG) == 0)
        return new SpecializedAtomicVariable<unsigned long>();
    if (strcmp(type_str, atomic_types::LLONG) == 0)
        return new SpecializedAtomicVariable<long long>();
    if (strcmp(type_str, atomic_types::ULLONG) == 0)
        return new SpecializedAtomicVariable<unsigned long long>();
    if (strcmp(type_str, atomic_types::CHAR16) == 0)
        return new SpecializedAtomicVariable<char16_t>();
    if (strcmp(type_str, atomic_types::CHAR32) == 0)
        return new SpecializedAtomicVariable<char32_t>();
    if (strcmp(type_str, atomic_types::WCHAR) == 0)
        return new SpecializedAtomicVariable<wchar_t>();
    if (strcmp(type_str, atomic_types::INT_LEAST8) == 0)
        return new SpecializedAtomicVariable<int_least8_t>();
    if (strcmp(type_str, atomic_types::UINT_LEAST8) == 0)
        return new SpecializedAtomicVariable<uint_least8_t>();
    if (strcmp(type_str, atomic_types::INT_LEAST16) == 0)
        return new SpecializedAtomicVariable<int_least16_t>();
    if (strcmp(type_str, atomic_types::UINT_LEAST16) == 0)
        return new SpecializedAtomicVariable<uint_least16_t>();
    if (strcmp(type_str, atomic_types::INT_LEAST32) == 0)
        return new SpecializedAtomicVariable<int_least32_t>();
    if (strcmp(type_str, atomic_types::UINT_LEAST32) == 0)
        return new SpecializedAtomicVariable<uint_least32_t>();
    if (strcmp(type_str, atomic_types::INT_LEAST64) == 0)
        return new SpecializedAtomicVariable<int_least64_t>();
    if (strcmp(type_str, atomic_types::UINT_LEAST64) == 0)
        return new SpecializedAtomicVariable<uint_least64_t>();
    if (strcmp(type_str, atomic_types::INT_FAST8) == 0)
        return new SpecializedAtomicVariable<int_fast8_t>();
    if (strcmp(type_str, atomic_types::UINT_FAST8) == 0)
        return new SpecializedAtomicVariable<uint_fast8_t>();
    if (strcmp(type_str, atomic_types::INT_FAST16) == 0)
        return new SpecializedAtomicVariable<int_fast16_t>();
    if (strcmp(type_str, atomic_types::UINT_FAST16) == 0)
        return new SpecializedAtomicVariable<uint_fast16_t>();
    if (strcmp(type_str, atomic_types::INT_FAST32) == 0)
        return new SpecializedAtomicVariable<int_fast32_t>();
    if (strcmp(type_str, atomic_types::UINT_FAST32) == 0)
        return new SpecializedAtomicVariable<uint_fast32_t>();
    if (strcmp(type_str, atomic_types::INT_FAST64) == 0)
        return new SpecializedAtomicVariable<int_fast64_t>();
    if (strcmp(type_str, atomic_types::UINT_FAST64) == 0)
        return new SpecializedAtomicVariable<uint_fast64_t>();
    if (strcmp(type_str, atomic_types::INTMAX) == 0)
        return new SpecializedAtomicVariable<intmax_t>();
    if (strcmp(type_str, atomic_types::UINTMAX) == 0)
        return new SpecializedAtomicVariable<uintmax_t>();

    // useful extras
    if (strcmp(type_str, atomic_types::INT8) == 0)
        return new SpecializedAtomicVariable<int8_t>();
    if (strcmp(type_str, atomic_types::UINT8) == 0)
        return new SpecializedAtomicVariable<uint8_t>();
    if (strcmp(type_str, atomic_types::INT16) == 0)
        return new SpecializedAtomicVariable<int16_t>();
    if (strcmp(type_str, atomic_types::UINT16) == 0)
        return new SpecializedAtomicVariable<uint16_t>();
    if (strcmp(type_str, atomic_types::INT32) == 0)
        return new SpecializedAtomicVariable<int32_t>();
    if (strcmp(type_str, atomic_types::UINT32) == 0)
        return new SpecializedAtomicVariable<uint32_t>();
    if (strcmp(type_str, atomic_types::INT64) == 0)
        return new SpecializedAtomicVariable<int64_t>();
    if (strcmp(type_str, atomic_types::UINT64) == 0)
        return new SpecializedAtomicVariable<uint64_t>();
    return nullptr;
}

/*** parsing arguments
 ***/
template<typename ExtractionType, typename ReturnType>
static inline bool extract_number(
        ErlNifEnv* env, ERL_NIF_TERM term,
        std::function<int(ErlNifEnv*, ERL_NIF_TERM, ExtractionType*)> extractor,
        typename std::enable_if<sizeof(ExtractionType) == sizeof(ReturnType), ReturnType>::type *out)
{
    return extractor(env, term, reinterpret_cast<ExtractionType*>(out));
}

template<typename ExtractionType, typename ReturnType>
static inline bool extract_number(
        ErlNifEnv* env, ERL_NIF_TERM term,
        std::function<int(ErlNifEnv*, ERL_NIF_TERM, ExtractionType*)> extractor,
        ReturnType* out)
{
    ExtractionType number = 0;
    if (not extractor(env, term, &number))
        return false;
    if ((number > std::numeric_limits<ReturnType>::max()) or (number < std::numeric_limits<ReturnType>::min()))
        return false;
    *out = static_cast<ReturnType>(number);
    return true;
}


template<typename T>
static inline bool extract_number(
        ErlNifEnv* env, ERL_NIF_TERM term,
        typename std::enable_if<
        std::is_integral<T>::value and std::is_signed<T>::value and (sizeof(T) <= sizeof(int)),
        T
        >::type* out)
{
    return extract_number<int, T>(env, term, enif_get_int, out);
}

template<typename T>
static inline bool extract_number(
        ErlNifEnv* env, ERL_NIF_TERM term,
        typename std::enable_if<
        std::is_integral<T>::value and std::is_unsigned<T>::value and (sizeof(T) <= sizeof(unsigned int)),
        T
        >::type* out)
{
    return extract_number<unsigned int, T>(env, term, enif_get_uint, out);
}

template<typename T>
static inline bool extract_number(
        ErlNifEnv* env, ERL_NIF_TERM term,
        typename std::enable_if<
        std::is_integral<T>::value and std::is_signed<T>::value and
        (sizeof(T) > sizeof(int)) and (sizeof(T) <= sizeof(ErlNifSInt64)),
        T
        >::type* out)
{
    return extract_number<int64_t, T>(env, term, enif_get_int64, out);
}

template<typename T>
static inline bool extract_number(
        ErlNifEnv* env, ERL_NIF_TERM term,
        typename std::enable_if<
        std::is_integral<T>::value and std::is_unsigned<T>::value and
        (sizeof(T) > sizeof(unsigned int)) and (sizeof(T) <= sizeof(ErlNifUInt64)),
        T
        >::type* out)
{
    return extract_number<uint64_t, T>(env, term, enif_get_uint64, out);
}


/*** returning values
 ***/
template<typename T>
static inline ERL_NIF_TERM pack_number(
        ErlNifEnv* env,
        typename std::enable_if<
        std::is_integral<T>::value and std::is_signed<T>::value and (sizeof(T) <= sizeof(int)),
        T
        >::type number)
{
    return enif_make_int(env, static_cast<int>(number));
}

template<typename T>
static inline ERL_NIF_TERM pack_number(
        ErlNifEnv* env,
        typename std::enable_if<
        std::is_integral<T>::value and std::is_unsigned<T>::value and (sizeof(T) <= sizeof(unsigned int)),
        T
        >::type number)
{
    return enif_make_uint(env, static_cast<unsigned int>(number));
}

template<typename T>
static inline ERL_NIF_TERM pack_number(
        ErlNifEnv* env,
        typename std::enable_if<
        std::is_integral<T>::value and std::is_signed<T>::value and
        (sizeof(T) > sizeof(int)) && (sizeof(T) <= sizeof(int64_t)),
        T
        >::type number)
{
    return enif_make_int64(env, static_cast<int64_t>(number));
}

template<typename T>
static inline ERL_NIF_TERM pack_number(
        ErlNifEnv* env,
        typename std::enable_if<
        std::is_integral<T>::value and std::is_unsigned<T>::value and
        (sizeof(T) > sizeof(unsigned int)) && (sizeof(T) <= sizeof(uint64_t)),
        T
        >::type number)
{
    return enif_make_int64(env, static_cast<uint64_t>(number));
}
