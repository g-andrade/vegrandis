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
#include <cassert>
#include <cstring>
#include <atomic>

#define ATOMIC_FLAG_RESOURCE "atomic_flag"
#define ATOMIC_VAR_RESOURCE "atomic_var"

ErlNifResourceType* atomic_flag_resource_type;
ErlNifResourceType* atomic_var_resource_type;

static inline ERL_NIF_TERM wrap_success(ErlNifEnv* env, ERL_NIF_TERM term) {
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), term);
}

static inline ERL_NIF_TERM wrap_error(ErlNifEnv* env, ERL_NIF_TERM term) {
    return enif_make_tuple2(env, enif_make_atom(env, "error"), term);
}

static void delete_atomic_flag_resource(ErlNifEnv* /*env*/, void* resource) {
    std::atomic_flag* flag = *(static_cast<std::atomic_flag**>(resource));
    delete flag;
}

static void delete_atomic_var_resource(ErlNifEnv* /*env*/, void* resource) {
    AtomicVariable* variable = *(static_cast<AtomicVariable**>(resource));
    delete variable;
}

static int load(ErlNifEnv* env, void** /*priv*/, ERL_NIF_TERM /*load_info*/) {
    atomic_flag_resource_type = enif_open_resource_type(
            env, nullptr,
            ATOMIC_FLAG_RESOURCE,
            delete_atomic_flag_resource,
            ERL_NIF_RT_CREATE, nullptr);
    atomic_var_resource_type = enif_open_resource_type(
            env, nullptr,
            ATOMIC_VAR_RESOURCE,
            delete_atomic_var_resource,
            ERL_NIF_RT_CREATE, nullptr);
    return 0;
}

//int upgrade(ErlNifEnv* /*env*/, void** /*priv_data*/, void** /*old_priv_data*/, ERL_NIF_TERM /*load_info*/) {
//    // TODO
//    return 0;
//}

//static void unload(ErlNifEnv* /*env*/, void* /*priv_data*/) {
//    // TODO
//}

/****************************************************************/
static ERL_NIF_TERM atomic_flag_new(ErlNifEnv* env, int /*argc*/, const ERL_NIF_TERM[] /*argv*/) {
    void* resource = enif_alloc_resource(atomic_flag_resource_type, sizeof(std::atomic_flag*));
    assert(resource != nullptr);

    std::atomic_flag* flag = new std::atomic_flag();
    assert(flag != nullptr);

    flag->clear();
    memcpy(resource, &flag, sizeof(std::atomic_flag*));
    ERL_NIF_TERM term = enif_make_resource(env, resource);
    enif_release_resource(resource);
    return term;
}

static ERL_NIF_TERM atomic_flag_clear(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    void* resource = nullptr;
    if (!enif_get_resource(env, argv[0], atomic_flag_resource_type, (void **) &resource))
        return enif_make_badarg(env);
    std::atomic_flag* flag = *(static_cast<std::atomic_flag**>(resource));
    switch (argc) {
        case 1:
            flag->clear();
            return enif_make_atom(env, "ok");
        case 2:
            std::memory_order memory_order;
            if (not extract_memory_order(env, argv[1], memory_order))
                return enif_make_badarg(env);
            flag->clear(memory_order);
            return enif_make_atom(env, "ok");
        default:
            return enif_make_badarg(env);
    }
}

static ERL_NIF_TERM atomic_flag_test_and_set(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    void* resource = nullptr;
    if (!enif_get_resource(env, argv[0], atomic_flag_resource_type, (void **) &resource))
        return enif_make_badarg(env);
    std::atomic_flag* flag = *(static_cast<std::atomic_flag**>(resource));
    switch (argc) {
        case 1:
            return (flag->test_and_set() ?
                    enif_make_atom(env, "true") :
                    enif_make_atom(env, "false"));
        case 2:
            std::memory_order memory_order;
            if (not extract_memory_order(env, argv[1], memory_order))
                return enif_make_badarg(env);
            return (flag->test_and_set(memory_order) ?
                    enif_make_atom(env, "true") :
                    enif_make_atom(env, "false"));
        default:
            return enif_make_badarg(env);
    }
}

/****************************************************************/
static ERL_NIF_TERM atomic_var_new(ErlNifEnv* env, int /*argc*/, const ERL_NIF_TERM argv[]) {
    char type_str[32] = {'\0'};
    if (!enif_get_atom(env, argv[0], type_str, 32, ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }

    AtomicVariable* variable = new_atomic_variable(type_str);
    if (variable == nullptr) {
        return enif_make_badarg(env);
    }

    void* resource = enif_alloc_resource(atomic_var_resource_type, sizeof(AtomicVariable*));
    assert(resource != nullptr);

    memcpy(resource, &variable, sizeof(AtomicVariable*));
    ERL_NIF_TERM term = enif_make_resource(env, resource);
    enif_release_resource(resource);
    return term;
}

static ERL_NIF_TERM atomic_var_is_lock_free(ErlNifEnv* env, int /*argc*/, const ERL_NIF_TERM argv[]) {
    void* resource = nullptr;
    if (!enif_get_resource(env, argv[0], atomic_var_resource_type, (void **) &resource))
        return enif_make_badarg(env);
    AtomicVariable* variable = *(static_cast<AtomicVariable**>(resource));
    return variable->is_lock_free(env);
}

static ERL_NIF_TERM atomic_var_store(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    void* resource = nullptr;
    if (!enif_get_resource(env, argv[0], atomic_var_resource_type, (void **) &resource))
        return enif_make_badarg(env);
    AtomicVariable* variable = *(static_cast<AtomicVariable**>(resource));
    switch (argc) {
        case 2:  return variable->store(env, argv[1]);
        case 3:  return variable->store(env, argv[1], argv[2]);
        default: return enif_make_badarg(env);
    }
}

static ERL_NIF_TERM atomic_var_load(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    void* resource = nullptr;
    if (!enif_get_resource(env, argv[0], atomic_var_resource_type, (void **) &resource))
        return enif_make_badarg(env);
    AtomicVariable* variable = *(static_cast<AtomicVariable**>(resource));
    switch (argc) {
        case 1:  return variable->load(env);
        case 2:  return variable->load(env, argv[1]);
        default: return enif_make_badarg(env);
    }
}

static ERL_NIF_TERM atomic_var_exchange(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    void* resource = nullptr;
    if (!enif_get_resource(env, argv[0], atomic_var_resource_type, (void **) &resource))
        return enif_make_badarg(env);
    AtomicVariable* variable = *(static_cast<AtomicVariable**>(resource));
    switch (argc) {
        case 2:  return variable->exchange(env, argv[1]);
        case 3:  return variable->exchange(env, argv[1], argv[2]);
        default: return enif_make_badarg(env);
    }
}

static ERL_NIF_TERM atomic_var_compare_exchange_weak(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    void* resource = nullptr;
    if (!enif_get_resource(env, argv[0], atomic_var_resource_type, (void **) &resource))
        return enif_make_badarg(env);
    AtomicVariable* variable = *(static_cast<AtomicVariable**>(resource));
    switch (argc) {
        case 3:  return variable->compare_exchange_weak(env, argv[1], argv[2]);
        case 5:  return variable->compare_exchange_weak(env, argv[1], argv[2], argv[3], argv[4]);
        default: return enif_make_badarg(env);
    }
}

static ERL_NIF_TERM atomic_var_compare_exchange_strong(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    void* resource = nullptr;
    if (!enif_get_resource(env, argv[0], atomic_var_resource_type, (void **) &resource))
        return enif_make_badarg(env);
    AtomicVariable* variable = *(static_cast<AtomicVariable**>(resource));
    switch (argc) {
        case 3:  return variable->compare_exchange_strong(env, argv[1], argv[2]);
        case 5:  return variable->compare_exchange_strong(env, argv[1], argv[2], argv[3], argv[4]);
        default: return enif_make_badarg(env);
    }
}

static ERL_NIF_TERM atomic_var_fetch_add(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    void* resource = nullptr;
    if (!enif_get_resource(env, argv[0], atomic_var_resource_type, (void **) &resource))
        return enif_make_badarg(env);
    AtomicVariable* variable = *(static_cast<AtomicVariable**>(resource));
    switch (argc) {
        case 2:  return variable->fetch_add(env, argv[1]);
        case 3:  return variable->fetch_add(env, argv[1], argv[2]);
        default: return enif_make_badarg(env);
    }
}

static ERL_NIF_TERM atomic_var_fetch_sub(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    void* resource = nullptr;
    if (!enif_get_resource(env, argv[0], atomic_var_resource_type, (void **) &resource))
        return enif_make_badarg(env);
    AtomicVariable* variable = *(static_cast<AtomicVariable**>(resource));
    switch (argc) {
        case 2:  return variable->fetch_sub(env, argv[1]);
        case 3:  return variable->fetch_sub(env, argv[1], argv[2]);
        default: return enif_make_badarg(env);
    }
}

static ERL_NIF_TERM atomic_var_fetch_and(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    void* resource = nullptr;
    if (!enif_get_resource(env, argv[0], atomic_var_resource_type, (void **) &resource))
        return enif_make_badarg(env);
    AtomicVariable* variable = *(static_cast<AtomicVariable**>(resource));
    switch (argc) {
        case 2:  return variable->fetch_and(env, argv[1]);
        case 3:  return variable->fetch_and(env, argv[1], argv[2]);
        default: return enif_make_badarg(env);
    }
}

static ERL_NIF_TERM atomic_var_fetch_or(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    void* resource = nullptr;
    if (!enif_get_resource(env, argv[0], atomic_var_resource_type, (void **) &resource))
        return enif_make_badarg(env);
    AtomicVariable* variable = *(static_cast<AtomicVariable**>(resource));
    switch (argc) {
        case 2:  return variable->fetch_or(env, argv[1]);
        case 3:  return variable->fetch_or(env, argv[1], argv[2]);
        default: return enif_make_badarg(env);
    }
}

static ERL_NIF_TERM atomic_var_fetch_xor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    void* resource = nullptr;
    if (!enif_get_resource(env, argv[0], atomic_var_resource_type, (void **) &resource))
        return enif_make_badarg(env);
    AtomicVariable* variable = *(static_cast<AtomicVariable**>(resource));
    switch (argc) {
        case 2:  return variable->fetch_xor(env, argv[1]);
        case 3:  return variable->fetch_xor(env, argv[1], argv[2]);
        default: return enif_make_badarg(env);
    }
}

static ERL_NIF_TERM atomic_var_add_fetch(ErlNifEnv* env, int /*argc*/, const ERL_NIF_TERM argv[]) {
    void* resource = nullptr;
    if (!enif_get_resource(env, argv[0], atomic_var_resource_type, (void **) &resource))
        return enif_make_badarg(env);
    AtomicVariable* variable = *(static_cast<AtomicVariable**>(resource));
    return variable->add_fetch(env, argv[1]);
}

static ERL_NIF_TERM atomic_var_sub_fetch(ErlNifEnv* env, int /*argc*/, const ERL_NIF_TERM argv[]) {
    void* resource = nullptr;
    if (!enif_get_resource(env, argv[0], atomic_var_resource_type, (void **) &resource))
        return enif_make_badarg(env);
    AtomicVariable* variable = *(static_cast<AtomicVariable**>(resource));
    return variable->sub_fetch(env, argv[1]);
}

static ERL_NIF_TERM atomic_var_and_fetch(ErlNifEnv* env, int /*argc*/, const ERL_NIF_TERM argv[]) {
    void* resource = nullptr;
    if (!enif_get_resource(env, argv[0], atomic_var_resource_type, (void **) &resource))
        return enif_make_badarg(env);
    AtomicVariable* variable = *(static_cast<AtomicVariable**>(resource));
    return variable->and_fetch(env, argv[1]);
}

static ERL_NIF_TERM atomic_var_or_fetch(ErlNifEnv* env, int /*argc*/, const ERL_NIF_TERM argv[]) {
    void* resource = nullptr;
    if (!enif_get_resource(env, argv[0], atomic_var_resource_type, (void **) &resource))
        return enif_make_badarg(env);
    AtomicVariable* variable = *(static_cast<AtomicVariable**>(resource));
    return variable->or_fetch(env, argv[1]);
}

static ERL_NIF_TERM atomic_var_xor_fetch(ErlNifEnv* env, int /*argc*/, const ERL_NIF_TERM argv[]) {
    void* resource = nullptr;
    if (!enif_get_resource(env, argv[0], atomic_var_resource_type, (void **) &resource))
        return enif_make_badarg(env);
    AtomicVariable* variable = *(static_cast<AtomicVariable**>(resource));
    return variable->xor_fetch(env, argv[1]);
}

static ErlNifFunc nif_funcs[] = {
    {"atomic_flag_new", 0, atomic_flag_new, 0},
    {"atomic_flag_clear", 1, atomic_flag_clear, 0},
    {"atomic_flag_clear", 2, atomic_flag_clear, 0},
    {"atomic_flag_test_and_set", 1, atomic_flag_test_and_set, 0},
    {"atomic_flag_test_and_set", 2, atomic_flag_test_and_set, 0},
    {"atomic_var_new", 1, atomic_var_new, 0},
    {"atomic_var_is_lock_free", 1, atomic_var_is_lock_free, 0},
    {"atomic_var_store", 2, atomic_var_store, 0},
    {"atomic_var_store", 3, atomic_var_store, 0},
    {"atomic_var_load", 1, atomic_var_load, 0},
    {"atomic_var_load", 2, atomic_var_load, 0},
    {"atomic_var_exchange", 2, atomic_var_exchange, 0},
    {"atomic_var_exchange", 3, atomic_var_exchange, 0},
    {"atomic_var_compare_exchange_weak", 3, atomic_var_compare_exchange_weak, 0},
    {"atomic_var_compare_exchange_weak", 5, atomic_var_compare_exchange_weak, 0},
    {"atomic_var_compare_exchange_strong", 3, atomic_var_compare_exchange_strong, 0},
    {"atomic_var_compare_exchange_strong", 5, atomic_var_compare_exchange_strong, 0},
    {"atomic_var_fetch_add", 2, atomic_var_fetch_add, 0},
    {"atomic_var_fetch_add", 3, atomic_var_fetch_add, 0},
    {"atomic_var_fetch_sub", 2, atomic_var_fetch_sub, 0},
    {"atomic_var_fetch_sub", 3, atomic_var_fetch_sub, 0},
    {"atomic_var_fetch_and", 2, atomic_var_fetch_and, 0},
    {"atomic_var_fetch_and", 3, atomic_var_fetch_and, 0},
    {"atomic_var_fetch_or", 2, atomic_var_fetch_or, 0},
    {"atomic_var_fetch_or", 3, atomic_var_fetch_or, 0},
    {"atomic_var_fetch_xor", 2, atomic_var_fetch_xor, 0},
    {"atomic_var_fetch_xor", 3, atomic_var_fetch_xor, 0},
    {"atomic_var_add_fetch", 2, atomic_var_add_fetch, 0},
    {"atomic_var_sub_fetch", 2, atomic_var_sub_fetch, 0},
    {"atomic_var_and_fetch", 2, atomic_var_and_fetch, 0},
    {"atomic_var_or_fetch", 2, atomic_var_or_fetch, 0},
    {"atomic_var_xor_fetch", 2, atomic_var_xor_fetch, 0}
};

//ERL_NIF_INIT(vegrandis_nif, nif_funcs, load, nullptr, upgrade, unload)
ERL_NIF_INIT(vegrandis_nif, nif_funcs, load, nullptr, nullptr, nullptr)
