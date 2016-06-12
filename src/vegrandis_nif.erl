%% @hidden
-module(vegrandis_nif).
-author('Guilherme Andrade <vegrandis(at)gandrade(dot)net>').
-on_load(init/0).

-ignore_xref({init, 0}).

-export([atomic_flag_new/0]).
-export([atomic_flag_clear/1]).
-export([atomic_flag_clear/2]).
-export([atomic_flag_test_and_set/1]).
-export([atomic_flag_test_and_set/2]).
-export([atomic_var_new/1]).
-export([atomic_var_is_lock_free/1]).
-export([atomic_var_store/2]).
-export([atomic_var_store/3]).
-export([atomic_var_load/1]).
-export([atomic_var_load/2]).
-export([atomic_var_exchange/2]).
-export([atomic_var_exchange/3]).
-export([atomic_var_compare_exchange_weak/3]).
-export([atomic_var_compare_exchange_weak/5]).
-export([atomic_var_compare_exchange_strong/3]).
-export([atomic_var_compare_exchange_strong/5]).
-export([atomic_var_fetch_add/2]).
-export([atomic_var_fetch_add/3]).
-export([atomic_var_fetch_sub/2]).
-export([atomic_var_fetch_sub/3]).
-export([atomic_var_fetch_and/2]).
-export([atomic_var_fetch_and/3]).
-export([atomic_var_fetch_or/2]).
-export([atomic_var_fetch_or/3]).
-export([atomic_var_fetch_xor/2]).
-export([atomic_var_fetch_xor/3]).
-export([atomic_var_add_fetch/2]).
-export([atomic_var_sub_fetch/2]).
-export([atomic_var_and_fetch/2]).
-export([atomic_var_or_fetch/2]).
-export([atomic_var_xor_fetch/2]).


init() ->
    ok = erlang:load_nif("./priv/vegrandis_nif", 0).


-spec atomic_flag_new() -> {ok, vegrandis_flag:atomic_flag()} | {error, out_of_memory}.
atomic_flag_new() ->
    erlang:nif_error(nif_library_not_loaded).

-spec atomic_flag_clear(_Flag :: vegrandis_flag:atomic_flag()) -> ok.
atomic_flag_clear(_Flag) ->
    erlang:nif_error(nif_library_not_loaded).

-spec atomic_flag_clear(_Flag :: vegrandis_flag:atomic_flag(),
                        _MemoryOrder :: vegrandis_flag:memory_order()) -> ok.
atomic_flag_clear(_Flag, _Memory_Order) ->
    erlang:nif_error(nif_library_not_loaded).

-spec atomic_flag_test_and_set(_Flag :: vegrandis_flag:atomic_flag()) -> boolean().
atomic_flag_test_and_set(_Flag) ->
    erlang:nif_error(nif_library_not_loaded).

-spec atomic_flag_test_and_set(_Flag :: vegrandis_flag:atomic_flag(),
                               _MemoryOrder :: vegrandis_flag:memory_order()) ->  boolean().
atomic_flag_test_and_set(_Flag, _Memory_Order) ->
    erlang:nif_error(nif_library_not_loaded).


-spec atomic_var_new(_Type :: vegrandis:var_type()) -> {ok, vegrandis:atomic_var()} | {error, out_of_memory}.
atomic_var_new(_Type) ->
    erlang:nif_error(nif_library_not_loaded).

-spec atomic_var_is_lock_free(_Var :: vegrandis:atomic_var()) -> boolean().
atomic_var_is_lock_free(_Var) ->
    erlang:nif_error(nif_library_not_loaded).

-spec atomic_var_store(_Var :: vegrandis:atomic_var(), _Value :: term()) -> ok.
atomic_var_store(_Var, _Value) ->
    erlang:nif_error(nif_library_not_loaded).

-spec atomic_var_store(_Var :: vegrandis:atomic_var(), _Value :: term(), _Memory_Order :: vegrandis:memory_order()) -> ok.
atomic_var_store(_Var, _Value, _Memory_Order) ->
    erlang:nif_error(nif_library_not_loaded).

-spec atomic_var_load(_Var :: vegrandis:atomic_var()) -> term().
atomic_var_load(_Var) ->
    erlang:nif_error(nif_library_not_loaded).

-spec atomic_var_load(_Var :: vegrandis:atomic_var(), _Memory_Order :: vegrandis:memory_order()) -> term().
atomic_var_load(_Var, _Memory_Order) ->
    erlang:nif_error(nif_library_not_loaded).

-spec atomic_var_exchange(_Var :: vegrandis:atomic_var(), _Value :: term()) -> term().
atomic_var_exchange(_Var, _Value) ->
    erlang:nif_error(nif_library_not_loaded).

-spec atomic_var_exchange(_Var :: vegrandis:atomic_var(), _Value :: term(), _Memory_Order :: vegrandis:memory_order()) -> term().
atomic_var_exchange(_Var, _Value, _Memory_Order) ->
    erlang:nif_error(nif_library_not_loaded).

-spec atomic_var_compare_exchange_weak(_Var :: vegrandis:atomic_var(), _Expected :: integer(), _Desired :: integer())
        -> true | {false, integer()}.
atomic_var_compare_exchange_weak(_Var, _Expected, _Desired) ->
    erlang:nif_error(nif_library_not_loaded).

-spec atomic_var_compare_exchange_weak(_Var :: vegrandis:atomic_var(), _Expected :: integer(), _Desired :: integer(),
                                       _Succ_Memory_Order :: vegrandis:memory_order(), _Fail_Memory_Order :: vegrandis:memory_order())
        -> true | {false, integer()}.
atomic_var_compare_exchange_weak(_Var, _Expected, _Desired, _Succ_Memory_Order, _Fail_Memory_Order) ->
    erlang:nif_error(nif_library_not_loaded).

-spec atomic_var_compare_exchange_strong(_Var :: vegrandis:atomic_var(), _Expected :: integer(), _Desired :: integer())
        -> true | {false, integer()}.
atomic_var_compare_exchange_strong(_Var, _Expected, _Desired) ->
    erlang:nif_error(nif_library_not_loaded).

-spec atomic_var_compare_exchange_strong(_Var :: vegrandis:atomic_var(), _Expected :: integer(), _Desired :: integer(),
                                         _Succ_Memory_Order :: vegrandis:memory_order(), _Fail_Memory_Order :: vegrandis:memory_order())
        -> true | {false, integer()}.
atomic_var_compare_exchange_strong(_Var, _Expected, _Desired, _Succ_Memory_Order, _Fail_Memory_Order) ->
    erlang:nif_error(nif_library_not_loaded).

-spec atomic_var_fetch_add(_Var :: vegrandis:atomic_var(), _Arg :: integer()) -> integer().
atomic_var_fetch_add(_Var, _Arg) ->
    erlang:nif_error(nif_library_not_loaded).

-spec atomic_var_fetch_add(_Var :: vegrandis:atomic_var(), _Arg :: integer(), _Memory_Order :: vegrandis:memory_order()) -> integer().
atomic_var_fetch_add(_Var, _Arg, _Memory_Order) ->
    erlang:nif_error(nif_library_not_loaded).

-spec atomic_var_fetch_sub(_Var :: vegrandis:atomic_var(), _Arg :: integer()) -> integer().
atomic_var_fetch_sub(_Var, _Arg) ->
    erlang:nif_error(nif_library_not_loaded).

-spec atomic_var_fetch_sub(_Var :: vegrandis:atomic_var(), _Arg :: integer(), _Memory_Order :: vegrandis:memory_order()) -> integer().
atomic_var_fetch_sub(_Var, _Arg, _Memory_Order) ->
    erlang:nif_error(nif_library_not_loaded).

-spec atomic_var_fetch_and(_Var :: vegrandis:atomic_var(), _Arg :: integer()) -> integer().
atomic_var_fetch_and(_Var, _Arg) ->
    erlang:nif_error(nif_library_not_loaded).

-spec atomic_var_fetch_and(_Var :: vegrandis:atomic_var(), _Arg :: integer(), _Memory_Order :: vegrandis:memory_order()) -> integer().
atomic_var_fetch_and(_Var, _Arg, _Memory_Order) ->
    erlang:nif_error(nif_library_not_loaded).

-spec atomic_var_fetch_or(_Var :: vegrandis:atomic_var(), _Arg :: integer()) -> integer().
atomic_var_fetch_or(_Var, _Arg) ->
    erlang:nif_error(nif_library_not_loaded).

-spec atomic_var_fetch_or(_Var :: vegrandis:atomic_var(), _Arg :: integer(), _Memory_Order :: vegrandis:memory_order()) -> integer().
atomic_var_fetch_or(_Var, _Arg, _Memory_Order) ->
    erlang:nif_error(nif_library_not_loaded).

-spec atomic_var_fetch_xor(_Var :: vegrandis:atomic_var(), _Arg :: integer()) -> integer().
atomic_var_fetch_xor(_Var, _Arg) ->
    erlang:nif_error(nif_library_not_loaded).

-spec atomic_var_fetch_xor(_Var :: vegrandis:atomic_var(), _Arg :: integer(), _Memory_Order :: vegrandis:memory_order()) -> integer().
atomic_var_fetch_xor(_Var, _Arg, _Memory_Order) ->
    erlang:nif_error(nif_library_not_loaded).

-spec atomic_var_add_fetch(_Var :: vegrandis:atomic_var(), _Arg :: integer()) -> integer().
atomic_var_add_fetch(_Var, _Arg) ->
    erlang:nif_error(nif_library_not_loaded).

-spec atomic_var_sub_fetch(_Var :: vegrandis:atomic_var(), _Arg :: integer()) -> integer().
atomic_var_sub_fetch(_Var, _Arg) ->
    erlang:nif_error(nif_library_not_loaded).

-spec atomic_var_and_fetch(_Var :: vegrandis:atomic_var(), _Arg :: integer()) -> integer().
atomic_var_and_fetch(_Var, _Arg) ->
    erlang:nif_error(nif_library_not_loaded).

-spec atomic_var_or_fetch(_Var :: vegrandis:atomic_var(), _Arg :: integer()) -> integer().
atomic_var_or_fetch(_Var, _Arg) ->
    erlang:nif_error(nif_library_not_loaded).

-spec atomic_var_xor_fetch(_Var :: vegrandis:atomic_var(), _Arg :: integer()) -> integer().
atomic_var_xor_fetch(_Var, _Arg) ->
    erlang:nif_error(nif_library_not_loaded).
