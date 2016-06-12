-module(vegrandis).
-author('Guilherme Andrade <vegrandis(at)gandrade(dot)net>').

-export([new/0]).                     -ignore_xref({new, 0}).
-export([new/1]).                     -ignore_xref({new, 1}).
-export([is_lock_free/1]).            -ignore_xref({is_lock_free, 1}).
-export([store/2]).                   -ignore_xref({store, 2}).
-export([store/3]).                   -ignore_xref({store, 3}).
-export([load/1]).                    -ignore_xref({load, 1}).
-export([load/2]).                    -ignore_xref({load, 2}).
-export([exchange/2]).                -ignore_xref({exchange, 2}).
-export([exchange/3]).                -ignore_xref({exchange, 3}).
-export([compare_exchange_weak/3]).   -ignore_xref({compare_exchange_weak, 3}).
-export([compare_exchange_weak/5]).   -ignore_xref({compare_exchange_weak, 5}).
-export([compare_exchange_strong/3]). -ignore_xref({compare_exchange_strong, 3}).
-export([compare_exchange_strong/5]). -ignore_xref({compare_exchange_strong, 5}).
-export([fetch_add/2]).               -ignore_xref({fetch_add, 2}).
-export([fetch_add/3]).               -ignore_xref({fetch_add, 3}).
-export([fetch_sub/2]).               -ignore_xref({fetch_sub, 2}).
-export([fetch_sub/3]).               -ignore_xref({fetch_sub, 3}).
-export([fetch_and/2]).               -ignore_xref({fetch_and, 2}).
-export([fetch_and/3]).               -ignore_xref({fetch_and, 3}).
-export([fetch_or/2]).                -ignore_xref({fetch_or, 2}).
-export([fetch_or/3]).                -ignore_xref({fetch_or, 3}).
-export([fetch_xor/2]).               -ignore_xref({fetch_xor, 2}).
-export([fetch_xor/3]).               -ignore_xref({fetch_xor, 3}).
-export([add_fetch/2]).               -ignore_xref({add_fetch, 2}).
-export([sub_fetch/2]).               -ignore_xref({sub_fetch, 2}).
-export([and_fetch/2]).               -ignore_xref({and_fetch, 2}).
-export([or_fetch/2]).                -ignore_xref({or_fetch, 2}).
-export([xor_fetch/2]).               -ignore_xref({xor_fetch, 2}).

%% @headerfile "../include/vegrandis.hrl"
-include("vegrandis.hrl").
-export_type([var_type/0, memory_order/0]).

-type atomic_var() :: term().
-export_type([atomic_var/0]).

-spec new() -> atomic_var().
new() ->
    new(term).

-spec new(Type :: var_type()) -> atomic_var().
new(Type) ->
    vegrandis_nif:atomic_var_new(Type).

-spec is_lock_free(Var :: atomic_var()) -> boolean().
is_lock_free(Var) ->
    vegrandis_nif:atomic_var_is_lock_free(Var).

-spec store(Var :: atomic_var(), Value :: term()) -> ok.
store(Var, Value) ->
    vegrandis_nif:atomic_var_store(Var, Value).

-spec store(Var :: atomic_var(), Value :: term(), MemoryOrder :: memory_order()) -> ok.
store(Var, Value, MemoryOrder) ->
    vegrandis_nif:atomic_var_store(Var, Value, MemoryOrder).

-spec load(Var :: atomic_var()) -> term().
load(Var) ->
    vegrandis_nif:atomic_var_load(Var).

-spec load(Var :: atomic_var(), MemoryOrder :: memory_order()) -> term().
load(Var, MemoryOrder) ->
    vegrandis_nif:atomic_var_load(Var, MemoryOrder).

-spec exchange(Var :: atomic_var(), Value :: term()) -> term().
exchange(Var, Value) ->
    vegrandis_nif:atomic_var_exchange(Var, Value).

-spec exchange(Var :: atomic_var(), Value :: term(), MemoryOrder :: memory_order()) -> term().
exchange(Var, Value, MemoryOrder) ->
    vegrandis_nif:atomic_var_exchange(Var, Value, MemoryOrder).

-spec compare_exchange_weak(Var :: atomic_var(), Expected :: integer(), Desired :: integer())
        -> true | {false, integer()}.
compare_exchange_weak(Var, Expected, Desired) ->
    vegrandis_nif:atomic_var_compare_exchange_weak(Var, Expected, Desired).

-spec compare_exchange_weak(Var :: atomic_var(), Expected :: integer(), Desired :: integer(),
                            SuccMemoryOrder :: memory_order(), FailMemoryOrder :: memory_order())
        -> true | {false, integer()}.
compare_exchange_weak(Var, Expected, Desired, SuccMemoryOrder, FailMemoryOrder) ->
    vegrandis_nif:atomic_var_compare_exchange_weak(Var, Expected, Desired, SuccMemoryOrder, FailMemoryOrder).

-spec compare_exchange_strong(Var :: atomic_var(), Expected :: integer(), Desired :: integer())
        -> true | {false, integer()}.
compare_exchange_strong(Var, Expected, Desired) ->
    vegrandis_nif:atomic_var_compare_exchange_strong(Var, Expected, Desired).

-spec compare_exchange_strong(Var :: atomic_var(), Expected :: integer(), Desired :: integer(),
                              SuccMemoryOrder :: memory_order(), FailMemoryOrder :: memory_order())
        -> true | {false, integer()}.
compare_exchange_strong(Var, Expected, Desired, SuccMemoryOrder, FailMemoryOrder) ->
    vegrandis_nif:atomic_var_compare_exchange_strong(Var, Expected, Desired, SuccMemoryOrder, FailMemoryOrder).

-spec fetch_add(Var :: atomic_var(), Arg :: integer()) -> integer().
fetch_add(Var, Arg) ->
    vegrandis_nif:atomic_var_fetch_add(Var, Arg).

-spec fetch_add(Var :: atomic_var(), Arg :: integer(), MemoryOrder :: memory_order()) -> integer().
fetch_add(Var, Arg, MemoryOrder) ->
    vegrandis_nif:atomic_var_fetch_add(Var, Arg, MemoryOrder).

-spec fetch_sub(Var :: atomic_var(), Arg :: integer()) -> integer().
fetch_sub(Var, Arg) ->
    vegrandis_nif:atomic_var_fetch_sub(Var, Arg).

-spec fetch_sub(Var :: atomic_var(), Arg :: integer(), MemoryOrder :: memory_order()) -> integer().
fetch_sub(Var, Arg, MemoryOrder) ->
    vegrandis_nif:atomic_var_fetch_sub(Var, Arg, MemoryOrder).

-spec fetch_and(Var :: atomic_var(), Arg :: integer()) -> integer().
fetch_and(Var, Arg) ->
    vegrandis_nif:atomic_var_fetch_and(Var, Arg).

-spec fetch_and(Var :: atomic_var(), Arg :: integer(), MemoryOrder :: memory_order()) -> integer().
fetch_and(Var, Arg, MemoryOrder) ->
    vegrandis_nif:atomic_var_fetch_and(Var, Arg, MemoryOrder).

-spec fetch_or(Var :: atomic_var(), Arg :: integer()) -> integer().
fetch_or(Var, Arg) ->
    vegrandis_nif:atomic_var_fetch_or(Var, Arg).

-spec fetch_or(Var :: atomic_var(), Arg :: integer(), MemoryOrder :: memory_order()) -> integer().
fetch_or(Var, Arg, MemoryOrder) ->
    vegrandis_nif:atomic_var_fetch_or(Var, Arg, MemoryOrder).

-spec fetch_xor(Var :: atomic_var(), Arg :: integer()) -> integer().
fetch_xor(Var, Arg) ->
    vegrandis_nif:atomic_var_fetch_xor(Var, Arg).

-spec fetch_xor(Var :: atomic_var(), Arg :: integer(), MemoryOrder :: memory_order()) -> integer().
fetch_xor(Var, Arg, MemoryOrder) ->
    vegrandis_nif:atomic_var_fetch_xor(Var, Arg, MemoryOrder).

-spec add_fetch(Var :: atomic_var(), Arg :: integer()) -> integer().
add_fetch(Var, Arg) ->
    vegrandis_nif:atomic_var_add_fetch(Var, Arg).

-spec sub_fetch(Var :: atomic_var(), Arg :: integer()) -> integer().
sub_fetch(Var, Arg) ->
    vegrandis_nif:atomic_var_sub_fetch(Var, Arg).

-spec and_fetch(Var :: atomic_var(), Arg :: integer()) -> integer().
and_fetch(Var, Arg) ->
    vegrandis_nif:atomic_var_and_fetch(Var, Arg).

-spec or_fetch(Var :: atomic_var(), Arg :: integer()) -> integer().
or_fetch(Var, Arg) ->
    vegrandis_nif:atomic_var_or_fetch(Var, Arg).

-spec xor_fetch(Var :: atomic_var(), Arg :: integer()) -> integer().
xor_fetch(Var, Arg) ->
    vegrandis_nif:atomic_var_xor_fetch(Var, Arg).
