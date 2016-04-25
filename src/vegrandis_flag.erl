-module(vegrandis_flag).
-author('Guilherme Andrade <vegrandis(at)gandrade(dot)net>').

-export([new/0]).          -ignore_xref({new, 0}).
-export([clear/1]).        -ignore_xref({clear, 1}).
-export([clear/2]).        -ignore_xref({clear, 2}).
-export([test_and_set/1]). -ignore_xref({test_and_set, 1}).
-export([test_and_set/2]). -ignore_xref({test_and_set, 2}).

%% @headerfile "../include/vegrandis.hrl"
-include("vegrandis.hrl").
-export_type([memory_order/0]).

-type atomic_flag() :: term().
-export_type([atomic_flag/0]).


-spec new() -> {ok, atomic_flag()} | {error, out_of_memory}.
new() ->
    vegrandis_nif:atomic_flag_new().

-spec clear(Flag :: atomic_flag()) -> ok.
clear(Flag) ->
    vegrandis_nif:atomic_flag_clear(Flag).

-spec clear(Flag :: atomic_flag(), MemoryOrder :: memory_order()) -> ok.
clear(Flag, MemoryOrder) ->
    vegrandis_nif:atomic_flag_clear(Flag, MemoryOrder).

-spec test_and_set(Flag :: atomic_flag()) -> boolean().
test_and_set(Flag) ->
    vegrandis_nif:atomic_flag_test_and_set(Flag).

-spec test_and_set(Flag :: atomic_flag(), MemoryOrder :: memory_order()) ->  boolean().
test_and_set(Flag, MemoryOrder) ->
    vegrandis_nif:atomic_flag_test_and_set(Flag, MemoryOrder).
