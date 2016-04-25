

# vegrandis #

Copyright (c) 2016 Guilherme Andrade

__Version:__ 1.0.0

__Authors:__ Guilherme Andrade ([`vegrandis(at)gandrade(dot)net`](mailto:vegrandis(at)gandrade(dot)net)).

`vegrandis`: Native atomic shared variables for Erlang

---------

`vegrandis` provides native atomic variables and flags that can be shared between processes in the same node.

It wraps around C++11's [std::atomic](http://en.cppreference.com/w/cpp/atomic/atomic) library - all standard integral data types can be used and most of the original operations can be performed, including optionally specifying [memory ordering](http://en.cppreference.com/w/cpp/atomic/memory_order) constraints and, if the hardware allows it, in a [lockfree](http://en.cppreference.com/w/cpp/atomic/atomic_is_lock_free) fashion.


### <a name="Examples">Examples</a> ###


```erlang

{ok, AtomicCounter} = vegrandis_var:new(uint8),
Increments = 10,
Parent = self(),

[spawn(
    fun () ->
        Parent ! vegrandis_var:fetch_add(AtomicCounter, 1)
    end)
 || _ <- lists:seq(1, Increments)],

% [0,1,2,3,4,5,6,7,8,9]
[receive Value -> Value end || _ <- lists:seq(1, Increments)],

% 10
vegrandis_var:load(AtomicCounter).

```

```erlang

{ok, AtomicCounter} = vegrandis_var:new(int_fast32),
vegrandis_var:store(AtomicCounter, 123),
[spawn(
    fun F() ->
        Value = vegrandis_var:load(AtomicCounter, memory_order_relaxed),
        case vegrandis_var:compare_exchange_weak(AtomicCounter,
                Value, Value + 1, memory_order_release, memory_order_relaxed)
        of
            true ->
                ok;
            {false, ChangedValue} ->
                F()
        end
    end)
 || _ <- lists:seq(1, 100)],

timer:sleep(1000),
vegrandis_var:load(AtomicCounter). % 223

```

```erlang

{ok, AtomicFlag} = vegrandis_flag:new(),
[spawn(
    fun F() ->
        case vegrandis_flag:test_and_set(AtomicFlag) of
            false ->
                io:format("~p acquired spinlock~n", [self()]),
                vegrandis_flag:clear(AtomicFlag),
                io:format("~p cleared spinlock~n", [self()]);
            true ->
                F()
        end
    end)
 || _ <- lists:seq(1, 10)].

```


### <a name="Variable_types">Variable types</a> ###


* int8
* uint8
* int16
* uint16
* int32
* uint32
* int64
* uint64
* char
* schar
* uchar
* short
* ushort
* int
* uint
* long
* ulong
* llong
* ullong
* char16
* char32
* wchar
* int_least8
* uint_least8
* int_least16
* uint_least16
* int_least32
* uint_least32
* int_least64
* uint_least64
* int_fast8
* uint_fast8
* int_fast16
* uint_fast16
* int_fast32
* uint_fast32
* int_fast64
* uint_fast64
* intmax
* uintmax


### <a name="Memory_orderings">Memory orderings</a> ###

* memory_order_relaxed
* memory_order_consume
* memory_order_acquire
* memory_order_release
* memory_order_acq_rel
* memory_order_seq_cst


## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="https://github.com/g-andrade/vegrandis/blob/master/doc/vegrandis_flag.md" class="module">vegrandis_flag</a></td></tr>
<tr><td><a href="https://github.com/g-andrade/vegrandis/blob/master/doc/vegrandis_var.md" class="module">vegrandis_var</a></td></tr></table>

