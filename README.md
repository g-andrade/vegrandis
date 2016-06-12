

# vegrandis #

Copyright (c) 2016 Guilherme Andrade

__Version:__ 3.0.0

__Authors:__ Guilherme Andrade ([`vegrandis(at)gandrade(dot)net`](mailto:vegrandis(at)gandrade(dot)net)).

`vegrandis`: Atomic shared variables for Erlang

---------

`vegrandis` provides atomic variables - for both native integral types and Erlang terms - and native flags that can be shared between Erlang processes living in the same node.

It consists of a [NIF](http://erlang.org/doc/man/erl_nif.md) library wrapping around C++11's [std::atomic](http://en.cppreference.com/w/cpp/atomic/atomic); a majority of the standard integral data types can be used and most of the original operations can be performed, including optionally specifying [memory ordering](http://en.cppreference.com/w/cpp/atomic/memory_order) constraints and, if both hardware and compiler implementation allow it, operating in a [lockfree](http://en.cppreference.com/w/cpp/atomic/atomic_is_lock_free) fashion.

Any allocated variables will be automatically deallocated by the garbage collector once there are no more references to it.

Original development rig runs OTP 17.5 over GNU/Linux x86_64, and quick test with OTP 17.5 over GNU/Linux ARM was also successful; other platforms have not been tested. Building dependencies include `make` and `g++` (but there's no reason `clang` won't work as well.)


### <a name="Examples">Examples</a> ###


```erlang

SharedTerm = vegrandis:new(),
vegrandis:store(SharedTerm, math:pi()),
spawn(fun () ->
          io:format("stored value: ~p~n", [vegrandis:load(SharedTerm)])
      end).
% stored value: 3.141592653589793

```


---------


```erlang

AtomicCounter = vegrandis:new(uint8),
Increments = 10,
Parent = self(),

[spawn(
    fun () ->
        Parent ! vegrandis:fetch_add(AtomicCounter, 1)
    end)
 || _ <- lists:seq(1, Increments)],

% [0,1,2,3,4,5,6,7,8,9]
[receive Value -> Value end || _ <- lists:seq(1, Increments)],

% 10
vegrandis:load(AtomicCounter).

```


---------


```erlang

AtomicCounter = vegrandis:new(int_fast32),
vegrandis:store(AtomicCounter, 123),
[spawn(
    fun F() ->
        Value = vegrandis:load(AtomicCounter, memory_order_relaxed),
        case vegrandis:compare_exchange_weak(AtomicCounter,
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
vegrandis:load(AtomicCounter). % 223

```


---------


```erlang

AtomicFlag = vegrandis_flag:new(),
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


---------


```erlang

AtomicCounter = vegrandis:new(long),
vegrandis:is_lock_free(AtomicCounter) orelse exit(this_wont_do).

```


### <a name="Building">Building</a> ###

The ERTS headers are required; if they're not available on a global include path, 'ERL_INCLUDE' can be explicitly defined.

```bash

% Example path
ERL_INCLUDE=/opt/kerl/17.5/usr/include rebar compile

```


The NIF shared object will be dumped into the priv/ directory.


### <a name="Variable_types">Variable types</a> ###


* term (any Erlang term)
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


### <a name="TODO">TODO</a> ###

* Unit tests
* Clean up C++ code / look for alternatives to the current template hell
* Support named variables (in ETS fashion)


## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="https://github.com/g-andrade/vegrandis/blob/master/doc/vegrandis.md" class="module">vegrandis</a></td></tr>
<tr><td><a href="https://github.com/g-andrade/vegrandis/blob/master/doc/vegrandis_flag.md" class="module">vegrandis_flag</a></td></tr></table>

