

# Module vegrandis #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)



<a name="types"></a>

## Data Types ##




### <a name="type-atomic_var">atomic_var()</a> ###



<pre><code>
atomic_var() = term()
</code></pre>





### <a name="type-memory_order">memory_order()</a> ###



<pre><code>
memory_order() = (memory_order_relaxed | memory_order_consume | memory_order_acquire | memory_order_release | memory_order_acq_rel | memory_order_seq_cst)
</code></pre>





### <a name="type-var_type">var_type()</a> ###



<pre><code>
var_type() = (char | schar | uchar | short | ushort | int | uint | long | ulong | llong | ullong | char16 | char32 | wchar | int_least8 | uint_least8 | int_least16 | uint_least16 | int_least32 | uint_least32 | int_least64 | uint_least64 | int_fast8 | uint_fast8 | int_fast16 | uint_fast16 | int_fast32 | uint_fast32 | int_fast64 | uint_fast64 | intmax | uintmax | int8 | uint8 | int16 | uint16 | int32 | uint32 | int64 | uint64 | term)
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_fetch-2">add_fetch/2</a></td><td></td></tr><tr><td valign="top"><a href="#and_fetch-2">and_fetch/2</a></td><td></td></tr><tr><td valign="top"><a href="#compare_exchange_strong-3">compare_exchange_strong/3</a></td><td></td></tr><tr><td valign="top"><a href="#compare_exchange_strong-5">compare_exchange_strong/5</a></td><td></td></tr><tr><td valign="top"><a href="#compare_exchange_weak-3">compare_exchange_weak/3</a></td><td></td></tr><tr><td valign="top"><a href="#compare_exchange_weak-5">compare_exchange_weak/5</a></td><td></td></tr><tr><td valign="top"><a href="#exchange-2">exchange/2</a></td><td></td></tr><tr><td valign="top"><a href="#exchange-3">exchange/3</a></td><td></td></tr><tr><td valign="top"><a href="#fetch_add-2">fetch_add/2</a></td><td></td></tr><tr><td valign="top"><a href="#fetch_add-3">fetch_add/3</a></td><td></td></tr><tr><td valign="top"><a href="#fetch_and-2">fetch_and/2</a></td><td></td></tr><tr><td valign="top"><a href="#fetch_and-3">fetch_and/3</a></td><td></td></tr><tr><td valign="top"><a href="#fetch_or-2">fetch_or/2</a></td><td></td></tr><tr><td valign="top"><a href="#fetch_or-3">fetch_or/3</a></td><td></td></tr><tr><td valign="top"><a href="#fetch_sub-2">fetch_sub/2</a></td><td></td></tr><tr><td valign="top"><a href="#fetch_sub-3">fetch_sub/3</a></td><td></td></tr><tr><td valign="top"><a href="#fetch_xor-2">fetch_xor/2</a></td><td></td></tr><tr><td valign="top"><a href="#fetch_xor-3">fetch_xor/3</a></td><td></td></tr><tr><td valign="top"><a href="#is_lock_free-1">is_lock_free/1</a></td><td></td></tr><tr><td valign="top"><a href="#load-1">load/1</a></td><td></td></tr><tr><td valign="top"><a href="#load-2">load/2</a></td><td></td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td></td></tr><tr><td valign="top"><a href="#or_fetch-2">or_fetch/2</a></td><td></td></tr><tr><td valign="top"><a href="#store-2">store/2</a></td><td></td></tr><tr><td valign="top"><a href="#store-3">store/3</a></td><td></td></tr><tr><td valign="top"><a href="#sub_fetch-2">sub_fetch/2</a></td><td></td></tr><tr><td valign="top"><a href="#xor_fetch-2">xor_fetch/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_fetch-2"></a>

### add_fetch/2 ###


<pre><code>
add_fetch(Var::<a href="#type-atomic_var">atomic_var()</a>, Arg::integer()) -&gt; integer()
</code></pre>

<br></br>



<a name="and_fetch-2"></a>

### and_fetch/2 ###


<pre><code>
and_fetch(Var::<a href="#type-atomic_var">atomic_var()</a>, Arg::integer()) -&gt; integer()
</code></pre>

<br></br>



<a name="compare_exchange_strong-3"></a>

### compare_exchange_strong/3 ###


<pre><code>
compare_exchange_strong(Var::<a href="#type-atomic_var">atomic_var()</a>, Expected::integer(), Desired::integer()) -&gt; true | {false, integer()}
</code></pre>

<br></br>



<a name="compare_exchange_strong-5"></a>

### compare_exchange_strong/5 ###


<pre><code>
compare_exchange_strong(Var::<a href="#type-atomic_var">atomic_var()</a>, Expected::integer(), Desired::integer(), SuccMemoryOrder::<a href="#type-memory_order">memory_order()</a>, FailMemoryOrder::<a href="#type-memory_order">memory_order()</a>) -&gt; true | {false, integer()}
</code></pre>

<br></br>



<a name="compare_exchange_weak-3"></a>

### compare_exchange_weak/3 ###


<pre><code>
compare_exchange_weak(Var::<a href="#type-atomic_var">atomic_var()</a>, Expected::integer(), Desired::integer()) -&gt; true | {false, integer()}
</code></pre>

<br></br>



<a name="compare_exchange_weak-5"></a>

### compare_exchange_weak/5 ###


<pre><code>
compare_exchange_weak(Var::<a href="#type-atomic_var">atomic_var()</a>, Expected::integer(), Desired::integer(), SuccMemoryOrder::<a href="#type-memory_order">memory_order()</a>, FailMemoryOrder::<a href="#type-memory_order">memory_order()</a>) -&gt; true | {false, integer()}
</code></pre>

<br></br>



<a name="exchange-2"></a>

### exchange/2 ###


<pre><code>
exchange(Var::<a href="#type-atomic_var">atomic_var()</a>, Value::term()) -&gt; term()
</code></pre>

<br></br>



<a name="exchange-3"></a>

### exchange/3 ###


<pre><code>
exchange(Var::<a href="#type-atomic_var">atomic_var()</a>, Value::term(), MemoryOrder::<a href="#type-memory_order">memory_order()</a>) -&gt; term()
</code></pre>

<br></br>



<a name="fetch_add-2"></a>

### fetch_add/2 ###


<pre><code>
fetch_add(Var::<a href="#type-atomic_var">atomic_var()</a>, Arg::integer()) -&gt; integer()
</code></pre>

<br></br>



<a name="fetch_add-3"></a>

### fetch_add/3 ###


<pre><code>
fetch_add(Var::<a href="#type-atomic_var">atomic_var()</a>, Arg::integer(), MemoryOrder::<a href="#type-memory_order">memory_order()</a>) -&gt; integer()
</code></pre>

<br></br>



<a name="fetch_and-2"></a>

### fetch_and/2 ###


<pre><code>
fetch_and(Var::<a href="#type-atomic_var">atomic_var()</a>, Arg::integer()) -&gt; integer()
</code></pre>

<br></br>



<a name="fetch_and-3"></a>

### fetch_and/3 ###


<pre><code>
fetch_and(Var::<a href="#type-atomic_var">atomic_var()</a>, Arg::integer(), MemoryOrder::<a href="#type-memory_order">memory_order()</a>) -&gt; integer()
</code></pre>

<br></br>



<a name="fetch_or-2"></a>

### fetch_or/2 ###


<pre><code>
fetch_or(Var::<a href="#type-atomic_var">atomic_var()</a>, Arg::integer()) -&gt; integer()
</code></pre>

<br></br>



<a name="fetch_or-3"></a>

### fetch_or/3 ###


<pre><code>
fetch_or(Var::<a href="#type-atomic_var">atomic_var()</a>, Arg::integer(), MemoryOrder::<a href="#type-memory_order">memory_order()</a>) -&gt; integer()
</code></pre>

<br></br>



<a name="fetch_sub-2"></a>

### fetch_sub/2 ###


<pre><code>
fetch_sub(Var::<a href="#type-atomic_var">atomic_var()</a>, Arg::integer()) -&gt; integer()
</code></pre>

<br></br>



<a name="fetch_sub-3"></a>

### fetch_sub/3 ###


<pre><code>
fetch_sub(Var::<a href="#type-atomic_var">atomic_var()</a>, Arg::integer(), MemoryOrder::<a href="#type-memory_order">memory_order()</a>) -&gt; integer()
</code></pre>

<br></br>



<a name="fetch_xor-2"></a>

### fetch_xor/2 ###


<pre><code>
fetch_xor(Var::<a href="#type-atomic_var">atomic_var()</a>, Arg::integer()) -&gt; integer()
</code></pre>

<br></br>



<a name="fetch_xor-3"></a>

### fetch_xor/3 ###


<pre><code>
fetch_xor(Var::<a href="#type-atomic_var">atomic_var()</a>, Arg::integer(), MemoryOrder::<a href="#type-memory_order">memory_order()</a>) -&gt; integer()
</code></pre>

<br></br>



<a name="is_lock_free-1"></a>

### is_lock_free/1 ###


<pre><code>
is_lock_free(Var::<a href="#type-atomic_var">atomic_var()</a>) -&gt; boolean()
</code></pre>

<br></br>



<a name="load-1"></a>

### load/1 ###


<pre><code>
load(Var::<a href="#type-atomic_var">atomic_var()</a>) -&gt; term()
</code></pre>

<br></br>



<a name="load-2"></a>

### load/2 ###


<pre><code>
load(Var::<a href="#type-atomic_var">atomic_var()</a>, MemoryOrder::<a href="#type-memory_order">memory_order()</a>) -&gt; term()
</code></pre>

<br></br>



<a name="new-1"></a>

### new/1 ###


<pre><code>
new(Type::<a href="#type-var_type">var_type()</a>) -&gt; {ok, <a href="#type-atomic_var">atomic_var()</a>} | {error, out_of_memory}
</code></pre>

<br></br>



<a name="or_fetch-2"></a>

### or_fetch/2 ###


<pre><code>
or_fetch(Var::<a href="#type-atomic_var">atomic_var()</a>, Arg::integer()) -&gt; integer()
</code></pre>

<br></br>



<a name="store-2"></a>

### store/2 ###


<pre><code>
store(Var::<a href="#type-atomic_var">atomic_var()</a>, Value::term()) -&gt; ok
</code></pre>

<br></br>



<a name="store-3"></a>

### store/3 ###


<pre><code>
store(Var::<a href="#type-atomic_var">atomic_var()</a>, Value::term(), MemoryOrder::<a href="#type-memory_order">memory_order()</a>) -&gt; ok
</code></pre>

<br></br>



<a name="sub_fetch-2"></a>

### sub_fetch/2 ###


<pre><code>
sub_fetch(Var::<a href="#type-atomic_var">atomic_var()</a>, Arg::integer()) -&gt; integer()
</code></pre>

<br></br>



<a name="xor_fetch-2"></a>

### xor_fetch/2 ###


<pre><code>
xor_fetch(Var::<a href="#type-atomic_var">atomic_var()</a>, Arg::integer()) -&gt; integer()
</code></pre>

<br></br>



