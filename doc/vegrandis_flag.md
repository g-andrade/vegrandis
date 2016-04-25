

# Module vegrandis_flag #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)



<a name="types"></a>

## Data Types ##




### <a name="type-atomic_flag">atomic_flag()</a> ###



<pre><code>
atomic_flag() = term()
</code></pre>





### <a name="type-memory_order">memory_order()</a> ###



<pre><code>
memory_order() = (memory_order_relaxed | memory_order_consume | memory_order_acquire | memory_order_release | memory_order_acq_rel | memory_order_seq_cst)
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#clear-1">clear/1</a></td><td></td></tr><tr><td valign="top"><a href="#clear-2">clear/2</a></td><td></td></tr><tr><td valign="top"><a href="#new-0">new/0</a></td><td></td></tr><tr><td valign="top"><a href="#test_and_set-1">test_and_set/1</a></td><td></td></tr><tr><td valign="top"><a href="#test_and_set-2">test_and_set/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="clear-1"></a>

### clear/1 ###


<pre><code>
clear(Flag::<a href="#type-atomic_flag">atomic_flag()</a>) -&gt; ok
</code></pre>

<br></br>



<a name="clear-2"></a>

### clear/2 ###


<pre><code>
clear(Flag::<a href="#type-atomic_flag">atomic_flag()</a>, MemoryOrder::<a href="#type-memory_order">memory_order()</a>) -&gt; ok
</code></pre>

<br></br>



<a name="new-0"></a>

### new/0 ###


<pre><code>
new() -&gt; {ok, <a href="#type-atomic_flag">atomic_flag()</a>} | {error, out_of_memory}
</code></pre>

<br></br>



<a name="test_and_set-1"></a>

### test_and_set/1 ###


<pre><code>
test_and_set(Flag::<a href="#type-atomic_flag">atomic_flag()</a>) -&gt; boolean()
</code></pre>

<br></br>



<a name="test_and_set-2"></a>

### test_and_set/2 ###


<pre><code>
test_and_set(Flag::<a href="#type-atomic_flag">atomic_flag()</a>, MemoryOrder::<a href="#type-memory_order">memory_order()</a>) -&gt; boolean()
</code></pre>

<br></br>



