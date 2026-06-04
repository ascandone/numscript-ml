## Encoding
There are 3 distincts (but related) data formats:
1. The serialization format we'll use to store scripts in db or send them around as raft payloads. It explicitly lists location of tables.
2. The deserialized format, expressed as golang data structure. This'll strip away headers used to index tables and load them directly as slices. In order to avoid unnecessary allocations, bytecode operations will be expressed via stack-allocated go slices (no heap-allocated values like strings or recursive data). For example, data segment will be loaded in different pools, but values won't be inlined in instructions.
3. The hydrated vm instance.

Serialization format:

TODO check format after we finish design all ops, check padding, etc
```
[? bytes] magic header ("num")
[? bytes] padding

[? bytes] index,len of data segment
[? bytes] index,len of data table
[? bytes] index,len of expr segmnet
[? bytes] index,len of sources table
[? bytes] index,len of dests table
---

... contiguous (padded) data as indexed by headers

```

TODO is data segment aligned? if so, should the len be %4? (same for other semgnets)


TODO specify exact encoding


## Issue: allowing re-using previously allocated vm instances
TODO

## Issue: avoiding unnecessary int re-alloc
The ADD operation always allocates a third int.
```
ADD() := {
  l := pop()
  r := pop()
  push(l + r)
}
```

This is trivially solved at compile time when we can perform constant folding, but that's not always the case
To fix that, we can introduce mutable opcodes (in *addition to* the immutable ones):
```
ADD_MUT() := {
  l := pop()
  r := peek()
  r.add(l)
}
```
Static analysis can determine when it's sound to emit the mutable version. This can be implemented as a non-breaking update

Examples of places where it's **not** safe to use the mutable api are exprs in which we will reuse the values:
```
var {
  number $e1 // e1 is used later on..
  number $e2 = $e1 + $e1 // thus we cannot mutate $e1
}

send [USD/2 $e1] ( // <- $e1 is reused here
  ..
)
```

Examples of places where it's safe to use the mutable api are exprs in which we "own" lx and rx value:
```
var {
  number $e1
  number $e2
  number $e3 = $e1 + $e2
  // assume $e1 and $e2 are never reused again
}
```
This can become:
```
FETCH_VAR<number> $e1
FETCH_VAR<number> $e2
ADD_MUT
```

Note: it's not always safe to treat literals as "owned", as they are owned by the data segment

**TODO** reason about static analysis
