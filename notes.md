## Encoding
TODO

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
