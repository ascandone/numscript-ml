## Setup

Install deps:
```bash
opam install . --deps-only --with-test --with-doc --with-dev-setup
```

Build:
```bash
dune build
dune build -w # <- in watch mode
```

Test:
```bash
dune test
dune test -w # <- in watch mode
```
