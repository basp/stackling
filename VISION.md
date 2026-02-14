# **Stackling — Vision & Roadmap**

Stackling is inspired by *Joy*, but it is **not** a strict Joy implementation.  
We aim to stay faithful to Joy’s core ideas — concatenative programming, quotations, and algebraic reasoning — while modernizing the language and making it more usable, transparent, and enjoyable.

Stackling is a *Joy sibling*, not a clone.

---

# 🌱 **Vision**

Joy is elegant and mathematically profound, but its reference implementation is difficult to debug, opaque in its error reporting, and not designed for practical programming. Stackling’s mission is to bring Joy’s ideas into the modern world while preserving its conceptual beauty.

Stackling should be:

- **Fun** — no deadlines, no pressure, no stakeholders.  
  We move in *tiny, deliberate steps* and enjoy the process.

- **Correct** — a Joy‑like core with **clear, predictable semantics**.  
  Any divergence from Joy is intentional and documented.

- **Educational** — an interpreter written in **idiomatic F#**, prioritizing readability over performance.  
  Trade‑offs are explicit and thoughtful.

- **Transparent** — the interpreter is a **glass box**, not a black box.  
  We want:
    - stepping
    - backstepping
    - full runtime traces
    - clear snapshots of state evolution

- **Usable** — Stackling should be pleasant to interact with:
    - via the F# API
    - eventually via a REPL
    - possibly via a compiler in the future

- **Helpful with errors** — Stackling should provide:
    - meaningful runtime errors
    - trace‑based context
    - (later) source locations from parsed input

---

# 🌿 **Roadmap**

This roadmap is intentionally flexible.  
It’s a guide, not a contract — Stackling grows at the pace that feels right.

---

## **Phase 1 — A Trustworthy Core Interpreter**
*(Current phase)*

### Goals
- Minimal Joy‑like interpreter
- Strong test suite
- Deterministic stepping
- Transparent runtime state
- Clear error model
- Stable runtime view abstraction

### Deliverables
- Stack, queue, environment
- Builtins (`dup`, `swap`, arithmetic, comparisons, etc.)
- User‑defined words
- Trace entries with before/after snapshots
- `RuntimeStateView` for stable testing
- `step` and `runUntilHalt`
- Error types with trace info

### Remaining tasks
- Normalize the runtime view
- Add trace ordering tests
- Add nested expansion tests
- Add more builtins (slowly, deliberately)

---

## **Phase 2 — Quotations & Higher‑Order Joy**

### Goals
- First‑class quotations
- Quotation execution
- Basic combinators

### Deliverables
- Quotation syntax
- `i` (execute quotation)
- `dip`, `map`, `fold`, `ifte`
- Trace entries that show quotation boundaries

---

## **Phase 3 — Parsing & Syntax Errors**

### Goals
- Parse Joy‑like syntax
- Associate source locations
- Improve error reporting

### Deliverables
- Parser (FParsec or hand‑rolled)
- Syntax error type
- Runtime errors enriched with source spans
- Pretty error messages

---

## **Phase 4 — Debugging & Transparency Tools**

### Goals
- Step forward/backward
- Inspect runtime state
- Pretty‑printed traces
- Optional breakpoints

### Deliverables
- Reverse stepping via trace replay
- Human‑friendly trace printer
- Debugger‑like stepping API

---

## **Phase 5 — REPL & Interactive Environment**

### Goals
- Interactive REPL
- Load files
- Define words interactively
- Inspect environment

### Deliverables
- REPL loop
- Pretty printing
- Error reporting
- History, multiline input

---

## **Phase 6 — Optional: Compilation & Optimization**

### Goals
- Bytecode or IL compilation
- Optimized execution
- Static analysis

### Deliverables
- Compiler pipeline
- Optimizer passes
- Optional type inference or effect tracking

---

# 🌟 Closing Thoughts

Stackling is a slow, deliberate, joyful exploration of concatenative programming.  
It’s a language, a teaching tool, and a playground for ideas — built with clarity, transparency, and fun at its core.
