# Contributing to ggchangepoint

Thanks for helping. The most useful contributions are a bug report with a
reprex, a new engine, and a measurement that shows a claim in the docs is
wrong. This page covers all three.

## 🐛 Reporting a bug

Open an issue with the bug template. Include a
[reprex](https://reprex.tidyverse.org), `packageVersion("ggchangepoint")`
and the engine's version (`fit$versions` on any result shows both). If the
answer is wrong rather than an error, say what you expected and why: a
wrong answer that runs is the kind of bug this package most needs to hear
about.

## 🔌 Adding an engine

Every wrapper has the same five steps, and the registry does the rest.

| Step | Where |
| :- | :- |
| 1. Check the input and load the engine | `validate_data(x)`, then `need_pkg("engine")` |
| 2. Call the engine | inside the wrapper; `cpt_detect()` adds the engine-error handling |
| 3. Extract the changepoints as the **last observation before each change** | convert if the engine reports the first observation after (`"right"`) |
| 4. Build the result | `ggcpt_build(x, cp, method =, change_in =, penalty =, fit =, call = match.call())` |
| 5. Register it | a row in `builtin_registry_core()` in `R/registry.R` |

`Rscript data-raw/use_cpt_wrapper.R <method> <package> <function>` writes a
skeleton wrapper, a test file and the registry row to paste.

Then fill in what the registry derives everything else from:

- **Capabilities**: the flags in the registry row (`ci`, `fitted`,
  `posterior`, `statistic`, `path`, ...). `cpt_methods()` shows them, and a
  flag that is `TRUE` must be backed by a column in the result.
- **Vocabulary** (`R/vocabulary.R`): the families it can fit
  (`family_rows_explicit()`), its modelling-choice arguments and their
  legal values (`method_choices()`), the argument a minimum segment length
  becomes (`min_segment_args()`), and its upstream changepoint convention
  when it is not `"left"` (`upstream_conventions()`).
- **Citation**: a row in `cpt_references()` (`R/cite.R`), which is what
  `cpt_cite()` prints, and a BibTeX entry in `inst/REFERENCES.bib` for the
  wrapper's help page (`\insertRef{}`).
- **Measurements**: run the engine through `data-raw/na_handling.R` (what it
  does with a missing value) and `data-raw/measure_engines.R` (runtime,
  invariances, noise regimes, data types, null size), then
  `data-raw/build_measured_data.R`. The measured columns of
  `cpt_methods()` and `cpt_recommend()`'s scores come from those tables,
  never from a hand-written list.

The convention test, the change-type contract sweep and the boundary-input
matrix in `tests/testthat/` run every registered engine automatically, so a
new wrapper is checked against the contract the moment its row exists.

## ✍️ Code conventions

- **Errors and warnings are classed.** Use `cpt_abort(..., class = )` and
  `cpt_warn(..., class = )`, never `stop()` or `warning()`: the classes in
  `?ggchangepoint-conditions` are API. A meta-test enforces it.
- **Choices are validated by name.** `cpt_match_arg()`, which suggests the
  nearest valid value, not `match.arg()`.
- **Comments say why.** The code says what. A guard that exists because an
  engine misbehaved on some input names the input and what it did.
- **A slow test** (0.2 seconds or more) starts with `skip_on_cran()`; one
  that runs an engine checks `engine_usable()` first.

## ✅ Before a pull request

```sh
Rscript -e 'devtools::document()'
NOT_CRAN=true Rscript -e 'devtools::test()'
R CMD build . && R CMD check --as-cran ggchangepoint_*.tar.gz
```

Add a line to `NEWS.md` under the development version.

## 📜 Stability

Anything in the contract (exported arguments, result slots, condition
classes, the JSON schema) changes only through the deprecation cycle in the
[stability article](https://pursuitofdatascience.github.io/ggchangepoint/articles/stability.html).
