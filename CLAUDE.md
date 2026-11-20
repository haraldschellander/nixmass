# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working
with code in this repository.

## What this is

`nixmass` is a CRAN R package that models snow water equivalent (SWE)
from daily snow depth (`hs`, meters) series. It offers process-based
models (delta.snow, HS2SWE) and empirical regression models (jo09, pi16,
st10, gu19). Docs site: <https://haraldschellander.github.io/nixmass/>
(built by pkgdown).

## Commands

Run from the package root in R (devtools/testthat workflow; no
Makefile):

``` r

devtools::document()                         # regenerate man/ and NAMESPACE from roxygen
devtools::load_all()                          # load package for interactive work
devtools::test()                              # run all tests
testthat::test_file("tests/testthat/test-swe.delta.snow.R")   # single test file
devtools::test(filter = "hs2swe")             # tests whose file name matches
devtools::check()                             # full R CMD check (CRAN-like)
spelling::spell_check_package()               # spelling (also run via tests/spelling.R)
```

Shell equivalents:
`R CMD build . && R CMD check --as-cran nixmass_*.tar.gz`.

## Architecture

- **[`nixmass()`](https://haraldschellander.github.io/nixmass/reference/nixmass.md)**
  ([R/nixmass.R](https://haraldschellander.github.io/nixmass/R/nixmass.R))
  is the user-facing wrapper. It takes a `data.frame` with `date` and
  `hs` columns, dispatches to one or more model functions by name
  (`delta.snow`, `delta.snow.dyn_rho_max`, `hs2swe`, `jo09`, `pi16`,
  `st10`, `gu19`), and returns an S3 object of class `nixmass` — a list
  with `swe` (named list, one entry per model), `date`, `hs`.
  `summary.nixmass` and `plot.nixmass` (ggplot2-based; `density = TRUE`
  requires a single delta.snow model run with `layers = TRUE`) live in
  the same file. Model-specific required args (`alt`/`region.jo09`,
  `snowclass.st10`, `region.gu19`) are validated here.
- **Each model is its own exported function in its own file**
  (`swe.delta.snow`, `hs2swe`, `swe.jo09`, `swe.pi16`, `swe.st10`,
  `swe.gu19`) and can be called directly. Adding a model means: new
  `R/<model>.R`, a branch in the
  [`nixmass()`](https://haraldschellander.github.io/nixmass/reference/nixmass.md)
  dispatch loop, the name in the `model` argument choices, and roxygen
  `@export`.
- **`swe.delta.snow`**
  ([R/swe.delta.snow.R](https://haraldschellander.github.io/nixmass/R/swe.delta.snow.R))
  is the core and largest model. It is a layer-based simulation: new
  snow creates layers; helper closures defined inside the function
  (`dry_compaction`, `assignH`, `drenchH`, `scaleH`, `rho_max_dyn`)
  handle compaction, overburden, and melt. Parameters come from a
  defaults list merged with the user’s `model_opts` via
  [`utils::modifyList`](https://rdrr.io/r/utils/modifyList.html);
  defaults differ depending on `dyn_rho_max` (age-dependent maximum
  density, Werkgarner 2026 calibration). Note the function default is
  `dyn_rho_max = TRUE`, whereas `nixmass(model = "delta.snow")` passes
  `FALSE`. `layers = TRUE` returns per-layer `h`/`swe`/`age` matrices
  (rows = layers, sized to the max layer count of any single season).
  `strict_mode` controls checks for strictly regular dates and a series
  starting at zero.
- **[R/swe.delta.snow.para.R](https://haraldschellander.github.io/nixmass/R/swe.delta.snow.para.R)**
  is an experimental parallel/season-segmented variant
  (`swe.delta.snow.para`, with `.delta_snow_core`). It duplicates the
  delta.snow core logic and is **excluded from the build via
  `.Rbuildignore`** — it is not part of the shipped package. Keep model
  changes in `swe.delta.snow.R` in sync with it only if intended.
- Package-wide imports are declared via roxygen tags at the top of
  `R/nixmass.R`; `NAMESPACE` and `man/` are generated — do not edit by
  hand.
- Example dataset `hsdata` (`data/hsdata.RData`, documented in
  `R/nixmass.R`) is used throughout examples and tests.

## Conventions

- Input validation errors use
  [`stop()`](https://rdrr.io/r/base/stop.html) with specific messages
  that tests match via `expect_error(..., "<message>")` — keep message
  text in sync with tests.
- Performance matters (v1.4.0 vectorised the delta.snow inner loops and
  removed `<<-`); avoid reintroducing superassignment or per-layer
  loops.
- Record user-visible changes in `NEWS` (plain-text format,
  `nixmass vX.Y.Z (release date: ...)` headers) and bump
  `Version`/`Date` in `DESCRIPTION`.
- `koi/` and `.github/workflows/koi.yml` are a cosmetic daily README
  GIF; the bot’s “koi: draw the day’s koi” commits are unrelated to
  package code.
