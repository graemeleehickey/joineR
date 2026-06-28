# Changelog

## joineR 1.2.9

### Bug fixes

- `emUpdate()` and `emUpdateCR()` now issue a
  [`warning()`](https://rdrr.io/r/base/warning.html) on non-convergence
  instead of silently calling
  [`print()`](https://rdrr.io/r/base/print.html), making it possible for
  callers to catch non-convergence programmatically.

- `survst()` no longer unconditionally forces the first subject’s
  censoring indicator to 1 before passing data to
  [`coxph()`](https://rdrr.io/pkg/survival/man/coxph.html). A proper
  [`stop()`](https://rdrr.io/r/base/stop.html) is raised if no events
  are observed. The `rs` index vector is now computed correctly via
  [`findInterval()`](https://rdrr.io/r/base/findInterval.html), clamping
  subjects who precede the first event time to risk set 1 rather than
  producing `NA`.

- `survst()` and `survstCR()` now correctly index `loglik[2]` (the
  fitted model log-likelihood) rather than storing the full length-2
  vector in `$log.like`.

- Bootstrap confidence intervals in
  [`jointSE()`](https://graemeleehickey.github.io/joineR/reference/jointSE.md)
  now use [`quantile()`](https://rdrr.io/r/stats/quantile.html) instead
  of a manual sorted-index lookup, fixing an off-by-one error for small
  `n.boot` values.

### Improvements

- [`joint()`](https://graemeleehickey.github.io/joineR/reference/joint.md)
  and
  [`jointSE()`](https://graemeleehickey.github.io/joineR/reference/jointSE.md)
  arguments `gpt`, `lgpt`, `max.it`, and `tol` now have explicit default
  values in the function signatures, replacing the previous
  [`missing()`](https://rdrr.io/r/base/missing.html) pattern. Defaults
  are visible in auto-complete and
  [`?joint`](https://graemeleehickey.github.io/joineR/reference/joint.md).

- [`jointSE()`](https://graemeleehickey.github.io/joineR/reference/jointSE.md)
  output now includes a `p-value` column containing two-sided Wald
  p-values for all fixed-effect and association parameters. Variance
  component rows (`U_*`, `Residual`) return `NA` as Wald tests are not
  appropriate for positive-constrained parameters. Closes
  [\#58](https://github.com/graemeleehickey/joineR/issues/58).

- [`jointSE()`](https://graemeleehickey.github.io/joineR/reference/jointSE.md)
  confidence intervals are now computed for all values of `n.boot`.
  Previously, runs with fewer than 100 bootstrap samples silently
  returned `0` for both CI bounds; now the empirical percentile CIs are
  always returned and a
  [`warning()`](https://rdrr.io/r/base/warning.html) is issued when
  `n.boot < 100` to alert the user that the intervals may be unreliable.

- [`tidy()`](https://generics.r-lib.org/reference/tidy.html) and
  [`glance()`](https://generics.r-lib.org/reference/glance.html) methods
  are now available for `joint` objects, providing a tidy -style
  interface. [`tidy()`](https://generics.r-lib.org/reference/tidy.html)
  returns a of parameter estimates (one row per term) and optionally
  accepts a result to add standard errors, Wald statistics, p-values,
  and confidence intervals.
  [`glance()`](https://generics.r-lib.org/reference/glance.html) returns
  a single-row summary of model-level statistics including
  log-likelihood, AIC, BIC, number of observations, number of subjects,
  convergence status, and EM iteration count. Closes
  [\#59](https://github.com/graemeleehickey/joineR/issues/59).

- [`simjoint()`](https://graemeleehickey.github.io/joineR/reference/simjoint.md)
  no longer prints a progress line via
  [`cat()`](https://rdrr.io/r/base/cat.html) from the internal
  `simdat()` function. It now emits a
  [`message()`](https://rdrr.io/r/base/message.html) at the
  [`simjoint()`](https://graemeleehickey.github.io/joineR/reference/simjoint.md)
  level, which can be suppressed with
  [`suppressMessages()`](https://rdrr.io/r/base/message.html).

- `survival` has been moved from `Depends` to `Imports`. `Surv` is
  re-exported so existing user code does not require changes.

- Test coverage improved from 89% to 93%.

- Code formatted with `air`.

### Housekeeping

- Added a `pkgdown` site with Bootstrap 5, full dark mode (light
  switch), and a structured reference index. A `pkgdown.yaml` GitHub
  Actions workflow builds and deploys the site to GitHub Pages on push
  and release.

- Added a `render-readme.yaml` GitHub Actions workflow that
  automatically re-renders `README.md` from `README.Rmd` when
  `README.Rmd` is changed.

- Updated `R-CMD-check.yaml` and `test-coverage.yaml` GitHub Actions
  workflows to use `actions/checkout@v6`, `codecov-action@v7`, and
  `upload-artifact@v7`.

- `DESCRIPTION` URL field now includes the pkgdown site, CRAN page, and
  GitHub repository.

- README badges updated: removed defunct AppVeyor and Depsy badges;
  added pkgdown deployment badge and lifecycle (stable) badge.

- Updated README with new MRC logo and fixed badges.

- Fixed deprecated documentation for `joineR-package.R`.

- Added reverse dependency checks.

- Removed dead commented-out code blocks in `survst.R` and
  `prepSurvData.R`.

- Removed no-op `fd <- fd` / `sd <- sd` assignments in `emUpdate()`.

- The internal helper `sortJointData()` (previously `sort.dat()` defined
  inside
  [`joint()`](https://graemeleehickey.github.io/joineR/reference/joint.md)
  on every call) is now a file-level internal function, avoiding a
  spurious S3 method consistency note in R CMD check.

- Fixed copy-paste error in `@param ylab` documentation for
  [`jointplot()`](https://graemeleehickey.github.io/joineR/reference/jointplot.md)
  (was labelled “x-axis”).

- Renamed variable `t` to `obs_time` in
  [`jointplot()`](https://graemeleehickey.github.io/joineR/reference/jointplot.md)
  to avoid shadowing [`base::t()`](https://rdrr.io/r/base/t.html).

- Fixed typo “am Expectation Maximization” → “an” in `DESCRIPTION`.

- Updated test suite: removed deprecated `context()` calls and replaced
  `expect_is()` with `expect_s3_class()` throughout. Fixed typo
  “seperate” → “separate” in one test name. Enabled testthat edition 3.

## joineR 1.2.8

CRAN release: 2023-01-22

### Bugs

- Fixes CRAN CMD check when run using ATLAS (alternative BLAS/LAPACK
  implementation). Issue was in the tests.

## joineR 1.2.7

CRAN release: 2023-01-18

### Bugs

- Fixed bug for case when all subjects experience events.

- Change to
  [`survival::basehaz`](https://rdrr.io/pkg/survival/man/basehaz.html)
  arguments.

- Fixed a `NOTE` on checking for class type.s

### Housekeeping

- Edited `NEWS` v1.1 entry.

## joineR 1.2.6

CRAN release: 2021-06-01

### Bugs

- Fixed bug for covariates in competing risks models.

### Housekeeping

- Changed package maintainer to Graeme Hickey.

## joineR 1.2.5

CRAN release: 2020-02-08

### Bugs

- Fixed error when ID column was not the first column in the dataset.

## joineR 1.2.4

CRAN release: 2018-05-17

### Housekeeping

- Added Zenodo badge to README.

- Added further ORCID IDs for authors.

- Changed package maintainer to Pete Philipson.

## joineR 1.2.3

CRAN release: 2018-02-06

### Bugs

- Fixed errors when subjects IDs were `character` format.

### Housekeeping

- Added ORCID IDs for authors.

- Added Depsy badge to README.

## joineR 1.2.2

CRAN release: 2017-09-14

### Minor update

- Modified the control parameters for the call to
  [`nlme::lme()`](https://rdrr.io/pkg/nlme/man/lme.html) that is used to
  generate initial parameter estimates. In some bootstrap settings, this
  was throwing an error, leading to the entire bootstrap run to cease.

### Housekeeping

- Added hex sticker badge.

## joineR 1.2.1

CRAN release: 2017-07-24

### Minor updates

- Added Rd file for `joint.object` to describe what is contained in an
  object of class `joint`.

- Added the ubiquitous `aids` dataset for teaching purposes.

### Bugs

Fixed an error in the `liver` dataset.

### Maintanence

- Updated the documentation for the datasets.

- Added `ByteCompile: true` to the DESCRIPTION.

## joineR 1.2.0

CRAN release: 2017-05-19

### Major updates

- `joint` now allows for competing risks data (2 failure types) as per
  the model developed by Williamson et al. (2008). Other functions have
  been upgraded to handle the competing risks data.

- A second vignette for the competing risks model is available.

### Minor updates

- R version 3.4.0 deprecated the recycling of 1x1 matrices, and led to
  warnings being thrown. This is now fixed.

### Maintanence

- General code and documentation tidy-up.

- New unit tests added to increase code coverage.

## joineR 1.1.0

CRAN release: 2017-02-02

### Minor updates

- Add `simjoint` function to simulate data from joint models with
  several types of association structures.

- Removed `jlike` function and integrated likelihood calculation
  directly into `em.alg` function.

- Minor bug fixes to the `joint` and `em.alg` functions.

### Maintanence

- Added a `NEWS.md` file to track changes to the package.

- Added some unit tests + code coverage monitoring integration.

- Converted Rd files to roxygen.

- Converted Sweave vignette to rmarkdown.

- Updates to vignette and documentation.

- Minor updates to `DESCRIPTION` and `NAMESPACE` to pass R CMD checks,
  provide additional information, and removed dependency of `boot` and
  `gdata` packages.

- Added a `README.Rmd`.

- Added project to GitHub with integrated Travis CI and appveyor.

## joineR 1.1

- First version of software with subsequent minor patches. No NEWS file
  was maintained prior to version 1.1.
