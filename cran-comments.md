## Submission notes

This is a patch release (1.2.9) containing several bug fixes, new features,
and housekeeping changes since 1.2.8. Key changes include:

* Bug fixes to `survst()` (data mutation in initialisation), `jointSE()` (CI
  off-by-one, zeros returned for small `n.boot`), and `emUpdate()`/`emUpdateCR()`
  (non-convergence was silently printed rather than warned).
* New `tidy()` and `glance()` methods for `joint` objects (via `generics`).
* New `p-value` column in `jointSE()` output (two-sided Wald test).
* `survival` moved from `Depends` to `Imports`; `Surv` re-exported.
* New dependency: `generics` (added to `Imports`).

## Test environments

* Local macOS (Sequoia 15.5), R 4.5.3
* Ubuntu 24.04 (via GitHub Actions), R release and devel
* macOS latest (via GitHub Actions), R release
* Windows latest (via GitHub Actions), R release

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

Checked 1 reverse dependency (joineRML) using `revdepcheck::revdep_check()`.

* 0 new problems
* 0 packages failed to check
