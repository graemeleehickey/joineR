# joineR 2.0.0

## Major updates

* `joint` now allows for competing risks data (2 failure types) as per the model
developed by Williamson et al. (2008). Other functions have been upgraded to
handle the competing risks data.

* A second vignette for the competing risks model is available.

## Maintanence

* General code and documentation tidy-up.

* New unit tests added to increase code coverage.

# joineR 1.1.0

## Minor updates

* Add `simjoint` function to simulate data from joint models with several types
of association structures.

* Removed `jlike` function and integrated likelihood calculation directly into
`em.alg` function.

* Minor bug fixes to the `joint` and `em.alg` functions.

## Maintanence

* Added a `NEWS.md` file to track changes to the package.

* Added some unit tests + code coverage monitoring integration.

* Converted Rd files to roxygen.

* Converted Sweave vignette to rmarkdown.

* Updates to vignette and documentation.

* Minor updates to `DESCRIPTION` and `NAMESPACE` to pass R CMD checks, provide
additional information, and removed dependency of `boot` and `gdata` packages.

* Added a `README.Rmd`.

* Added project to GitHub with integrated Travis CI and appveyor.

# joineR <1.1

* First version of software with subsequent minor patches. No NEWS file was
maintained prior to version 1.1



