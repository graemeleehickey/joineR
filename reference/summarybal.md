# Summary of a balanced longitudinal data set

For a balanced longitudinal data set a vector of the mean response and
variances at defined time points is returned along with the correlation
matrix of the responses across the time points.

## Usage

``` r
summarybal(object, Y.col, times, use = "all.obs", na.rm, ...)
```

## Arguments

- object:

  a longitudinal data set in the balanced format.

- Y.col:

  the column numbers of the longitudinal measurements at each design
  time point in the `object`. This does not have to be all of the
  longitudinal measurements taken and may be a subset instead.

- times:

  a vector of unique time points of the longitudinal measurements. This
  does not have to be all of the study time points and may be a subset
  instead, but should match the columns defined in `Y.col`.

- use:

  an optional character string giving a method for computing covariances
  in the presence of missing values. This must be (an abbreviation of)
  one of the strings `"all.obs"`, `"complete.obs"` or
  `"pairwise.complete.obs"`. Defaults to `use = "all.obs"`.

- na.rm:

  logical. Should missing values be removed? By default,
  `na.rm = FALSE`.

- ...:

  further arguments for the summary.

## Value

A `list` with three elements:

- `mean.vect`:

  a matrix with the time points in the first column and the mean
  response vector as the second column.

- `variance`:

  The vector of variances for the response at the time points.

- `cor.mtx`:

  Containing the correlation matrix of the responses between each pair
  of time points.

## See also

[`to.balanced`](https://graemeleehickey.github.io/joineR/reference/to.balanced.md).

## Author

Ines Sousa

## Examples

``` r
data(mental)
summarybal(mental, Y.col = 2:7, times = c(0, 1, 2, 4, 6, 8), na.rm = TRUE)
#> $mean.vect
#>      x        y
#> Y.t0 0 55.77333
#> Y.t1 1 53.03378
#> Y.t2 2 50.37008
#> Y.t4 4 48.62963
#> Y.t6 6 46.94048
#> Y.t8 8 46.02941
#> 
#> $variance
#>     Y.t0     Y.t1     Y.t2     Y.t4     Y.t6     Y.t8 
#> 132.1228 152.7403 167.5366 168.6653 228.8759 181.4917 
#> 
#> $cor.mtx
#>           [,1]      [,2]      [,3]      [,4]      [,5]      [,6]
#> [1,] 1.0000000 0.5939982 0.4537496 0.4186798 0.3432355 0.2997679
#> [2,] 0.5939982 1.0000000 0.7793311 0.6805021 0.6631543 0.6149448
#> [3,] 0.4537496 0.7793311 1.0000000 0.7992470 0.7077238 0.6202277
#> [4,] 0.4186798 0.6805021 0.7992470 1.0000000 0.8108585 0.7402240
#> [5,] 0.3432355 0.6631543 0.7077238 0.8108585 1.0000000 0.8681933
#> [6,] 0.2997679 0.6149448 0.6202277 0.7402240 0.8681933 1.0000000
#> 
```
