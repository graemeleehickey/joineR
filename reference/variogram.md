# Empirical variogram for longitudinal data

Calculates the variogram for observed measurements, with two components,
the total variability in the data, and the variogram for all time lags
in all individuals.

## Usage

``` r
variogram(indv, time, Y)
```

## Arguments

- indv:

  vector of individual identification, as in the longitudinal data,
  repeated for each time point.

- time:

  vector of observation time, as in the longitudinal data.

- Y:

  vector of observed measurements. This can be a vector of longitudinal
  data, or residuals after fitting a model for the mean response.

## Value

An object of class `vargm` and `list` with two elements. The first
`svar` is a matrix with columns for all values \\(u_ijk,v_ijk)\\, and
the second `sigma2` is the total variability in the data.

## Details

The empirical variogram in this function is calculated from observed
half-squared-differences between pairs of measurements, \\v_ijk = 0.5 \*
(r_ij-r_ik)^2\\ and the corresponding time differences
\\u_ijk=t_ij-t_ik\\. The variogram is plotted for averages of each time
lag for the \\v_ijk\\ for all \\i\\.

## Note

There is a function
[`plot.vargm`](https://graemeleehickey.github.io/joineR/reference/plot.vargm.md)
which should be used to plot the empirical variogram.

## Author

Ines Sousa

## Examples

``` r
data(mental)
mental.unbalanced <- to.unbalanced(mental, id.col = 1,
                                   times = c(0, 1, 2, 4, 6, 8),
                                   Y.col = 2:7,
                                   other.col = c(8, 10, 11))
names(mental.unbalanced)[3] <- "Y"

vgm <- variogram(indv = tail(mental.unbalanced[, 1], 30),
                 time = tail(mental.unbalanced[, 2], 30),
                 Y = tail(mental.unbalanced[, 3], 30))
```
