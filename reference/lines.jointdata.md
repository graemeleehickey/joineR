# Add lines to an existing `jointdata` plot

Add lines to an existing plot of an object of class `jointdata`, for a
longitudinal variable. It is possible to plot all the subjects in the
data set, or just a selected `subset`. See
[`subset.jointdata`](https://graemeleehickey.github.io/joineR/reference/subset.jointdata.md).

## Usage

``` r
# S3 method for class 'jointdata'
lines(x, Y.col, ...)
```

## Arguments

- x:

  object of class `jointdata`.

- Y.col:

  column number, or column name, of longitudinal variable to be plotted.
  Defaults to `Y.col = NA`, plotting all longitudinal variables.

- ...:

  other graphical arguments; see
  [`plot`](https://rdrr.io/r/graphics/plot.default.html).

## Value

A graphical device with a plot for longitudinal data.

## See also

Other functions are useful to be used with this such as
[`plot`](https://rdrr.io/r/graphics/plot.default.html) and
[`points`](https://rdrr.io/r/graphics/points.html).

## Author

Ines Sousa

## Examples

``` r
data(heart.valve)
heart.surv <- UniqueVariables(heart.valve,
                              var.col = c("fuyrs", "status"),
                              id.col = "num")
heart.long <- heart.valve[, c(1, 4, 5, 7, 8, 9, 10, 11)]
heart.jd <- jointdata(longitudinal = heart.long,
                      survival = heart.surv,
                      id.col = "num",
                      time.col = "time")

# Randomly select a pair of subjects to plot profiles of
take <- sample(1:max(heart.jd$survival$num), 2)
heart.jd.1 <- subset(heart.jd, take[1])
heart.jd.2 <- subset(heart.jd, take[2])

plot(heart.jd.1, Y.col = 4)
lines(heart.jd.2, Y.col = 4, lty = 2)
```
