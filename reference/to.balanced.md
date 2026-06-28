# Transform data to the longitudinal balanced format

Transforms a longitudinal data set in the unbalanced format to the
balanced format.

## Usage

``` r
to.balanced(data, id.col, time.col, Y.col, other.col = NA)
```

## Arguments

- data:

  a data frame with longitudinal data in the unbalanced format. That is,
  in the format of 'one row per observation'.

- id.col:

  a column number, or column name, in the data frame `data`, where the
  patient identifier is located.

- time.col:

  a column number, or column name, in the data frame `data`, where the
  time measurements are.

- Y.col:

  a vector of column numbers, or column names, of longitudinal
  variables, and/or time dependent covariates in the data frame `data`.

- other.col:

  a vector of column numbers, or column names, of baseline covariates,
  and/or other subject level data, as for example, survival data.
  Default does not include `other.col`.

## Value

A data frame with longitudinal data in the balanced format. The balanced
format is considered in this context as the format where each row has
data on each subject. Notice that in this format we will have multiple
columns for the same longitudinal variable, each corresponding to the
variable observed at each time point.

## See also

[`to.unbalanced`](https://graemeleehickey.github.io/joineR/reference/to.unbalanced.md).

## Author

Ines Sousa

## Examples

``` r
simul <- data.frame(num = 1:10,
                    Y1.1 = rnorm(10), Y1.2 = rnorm(10),
                    Y2.1 = rnorm(10), Y2.2 = rnorm(10),
                    age = rnorm(10))
simul <- to.unbalanced(simul, id.col = 1, times = c(1, 2),
                       Y.col = 2:5, other.col = 6)
simul <- to.balanced(simul, id.col = "num", time.col = "time",
                     Y.col = c("Y1.1", "Y2.1"), other.col = "age")
```
