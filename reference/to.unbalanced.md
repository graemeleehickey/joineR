# Transform data to the longitudinal unbalanced format

Transforms a longitudinal data set in the balanced format to the
unbalanced format.

## Usage

``` r
to.unbalanced(data, id.col, times, Y.col, other.col = NA)
```

## Arguments

- data:

  a data frame with longitudinal data in the balanced format. That is,
  in the format of 'one row per subject'. `data`, where the patient
  identifications is.

- id.col:

  a column number, or column name, in the data frame `data`, where the
  patient identifier is located.

- times:

  a vector with the unique time points where the patients are observed.
  This is the study design time points in a balanced data set.

- Y.col:

  a vector of column numbers, or column names, of longitudinal
  variables, and/or time dependent covariates in the data frame `data`.

- other.col:

  a vector of column numbers, or column names, of baseline covariates,
  and/or other subject level data, as for example, survival data.
  Default does not include `other.col`.

## Value

A data frame with longitudinal data in the unbalanced format. The
unbalanced format is considered in this context as the format where each
row has data on each subject observation.

## See also

[`to.balanced`](https://graemeleehickey.github.io/joineR/reference/to.balanced.md).

## Author

Ines Sousa

## Examples

``` r
simul <- data.frame(num = 1:10,
                    Y1.1 = rnorm(10), Y1.2 = rnorm(10),
                    Y2.1 = rnorm(10), Y2.2 = rnorm(10),
                    age = rnorm(10))
to.unbalanced(simul, id.col = 1, times = c(1, 2), Y.col = 2:5,
              other.col = 6)
#>    num time        Y1.1        Y2.1        age
#> 1    1    1 -0.10862698  0.39210595  0.1236927
#> 2    1    2  0.59756562  0.28010003  0.1236927
#> 3    2    1  1.82778651  0.46993317 -1.0955924
#> 4    2    2  0.12784049  0.33030835 -1.0955924
#> 5    3    1 -1.96191773  0.98884288  0.3883423
#> 6    3    2 -1.71687188  0.28475162  0.3883423
#> 7    4    1  0.94598280 -0.71372785 -0.7550303
#> 8    4    2  0.03610956  0.42859440 -0.7550303
#> 9    5    1  0.54896207  1.26969025 -1.2316026
#> 10   5    2  1.00726777 -0.12096736 -1.2316026
#> 11   6    1 -0.57746532  0.70906596  0.7642656
#> 12   6    2  0.71310922  0.45824225  0.7642656
#> 13   7    1 -1.22828491  1.04692267 -0.6458052
#> 14   7    2 -1.10996215  0.61653467 -0.6458052
#> 15   8    1  1.03953209 -1.21047936 -1.0419806
#> 16   8    2  0.14940570 -0.17504877 -1.0419806
#> 17   9    1  0.45560201  0.38014950 -0.8372059
#> 18   9    2 -0.38614273 -0.12283134 -0.8372059
#> 19  10    1 -2.37992517 -0.06738627  1.0263532
#> 20  10    2 -0.23049422 -1.47269150  1.0263532
```
