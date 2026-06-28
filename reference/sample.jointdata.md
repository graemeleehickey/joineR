# Sample from a `jointdata` x

Generic function used to sampling a subset of data from an x of class
`jointdata`, with a specific size of number of subjects.

## Usage

``` r
sample.jointdata(x, size, replace = FALSE)
```

## Arguments

- x:

  an object of class `jointdata`.

- size:

  number of subjects to include in the sampled subset.

- replace:

  should sampling be with replacement? Default is `replace = TRUE`.

## Value

An object of class `jointdata`, with data only on the subjects sampled.

## See also

[`sample`](https://rdrr.io/r/base/sample.html),
[`jointdata`](https://graemeleehickey.github.io/joineR/reference/jointdata.md),
[`UniqueVariables`](https://graemeleehickey.github.io/joineR/reference/UniqueVariables.md).

## Author

Ines Sousa

## Examples

``` r
data(heart.valve)
heart.surv <- UniqueVariables(heart.valve, 
                              var.col = c("fuyrs", "status"), 
                              id.col = "num")
heart.valve.jd <- jointdata(survival = heart.surv, 
                            id.col = "num", 
                            time.col = "time")
sample.jointdata(heart.valve.jd, size = 10)
#> $subject
#>  [1]  16  32  33  43  57  87 107 143 212 219
#> 
#> $longitudinal
#> [1] NA
#> 
#> $survival
#>    num    fuyrs status
#> 1   16 3.191781      0
#> 2   32 7.884932      0
#> 3   33 4.210959      0
#> 4   43 4.356164      0
#> 5   57 7.465753      0
#> 6   87 6.854795      1
#> 7  107 6.641096      0
#> 8  143 5.424658      1
#> 9  212 2.928767      0
#> 10 219 1.726027      0
#> 
#> $baseline
#> [1] NA
#> 
#> $time.col
#> [1] NA
#> 
#> $subj.col
#> [1] "num"
#> 
#> attr(,"class")
#> [1] "jointdata" "list"     
```
