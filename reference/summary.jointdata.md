# Summarise a `jointdata` object

Generic function used to produce summaries of objects of class
`jointdata`.

## Usage

``` r
# S3 method for class 'jointdata'
summary(object, ...)
```

## Arguments

- object:

  an object of class `joint`.

- ...:

  further arguments for the summary.

## Value

A list with five elements. Each summarises an element of the `jointdata`
object:

- `subjects`:

  Gives the number of subjects in the data set.

- `longitudinal`:

  If longitudinal data is available, it gives the names and class, of
  the longitudinal variables.

- `survival`:

  If survival data is available, it gives the number of subjects with
  failure and censored survival times.

- `baseline`:

  If baseline covariates is available, it gives the names and class, of
  the baseline covariates.

- `times`:

  If longitudinal data is available, it gives the unique longitudinal
  time measurements, if it is a balanced study. In case of unbalanced
  study, it will only state it is an unbalanced study.

## See also

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
summary(heart.valve.jd)
#> $subjects
#> [1] "Number of subjects: 256"
#> 
#> $longitudinal
#> [1] "No longitudinal data available"
#> 
#> $survival
#>                                  
#> Number of subjects that fail:  54
#> Number of subjects censored:  202
#> 
#> $baseline
#> [1] "No baseline covariates data available"
#> 
#> $times
#> [1] "No longitudinal data available"
#> 
```
