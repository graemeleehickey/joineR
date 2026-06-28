# Subsetting object of class `jointdata`

Returns an object of class `jointdata` which is a subset of an original
object of class `jointdata`.

## Usage

``` r
# S3 method for class 'jointdata'
subset(x, subj.subset, ...)
```

## Arguments

- x:

  an object of class `jointdata`.

- subj.subset:

  vector of subject identifiers, to include in the data subset. This
  must be a unique vector of patient identifiers.

- ...:

  further arguments to be passed to or from other methods.

## Value

An object of class `jointdata`, with data only on a subset of subjects.

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
take <- heart.jd$survival$num[heart.jd$survival$status == 0]
heart.jd.cens <- subset(heart.jd, take)
```
