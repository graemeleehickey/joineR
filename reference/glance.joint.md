# Glance at a joint model fit

Returns a single-row `data.frame` of model-level statistics for a fitted
`joint` model. Follows the conventions of the broom package.

## Usage

``` r
# S3 method for class 'joint'
glance(x, ...)
```

## Arguments

- x:

  an object of class `joint`.

- ...:

  additional arguments (currently unused).

## Value

A one-row `data.frame` with columns:

- `logLik`:

  joint log-likelihood at convergence.

- `AIC`:

  Akaike information criterion.

- `BIC`:

  Bayesian information criterion.

- `nobs`:

  total number of longitudinal observations.

- `nsubj`:

  number of subjects.

- `convergence`:

  logical; whether the EM algorithm converged.

- `iter`:

  number of EM iterations taken.

## See also

[`tidy.joint`](https://graemeleehickey.github.io/joineR/reference/tidy.joint.md),
[`joint`](https://graemeleehickey.github.io/joineR/reference/joint.md)

## Examples

``` r
data(heart.valve)
heart.surv <- UniqueVariables(heart.valve,
                              var.col = c("fuyrs", "status"),
                              id.col = "num")
heart.long <- heart.valve[, c("num", "time", "log.lvmi")]
heart.cov <- UniqueVariables(heart.valve,
                             c("age", "hs", "sex"),
                             id.col = "num")
heart.valve.jd <- jointdata(longitudinal = heart.long,
                            baseline = heart.cov,
                            survival = heart.surv,
                            id.col = "num",
                            time.col = "time")
fit <- joint(heart.valve.jd,
             long.formula = log.lvmi ~ 1 + time + hs,
             surv.formula = Surv(fuyrs, status) ~ hs,
             model = "intslope")
glance(fit)
#>      logLik      AIC     BIC nobs nsubj convergence iter
#> 1 -423.7073 863.4146 891.776  988   256        TRUE    9
```
