# Tidy a joint model fit

Summarises parameter estimates from a fitted `joint` model into a tidy
`data.frame`, one row per term. Follows the conventions of the broom
package.

## Usage

``` r
# S3 method for class 'joint'
tidy(x, se = NULL, conf.int = FALSE, conf.level = 0.95, ...)
```

## Arguments

- x:

  an object of class `joint`.

- se:

  a `data.frame` returned by
  [`jointSE`](https://graemeleehickey.github.io/joineR/reference/jointSE.md),
  optionally supplied to include standard errors, Wald p-values, and
  confidence intervals in the output. If `NULL` (the default), these
  columns are included but set to `NA`.

- conf.int:

  logical; if `TRUE` and `se` is provided, include `conf.low` and
  `conf.high` columns. Defaults to `FALSE`.

- conf.level:

  the confidence level for the intervals, matching the level used in
  `se`. Defaults to `0.95`.

- ...:

  additional arguments (currently unused).

## Value

A `data.frame` with columns:

- `component`:

  sub-model: `"longitudinal"`, `"survival"`, `"association"`, or
  `"variance"`.

- `term`:

  parameter name.

- `estimate`:

  point estimate.

- `std.error`:

  bootstrap standard error (`NA` if `se` not supplied).

- `statistic`:

  Wald z-statistic (`NA` if `se` not supplied or for variance
  components).

- `p.value`:

  two-sided Wald p-value (`NA` if `se` not supplied or for variance
  components).

- `conf.low`, `conf.high`:

  bootstrap percentile confidence bounds, included only when
  `conf.int = TRUE` and `se` is provided.

## See also

[`glance.joint`](https://graemeleehickey.github.io/joineR/reference/glance.joint.md),
[`joint`](https://graemeleehickey.github.io/joineR/reference/joint.md),
[`jointSE`](https://graemeleehickey.github.io/joineR/reference/jointSE.md)

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
tidy(fit)
#>      component              term     estimate std.error statistic p.value
#> 1 longitudinal       (Intercept)  4.992709176        NA        NA      NA
#> 2 longitudinal              time -0.006977116        NA        NA      NA
#> 3 longitudinal hsStentless valve  0.055921729        NA        NA      NA
#> 4     survival hsStentless valve  0.810613443        NA        NA      NA
#> 5  association           gamma_0  0.847627525        NA        NA      NA
#> 6     variance               U_0  0.113583056        NA        NA      NA
#> 7     variance               U_1  0.001765284        NA        NA      NA
#> 8     variance          Residual  0.037074269        NA        NA      NA
```
