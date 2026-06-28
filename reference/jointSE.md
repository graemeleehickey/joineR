# Standard errors via bootstrap for a joint model fit

This function takes a model fit from a joint model and calculates
standard errors, with optional confidence intervals, for the main
longitudinal and survival covariates.

## Usage

``` r
jointSE(
  fitted,
  n.boot,
  gpt = 3,
  lgpt = 10,
  max.it = 200,
  tol = 0.001,
  print.detail = FALSE
)
```

## Arguments

- fitted:

  a list containing as components the parameter estimates obtained by
  fitting a joint model along with the respective formulae for the
  longitudinal and survival sub-models and the model chosen, see `joint`
  for further details.

- n.boot:

  argument specifying the number of bootstrap samples to use in order to
  obtain the standard error estimates and confidence intervals. Note
  that at least `n.boot = 100` is required in order for the function to
  return non-zero confidence intervals.

- gpt:

  the number of quadrature points across which the integration with
  respect to the random effects will be performed. Defaults to `gpt = 3`
  which produces stable estimates in most datasets.

- lgpt:

  the number of quadrature points which the log-likelihood is evaluated
  over following a model fit. This defaults to `lgpt = 10`, though
  `lgpt = 3` is often sufficient.

- max.it:

  the maximum number of iterations of the EM algorithm that the function
  will perform. Defaults to `max.it = 200`, though more iterations may
  be necessary for large, complex data.

- tol:

  the tolerance level before convergence of the algorithm is deemed to
  have occurred. Default value is `tol = 0.001`.

- print.detail:

  This argument determines the level of printing that is done during the
  bootstrapping. If `TRUE` then the parameter estimates from each
  bootstrap sample are output.

## Value

An object of class `data.frame` with columns `Component`, `Parameter`,
`Estimate`, `SE`, `p-value`, `95%Lower`, and `95%Upper`.

## Details

Standard errors and confidence intervals are obtained by repeated
fitting of the requisite joint model to bootstrap samples of the
original longitudinal and survival data. It is rare that more than 200
bootstrap samples are needed for estimating a standard error. The number
of bootstrap samples needed for accurate confidence intervals can be as
large as 1000. Two-sided Wald p-values are computed as
\\2\Phi(-\|\hat\theta / \widehat{SE}\|)\\ for all fixed-effect and
association parameters. P-values are `NA` for variance components (`U_*`
and `Residual`) because Wald tests are not appropriate for variance
parameters constrained to be positive.

## References

Wulfsohn MS, Tsiatis AA. A joint model for survival and longitudinal
data measured with error. *Biometrics.* 1997; **53(1)**: 330-339.

Efron B, Tibshirani R. *An Introduction to the Bootstrap.* 2000; Boca
Raton, FL: Chapman & Hall/CRC.

## See also

[`lme`](https://rdrr.io/pkg/nlme/man/lme.html),
[`coxph`](https://rdrr.io/pkg/survival/man/coxph.html),
[`joint`](https://graemeleehickey.github.io/joineR/reference/joint.md),
[`jointdata`](https://graemeleehickey.github.io/joineR/reference/jointdata.md).

## Author

Ruwanthi Kolamunnage-Dona and Pete Philipson

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
             model = "int")
jointSE(fitted = fit, n.boot = 1)
#>      Component         Parameter Estimate SE p-value 95%Lower 95%Upper
#> 1 Longitudinal       (Intercept)   4.9836 NA      NA        0        0
#> 2                           time   0.0001 NA      NA        0        0
#> 3              hsStentless valve   0.0519 NA      NA        0        0
#> 4      Failure hsStentless valve   0.8109 NA      NA        0        0
#> 5  Association           gamma_0   1.1359 NA      NA        0        0
#> 6     Variance               U_0   0.0962 NA      NA        0        0
#> 7                       Residual   0.0454 NA      NA        0        0
```
