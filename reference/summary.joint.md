# Summarise a random effects joint model fit

Generic function used to produce summary information from a fitted
random effects joint model as represented by `object` of class `joint`.

## Usage

``` r
# S3 method for class 'joint'
summary(object, variance = TRUE, ...)
```

## Arguments

- object:

  an object of class `joint`.

- variance:

  should the variance components be output as variances or standard
  deviations? Defaults to `variance = TRUE`.

- ...:

  further arguments for the summary.

## Value

An object inheriting from class `summary.joint` with all components
included in `object` (see
[`joint`](https://graemeleehickey.github.io/joineR/reference/joint.md)
for a full description of the components) plus the following components:

- `nobs`:

  the total number of (typically longitudinal) observations (i.e. rows
  in an unbalanced data set).

- `ngrps`:

  the number of groups in the analyzed dataset, often individual
  subjects.

## Author

Pete Philipson

## Examples

``` r
data(heart.valve)
heart.surv <- UniqueVariables(heart.valve,
                              var.col = c("fuyrs","status"),
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
fit <- joint(data = heart.valve.jd,
             long.formula = log.lvmi ~ 1 + time + hs,
             surv.formula = Surv(fuyrs,status) ~ hs,
             model = "intslope")
summary(fit)
#> 
#> Call:
#> joint(data = heart.valve.jd, long.formula = log.lvmi ~ 1 + time + 
#>     hs, surv.formula = Surv(fuyrs, status) ~ hs, model = "intslope")
#> 
#> Random effects joint model
#>  Data: heart.valve.jd 
#>  Log-likelihood: -423.7073 
#> 
#> Longitudinal sub-model fixed effects: log.lvmi ~ 1 + time + hs                              
#> (Intercept)        4.992709176
#> time              -0.006977116
#> hsStentless valve  0.055921729
#> 
#> Survival sub-model fixed effects: Surv(fuyrs, status) ~ hs                           
#> hsStentless valve 0.8106134
#> 
#> Latent association:                 
#> gamma_0 0.8476275
#> 
#> Variance components:
#>         U_0         U_1    Residual 
#> 0.113583056 0.001765284 0.037074269 
#> 
#> Convergence at iteration: 9 
#> 
#> Number of observations: 988 
#> Number of groups: 256 
```
