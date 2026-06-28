# Fit joint model for survival and longitudinal data measured with error

This generic function fits a joint model with random latent association,
building on the formulation described in Wulfsohn and Tsiatis (1997)
while allowing for the presence of longitudinal and survival covariates,
and three choices for the latent process. The link between the
longitudinal and survival processes can be proportional or separate.
When failure is attributable to 2 separate causes, a competing risks
joint model is fitted as per Williamson et al. (2008).

## Usage

``` r
joint(
  data,
  long.formula,
  surv.formula,
  model = c("intslope", "int", "quad"),
  sepassoc = FALSE,
  longsep = FALSE,
  survsep = FALSE,
  gpt = 3,
  lgpt = 10,
  max.it = 200,
  tol = 0.001,
  verbose = FALSE
)
```

## Arguments

- data:

  an object of class `jointdata` containing the variables named in the
  formulae arguments.

- long.formula:

  a formula object with the response variable, and the covariates to
  include in the longitudinal sub-model.

- surv.formula:

  a formula object with the survival time, censoring indicator and the
  covariates to include in the survival sub-model. The response must be
  a survival object as returned by the
  [`Surv`](https://rdrr.io/pkg/survival/man/Surv.html) function.

- model:

  a character string specifying the type of latent association. This
  defaults to the intercept and slope version as seen in Wulfsohn and
  Tsiatis (1997). For association via the random intercept only, choose
  `model = "int"`, whereas for a quadratic association, use
  `model = "quad"`. Computing times are commensurate with the type of
  association structure chosen.

- sepassoc:

  logical value: if `TRUE` then the joint model is fitted with separate
  association, see `Details`.

- longsep:

  logical value: if `TRUE`, parameter estimates and log-likelihood from
  a separate linear mixed model analysis of the longitudinal data (see
  the [`lme`](https://rdrr.io/pkg/nlme/man/lme.html) function for
  details) are returned.

- survsep:

  if `TRUE`, parameter estimates and log-likelihood from a separate
  analysis of the survival data using the Cox proportional hazards model
  are returned (see
  [`coxph`](https://rdrr.io/pkg/survival/man/coxph.html) function for
  details).

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

- verbose:

  if `TRUE`, the parameter estimates at each iteration of the EM
  algorithm are printed. Default is `verbose = FALSE`.

## Value

A list containing the parameter estimates from the joint model and, if
required, from either or both of the separate analyses. The combined
log-likelihood from a separate analysis and the log-likelihood from the
joint model are also produced as part of the fit.

## Details

The `joint` function fits a joint model to survival and longitudinal
data. The formulation is similar to Wulfsohn and Tsiatis (1997). A
linear mixed effects model is assumed for the longitudinal data, namely

\$\$Y_i = X\_{i1}(t_i)^T\beta_1 + D_i(t_i)^T U_i + \epsilon_i,\$\$

where \\U_i\\ is a vector of random effects, \\(U\_{0i}, \ldots
U\_{qi})\\ whose length depends on the model chosen, i.e. \\q = 1\\ for
the random intercept model. \\D_i\\ is the random effects covariate
matrix, which will be time-dependent for all but the random intercept
model. \\X\_{i1}\\ is the longitudinal design matrix for unit \\i\\, and
\\t_i\\ is the vector of measurement times for subject \\i\\.
Measurement error is represented by \\\epsilon_i\\.

The Cox proportional hazards model is adopted for the survival data,
namely

\$\$\lambda(t) = \lambda_0(t) \exp\\{X\_{i2}(t)^T \beta_2 +
D_i(t)(\gamma^T U_i)}\\.\$\$

The parameter \\\gamma\\ determines the level of association between the
two processes. For the intercept and slope model with separate
association we have

\$\$D_i(t) (\gamma^T U_i) = \gamma_0 U\_{0i} + \gamma_1 U\_{1i} t,\$\$

whereas under proportional association

\$\$D_i(t)(\gamma^T U_i) = \gamma (U\_{0i} + U\_{1i} t).\$\$

\\X\_{i2}\\ is the vector of survival covariates for unit \\i\\. The
baseline hazard function is \\\lambda_0(t)\\.

The function uses an EM algorithm to estimate parameters in the joint
model. Starting values are provided by calls to standard R functions
[`lme`](https://rdrr.io/pkg/nlme/man/lme.html) and
[`coxph`](https://rdrr.io/pkg/survival/man/coxph.html) for the
longitudinal and survival components, respectively.

## Note

Since numerical integration is required, it is advisable to check the
stability of the maximum likelihood estimates with an increasing number
of Gauss-Hermite quadrature points. `joint()` uses `gpt = 3` by default,
as this has been adequate for many datasets. However, for certain
datasets and models, this might be too small.

## Competing risks

If failure can be attributed to 2 causes, i.e. so-called competing risks
events data, then a cause-specific hazards model is adopted, namely

\$\$\lambda_g(t) = \lambda\_{0g}(t) \exp\\{X\_{i2}(t)^T \beta_2^{(g)} +
D_i(t)(\gamma^T U_i)}\\,\$\$

where \\g = 1,2\\ denotes the failure type, \\\beta_2^{(g)}\\ (\\g =
1,2\\) are cause-specific hazard parameters corresponding to the same
covariates, and \\\lambda\_{0g}(t)\\ are cause-specific baseline hazard
functions. For this data, a proportional association structure is
assumed (i.e. `sepassoc = FALSE`) and a random-intercepts and
random-slopes model must be used (i.e. `model = "intslope"`). Note that
the function only permits 2 failure types. The model is specified in
full by Williamson et al. (2008). The function `joint()` automatically
detects whether competing risks are present by counting the number of
unique components in the event column on the event time data.

## Separate models

Both `longsep` and `survsep` ignore any latent association (i.e.
\\\gamma = 0\\) between the longitudinal and survival processes but
their output can be used to compare with the results from the joint
model. If interest is solely in the individual processes then the user
should instead make use of the functions
[`lme`](https://rdrr.io/pkg/nlme/man/lme.html) and
[`coxph`](https://rdrr.io/pkg/survival/man/coxph.html) mentioned above.
Furthermore, if interest is in the separate effect of each random effect
(this is for intercept and slope or quadratic models only) upon the
survival data, the user should set `sepassoc = TRUE`.

## References

Wulfsohn MS, Tsiatis AA. A joint model for survival and longitudinal
data measured with error. *Biometrics.* 1997; **53(1)**: 330-339.

Henderson R, Diggle PJ, Dobson A. Joint modelling of longitudinal
measurements and event time data. *Biostatistics.* 2000; **1(4)**:
465-480.

Williamson PR, Kolamunnage-Dona R, Philipson P, Marson AG. Joint
modelling of longitudinal and competing risks data. *Stat Med.* 2008;
**27**: 6426-6438.

## See also

[`lme`](https://rdrr.io/pkg/nlme/man/lme.html),
[`coxph`](https://rdrr.io/pkg/survival/man/coxph.html),
[`jointdata`](https://graemeleehickey.github.io/joineR/reference/jointdata.md),
[`jointplot`](https://graemeleehickey.github.io/joineR/reference/jointplot.md).

## Author

Pete Philipson

## Examples

``` r
## Standard joint model

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
fit <- joint(data = heart.valve.jd,
             long.formula = log.lvmi ~ 1 + time + hs,
             surv.formula = Surv(fuyrs, status) ~ hs,
             model = "intslope")

## Competing risks joint model (same data as Williamson et al. 2008)

if (FALSE) { # \dontrun{
data(epileptic)
epileptic$interaction <- with(epileptic, time * (treat == "LTG"))
longitudinal <- epileptic[, c(1:3, 13)]
survival <- UniqueVariables(epileptic, c(4, 6), "id")
baseline <- UniqueVariables(epileptic, "treat", "id")
data <- jointdata(longitudinal = longitudinal,
                  survival = survival,
                  baseline = baseline,
                  id.col = "id",
                  time.col = "time")

fit2 <- joint(data = data,
              long.formula = dose ~ time + treat + interaction,
              surv.formula = Surv(with.time, with.status2) ~ treat,
              longsep = FALSE, survsep = FALSE,
              gpt = 3)
summary(fit2)
} # }
```
