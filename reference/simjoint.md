# Simulate data from a joint model

This function simulates longitudinal and time-to-event data from a joint
model.

## Usage

``` r
simjoint(
  n = 500,
  model = c("intslope", "int", "quad"),
  sepassoc = FALSE,
  ntms = 5,
  b1 = c(1, 1, 1, 1),
  b2 = c(1, 1),
  gamma = c(1, 0.1),
  sigu,
  vare = 0.01,
  theta0 = -3,
  theta1 = 1,
  censoring = TRUE,
  censlam = exp(-3),
  truncation = FALSE,
  trunctime = max(ntms),
  gridstep = 0.01
)
```

## Arguments

- n:

  the number of subjects to simulate data for.

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

- ntms:

  the maximum number of (discrete) time points to simulate repeated
  longitudinal measurements at.

- b1:

  a vector specifying the coefficients of the fixed effects in the
  longitudinal sub-model. The order in each row is intercept, a
  continuous covariate, covariate, a binary covariate, the measurement
  time.

- b2:

  a vector of `length = 2` specifying the coefficients for the
  time-to-event baseline covariates, in the order of a continuous
  covariate and a binary covariate.

- gamma:

  a vector of specifying the latent association parameter(s) for the
  longitudinal outcome. It must be of length 1 if `sepassoc = FALSE`.

- sigu:

  a positive-definite matrix specifying the variance-covariance matrix.
  If `model = "int"`, the matrix has dimension `dim = c(1, 1)`; if
  `model = "intslope"`, the matrix has dimension `dim = c(2, 2)`; else
  if `model = "quad"`, the matrix has dimension `dim = c(3, 3)`. If
  `D = NULL` (default), an identity matrix is assumed.

- vare:

  a numeric value specifying the residual standard error.

- theta0, theta1:

  parameters controlling the failure rate. See Details.

- censoring:

  logical: if `TRUE`, includes an independent censoring time.

- censlam:

  a scale (\\\>0\\) parameter for an exponential distribution used to
  simulate random censoring times for when `censoring = TRUE`.

- truncation:

  logical: if `TRUE`, adds a truncation time for a maximum event time in
  the case of `model = "int"` or `model = "intslope"`.

- trunctime:

  a truncation time for use when `truncation = TRUE`. For
  `model = "quad"`, `trunctime` is required, and defaults to `max(ntms)`
  if not specified.

- gridstep:

  the step-size for the grid used to simulate event times when
  `model = "quad"`. Default is `gridstep = 0.01`. See Details.

## Value

A list of 2 `data.frame`s: one recording the requisite longitudinal
outcomes data, and one recording the time-to-event data.

## Details

The function `simjoint` simulates data from a joint model, similar to
that performed in Henderson et al. (2000). It works by first simulating
longitudinal data for all possible follow-up times using random draws
for the multivariate Gaussian random effects and residual error terms.
Data can be simulated assuming either random-intercepts only
(`model = "int"`) in each of the longitudinal sub-models;
random-intercepts and random-slopes (`model = "intslope"`); or quadratic
random effects structures (`model = "quad"`). The failure times are
simulated from proportional hazards time-to-event models, using the
following methodologies:

- `model = "int"`:

  The baseline hazard function is specified to be an exponential
  distribution with

  \$\$\lambda_0(t) = \exp{\theta_0}.\$\$

  Simulation is conditional on known time-independent effects, and the
  methodology of Bender et al. (2005) is used to simulate the failure
  time.

- `model = "intslope"`:

  The baseline hazard function is specified to be a Gompertz
  distribution with

  \$\$\lambda_0(t) = \exp{\theta_0 + \theta_1 t}.\$\$

  In the usual representation of the Gompertz distribution, \\\theta_1\\
  is the shape parameter, and the scale parameter is equivalent to
  \\\exp(\theta_0)\\. Simulation is conditional on on a predictable
  (linear) time-varying process, and the methodology of Austin (2012) is
  used to simulate the failure time.

- `model="quad"`:

  The baseline hazard function is specified as per `model="intslope"`.
  The integration technique used for the above two cases is complex in
  quadratic (and higher order) models, therefore we use a different
  approach. We note that hazard function can be written as

  \$\$\lim\_{dt \rightarrow 0} \lambda(t) dt = \lim\_{dt \rightarrow 0}
  P\[t \le T \le t + dt \| T \ge t\].\$\$

  In the simulation routine the parameter `gridstep` acts as \\dt\\ in
  that we choose a nominally small value, which multiplies the hazard
  and this scaled hazard is equivalent to the probability of having an
  event in the interval \\(t, t + dt)\\, or equivalently \\(t, t +
  \\`gridstep`\\)\\. A vector of possible times is set up for each
  individual, ranging from 0 to `trunctime` in increments of \\dt\\ (or
  `gridstep`). The failure probability at each time is compared to an
  independent \\U(0, 1)\\ draw, and if the probability does not exceed
  the random draw then the survival time is set as `trunctime`,
  otherwise it is the generated time from the vector of candidate times.
  The minimum of these candidate times (i.e. when we deem the event to
  have first happened) is taken as the survival time.

## References

Austin PC. Generating survival times to simulate Cox proportional
hazards models with time-varying covariates. *Stat Med.* 2012;
**31(29)**: 3946-3958.

Bender R, Augustin T, Blettner M. Generating survival times to simulate
Cox proportional hazards models. *Stat Med.* 2005; **24**: 1713-1723.

Henderson R, Diggle PJ, Dobson A. Joint modelling of longitudinal
measurements and event time data. *Biostatistics.* 2000; **1(4)**:
465-480.

## Author

Pete Philipson

## Examples

``` r
simjoint(10, sepassoc = TRUE)
#> 100% experienced event
#> $longitudinal
#>    id           Y time intercept      ctsxl binxl ltime
#> 1   1  1.84178069    0         1  1.6099430     0     0
#> 2   1  2.00657409    1         1  1.6099430     0     1
#> 3   1  2.18586040    2         1  1.6099430     0     2
#> 4   2  1.11100954    0         1  1.8611358     0     0
#> 5   2  2.86867843    1         1  1.8611358     0     1
#> 6   2  5.01897712    2         1  1.8611358     0     2
#> 7   3  2.33295276    0         1  0.3862491     0     0
#> 8   3  2.86241527    1         1  0.3862491     0     1
#> 9   3  2.94496750    2         1  0.3862491     0     2
#> 10  4 -0.51126501    0         1 -1.4686434     1     0
#> 11  4 -1.46913114    1         1 -1.4686434     1     1
#> 12  4 -2.73067249    2         1 -1.4686434     1     2
#> 13  4 -3.84850681    3         1 -1.4686434     1     3
#> 14  4 -5.03095798    4         1 -1.4686434     1     4
#> 15  5  1.20236744    0         1  0.7175248     0     0
#> 16  5  1.50775532    1         1  0.7175248     0     1
#> 17  5  2.82161350    2         1  0.7175248     0     2
#> 18  6  3.19876474    0         1  0.1319102     0     0
#> 19  7 -1.28342512    0         1 -1.8659237     0     0
#> 20  7  1.52389884    1         1 -1.8659237     0     1
#> 21  7  4.53450494    2         1 -1.8659237     0     2
#> 22  7  7.44308279    3         1 -1.8659237     0     3
#> 23  8  1.62778261    0         1 -0.9206634     1     0
#> 24  8  1.54253018    1         1 -0.9206634     1     1
#> 25  8  1.62086573    2         1 -0.9206634     1     2
#> 26  8  1.53297015    3         1 -0.9206634     1     3
#> 27  9  1.53799345    0         1  1.2040289     0     0
#> 28  9  0.78328101    1         1  1.2040289     0     1
#> 29  9 -0.07924711    2         1  1.2040289     0     2
#> 30 10  0.75826046    0         1  0.2672676     1     0
#> 31 10  1.04127352    1         1  0.2672676     1     1
#> 
#> $survival
#>    id  survtime cens       ctsx binx
#> 1   1 2.0007289    1  1.6099430    0
#> 2   2 2.5809048    1  1.8611358    0
#> 3   3 2.7068033    1  0.3862491    0
#> 4   4 4.7247363    1 -1.4686434    1
#> 5   5 2.7861392    1  0.7175248    0
#> 6   6 0.7899107    1  0.1319102    0
#> 7   7 3.9291546    1 -1.8659237    0
#> 8   8 3.3509493    1 -0.9206634    1
#> 9   9 2.9885086    1  1.2040289    0
#> 10 10 1.3844656    1  0.2672676    1
#> 
```
