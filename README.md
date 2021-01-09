
<!-- README.md is generated from README.Rmd. Please edit that file -->

# joineR <img src="man/figures/hex.png" width = "175" height = "200" align="right" />

<!-- badges: start -->

[![R build
status](https://github.com/graemeleehickey/joineR/workflows/R-CMD-check/badge.svg)](https://github.com/graemeleehickey/joineR/actions)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/graemeleehickey/joineR?branch=master&svg=true)](https://ci.appveyor.com/project/graemeleehickey/joineR)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/joineR)](https://CRAN.R-project.org/package=joineR)
[![](https://cranlogs.r-pkg.org/badges/joineR)](https://CRAN.R-project.org/package=joineR)
[![](https://cranlogs.r-pkg.org/badges/grand-total/joineR)](https://CRAN.R-project.org/package=joineR)
[![codecov](https://codecov.io/gh/graemeleehickey/joineR/branch/master/graph/badge.svg)](https://codecov.io/gh/graemeleehickey/joineR)
[![Research software
impact](http://depsy.org/api/package/cran/joineR/badge.svg)](http://depsy.org/package/r/joineR)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1167708.svg)](https://doi.org/10.5281/zenodo.1167708)
<!-- badges: end -->

The `joineR` package implements methods for analyzing data from
longitudinal studies in which the response from each subject consists of
a time-sequence of repeated measurements and a possibly censored
time-to-event outcome. The modelling framework for the repeated
measurements is the linear model with random effects and/or correlated
error structure (Laird and Ware, 1982). The model for the time-to-event
outcome is a Cox proportional hazards model with log-Gaussian frailty
(Cox, 1972). Stochastic dependence is captured by allowing the Gaussian
random effects of the linear model to be correlated with the frailty
term of the Cox proportional hazards model. The methodology used to fit
the model is described in Henderson et al. (2002) and Wulfsohn and
Tsiatis (1997).

The `joineR` package also allows competing risks data to be jointly
modelled through a cause-specific hazards model. The importance of
accounting for competing risks is detailed in Williamson et
al. (2007a,b). The methodology used to fit this model is described in
Williamson et al. (2008).

# Example

The `joineR` package comes with several data sets including one the
describes the survival of patients who underwent aortic valve
replacement surgery. The patients were routinely followed up in clinic,
where the left ventricular mass index (LVMI) was calculated. To fit a
joint model, we must first create a `jointdata` object, which holds the
survival, longitudinal, and baseline covariate data, along with the
names of the columns that identify the patient identifiers and repeated
time outcomes.

``` r
library(joineR)
#> Loading required package: survival
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
```

With the creation of the `heart.valve.jd` object, we can fit a joint
model using the `joint` function. For this, we need 4 arguments:

  - `jointdata`: the data object we created above
  - `long.formula`: the linear mixed effects model formula for the
    longitudinal sub-model
  - `surv.formula`: the survival formula the survival sub-model
  - `model`: the latent association structure.

<!-- end list -->

``` r
fit <- joint(data = heart.valve.jd, 
             long.formula = log.lvmi ~ 1 + time + hs, 
             surv.formula = Surv(fuyrs, status) ~ hs, 
             model = "intslope")

summary(fit)
#> 
#> Call:
#> joint(data = heart.valve.jd, long.formula = log.lvmi ~ 1 + time + 
#>     hs, surv.formula = Surv(fuyrs, status) ~ hs, model = "intslope")
#> 
#> Random effects joint model
#>  Data: heart.valve.jd 
#>  Log-likelihood: -424.7062 
#> 
#> Longitudinal sub-model fixed effects: log.lvmi ~ 1 + time + hs                              
#> (Intercept)        4.993354492
#> time              -0.006966354
#> hsStentless valve  0.055452730
#> 
#> Survival sub-model fixed effects: Surv(fuyrs, status) ~ hs                           
#> hsStentless valve 0.7926683
#> 
#> Latent association:                 
#> gamma_0 0.8227578
#> 
#> Variance components:
#>         U_0         U_1    Residual 
#> 0.113521695 0.001757578 0.037086210 
#> 
#> Convergence at iteration: 13 
#> 
#> Number of observations: 988 
#> Number of groups: 256
```

Full details on the data and the functions are provided in the help
documentation and package vignette. The purpose of this code is to
simply illustrate the ease and speed in fitting the models.

# Multivariate data

`joineR` only models a single repeated measurement and a single event
time. If multiple longitudinal outcomes are available (see Hickey et
al., 2016), a separate package is available:
[`joineRML`](https://CRAN.R-project.org/package=joineRML).

# Funding

This project was funded by the [Medical Research
Council](http://www.mrc.ac.uk) (Grant numbers G0400615 and
MR/M013227/1).

![](http://www.mrc.ac.uk/mrc/includes/themes/MRC/images/template/desktop/logo.png)

# Using the latest developmental version

To install the latest **developmental version**, you will need the R
package `devtools` and to run the following code

``` r
library('devtools')
install_github('graemeleehickey/joineR', build_vignettes = FALSE)
```

# References

1.  Cox DR. Regression models and life-tables. *J R Stat Soc Ser B Stat
    Methodol.* 1972; **34(2)**: 187-220.

2.  Henderson R, Diggle PJ, Dobson A. Joint modelling of longitudinal
    measurements and event time data. *Biostatistics.* 2000; **1(4)**:
    465-480.

3.  Hickey GL, Philipson P, Jorgensen A, Kolamunnage-Dona R. Joint
    modelling of time-to-event and multivariate longitudinal outcomes:
    recent developments and issues. *BMC Med Res Methodol.* 2016;
    **16(1)**: 117.

4.  Laird NM, Ware JH. Random-effects models for longitudinal data.
    *Biometrics.* 1982; **38(4)**: 963-974.

5.  Williamson PR, Kolamunnage-Dona R, Tudur-Smith C. The influence of
    competing-risks setting on the choice of hypothesis test for
    treatment effect. *Biostatistics.* 2007; **8(4)**: 689–694.

6.  Williamson PR., Tudur-Smith C, Sander JW, Marson AG. Importance of
    competing risks in the analysis of anti-epileptic drug failure.
    *Trials.* 2007; **8**: 12.

7.  Williamson PR, Kolamunnage-Dona R, Philipson P, Marson AG. Joint
    modelling of longitudinal and competing risks data. *Stat Med.*
    2008; **27**: 6426–6438.

8.  Wulfsohn MS, Tsiatis AA. A joint model for survival and longitudinal
    data measured with error. *Biometrics.* 1997; **53(1)**: 330-339.
