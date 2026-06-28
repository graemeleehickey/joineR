# Dose calibration of anti-epileptic drugs data

The SANAD (Standard and New Anti-epileptic Drugs) study (Marson et al.,
2007) is a randomized control trial of standard and new anti-epileptic
drugs, comparing effects on longer term clinical outcomes. The data
consists of longitudinal measurements of calibrated dose for the groups
randomized to a standard drug (CBZ) and a new drug (LTG). The objective
of the analysis is to investigate the effect of drug titration on the
relative effects of LTG and CBZ on treatment failure (withdrawal of the
randomized drug). There are several baseline covariates available, and
also data on the time to withdrawal from randomized drug.

## Usage

``` r
data(epileptic)
```

## Format

This is a data frame in the unbalanced format, that is, with one row per
observation. The data consists of columns for patient identifier, time
of measurement, calibrated dose, baseline covariates, and survival data.
The column names are identified as follows:

- `id`:

  integer: patient identifier.

- `dose`:

  numeric: calibrated dose.

- `time`:

  integer: timing of clinic visit at which dose recorded (days).

- `with.time`:

  integer: time of drug withdrawal/maximum follow up time (days).

- `with.status`:

  censoring indicator (`1 = `withdrawal of randomized drug and `0 = `not
  withdrawn from randomized drug/lost to follow up).

- `with.status2`:

  censoring indicator (`1 = `withdrawal of randomized drug due to
  inadequate seizure control, (`2 = `withdrawal of randomized drug due
  to unacceptable adverse effects, and `0 = `not withdrawn from
  randomized drug/lost to follow up).

- `with.status.uae`:

  `1 = `withdrawal due to unacceptable adverse effects, `0 = `otherwise.

- `with.status.isc`:

  `1 = `withdrawal due to inadequate seizure control, `0 = `otherwise.

- `treat`:

  factor: randomized treatment (CBZ or LTG).

- `age`:

  numeric: age of patient at randomization (years).

- `gender`:

  factor: gender of patient. `F = `female, `M = `male.

- `learn.dis`:

  factor: learning disability.

## Source

SANAD Trial Group, University of Liverpool

## References

Marson AG, Appleton R, Baker GA, et al. A randomised controlled trial
examining longer-term outcomes of standard versus new antiepileptic
drugs. The SANAD Trial. *Health Tech Assess*. 2007; **11(37)**.

Marson AG, Al-Kharusi AM, Alwaidh M, et al. The SANAD study of
effectiveness of carbamazepine, gabapentin, lamotrigine, oxcarbazepine,
or topiramate for treatment of partial epilepsy: an unblinded randomised
controlled trial. *Lancet*. 2007; **365**: 2007-2013.

Williamson PR, Kolamunnage-Dona R, Philipson P, Marson AG. Joint
modelling of longitudinal and competing risks data. *Stats Med.* 2008;
**27(30)**: 6426-6438.

## See also

[`heart.valve`](https://graemeleehickey.github.io/joineR/reference/heart.valve.md),
[`liver`](https://graemeleehickey.github.io/joineR/reference/liver.md),
[`mental`](https://graemeleehickey.github.io/joineR/reference/mental.md),
[`aids`](https://graemeleehickey.github.io/joineR/reference/aids.md).
