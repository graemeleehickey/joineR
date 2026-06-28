# Mental health trial data

The data is obtained from a trial in which chronically ill mental health
patients were randomized across two treatments: placebo and an active
drug. A questionnaire instrument was used to assess each patient's
mental state at weeks 0, 1, 2, 4, 6 and 8 post-randomisation, a high
recorded score implying a severe condition. Some of the 100 patients
dropped out of the study for reasons that were thought to be related to
their mental state, and therefore potentially informative; others
dropped out for reasons unrelated to their mental state.

## Usage

``` r
data(mental)
```

## Format

A balanced data set with respect to the times at which observations
recorded. The data consists of the following variables on each patient:

- `id`:

  integer: patient identifier.

- `Y.t0`:

  integer: mental state assessment in week 0. Coded `NA` if missing.

- `Y.t1`:

  integer: mental state assessment in week 1. Coded `NA` if missing.

- `Y.t2`:

  integer: mental state assessment in week 2. Coded `NA` if missing.

- `Y.t4`:

  integer: mental state assessment in week 4. Coded `NA` if missing.

- `Y.t6`:

  integer: mental state assessment in week 6. Coded `NA` if missing.

- `Y.t8`:

  integer: mental state assessment in week 8. Coded `NA` if missing.

- `treat`:

  integer: treatment allocation. Coded as `0 = `placebo; `1 = `active
  drug.

- `n.obs`:

  integer: number of non-missing mental state assessments.

- `surv.time`:

  numeric: imputed dropout time in weeks. Coded as `surv.time = 8.002`
  for completers.

- `cens.ind`:

  integer: censoring indicator. Coded as `0 = `completer or
  non-informative dropout; `1 = `potentially informative dropout.

## Source

Peter J. Diggle (p.diggle@lancaster.ac.uk)

## References

Henderson R, Diggle PJ, Dobson A. Joint modelling of longitudinal
measurements and event time data. *Biostatistics.* 2000; **1(4)**:
465-480.

Diggle PJ, Farewell D, Henderson R. Longitudinal data with dropout:
objectives, assumptions and a proposal (with Discussion). *Applied
Statistics.* 2007; **56**: 499-550.

## See also

[`heart.valve`](https://graemeleehickey.github.io/joineR/reference/heart.valve.md),
[`liver`](https://graemeleehickey.github.io/joineR/reference/liver.md),
[`epileptic`](https://graemeleehickey.github.io/joineR/reference/epileptic.md).
