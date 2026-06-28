# Liver cirrhosis drug trial data

This dataset gives the longitudinal observations of prothrombin index, a
measure of liver function, for patients from a controlled trial into
prednisone treatment of liver cirrhosis. Time-to-event information in
the form of the event time and associated censoring indicator are also
recorded along with a solitary baseline covariate - the allocated
treatment arm in this instance. The data are taken from Andersen et al.
(1993, p. 19) and were analyzed in Henderson et al. (2002). This is a
subset of the full data where a number of variables were recorded both
at entry and during the course of the trial.

## Usage

``` r
data(liver)
```

## Format

A `data.frame` in the unbalanced format with longitudinal observations
from 488 subjects. The columns are:

- `id`:

  integer: number for patient identification.

- `prothrombin`:

  integer: prothrombin index measurement (%).

- `time`:

  numeric: time of prothrombin index measurement (years).

- `treatment`:

  integer: patient treatment indicator. Coded as `0 = `placebo;
  `1 = `prednisone.

- `survival`:

  numeric: patient survival time (years).

- `cens`:

  integer: censoring indicator. Coded as `1 = `died; `0 = `censored.

## Source

Skrondal A, Rabe-Hesketh S. *Generalized Latent Variable Modeling:
Multilevel, Longitudinal and Structural Equation Models.* Chapman &
Hall/CRC. 2004. URL: <http://www.gllamm.org/books/readme.html#14.6>.

## References

Andersen PK, Borgan O, Gill RD, Kieding N. *Statistical Models Based on
Counting Processes*. New York: Springer. 2003.

Henderson R, Diggle PJ, Dobson A. Identification and efficacy of
longitudinal markers for survival. *Biostatistics* 2002; **3**: 33-50.

## See also

[`heart.valve`](https://graemeleehickey.github.io/joineR/reference/heart.valve.md),
[`epileptic`](https://graemeleehickey.github.io/joineR/reference/epileptic.md),
[`mental`](https://graemeleehickey.github.io/joineR/reference/mental.md),
[`aids`](https://graemeleehickey.github.io/joineR/reference/aids.md).
