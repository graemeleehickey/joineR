# AIDS drug trial data

This dataset describes a randomized clinical trial (Goldman et al.,
1996) in which both survival and longitudinal data were collected to
compare the efficacy and safety of two antiretroviral drugs, namely ddI
(didanosine) and ddC (zalcitabine), in treating HIV-infected patients
intolerant or failing zidovudine (AZT) therapy.

## Usage

``` r
data(aids)
```

## Format

A `data.frame` in the unbalanced format with 1405 longitudinal
observations from 467 subjects. The columns are:

- `id`:

  integer: number for patient identification.

- `time`:

  numeric: time to death (or censoring).

- `death`:

  integer: event indicator. Coded as `0 = `right-censoring, and
  `1 = `death.

- `obstime`:

  numeric: measurement times for the repeated CD4 count measurements.

- `CD4`:

  numeric: CD4 cell counts measured at `obstime`.

- `drug`:

  factor: drug indicator. Coded as `ddI = `didanosine and
  `ddC = `zalcitabine.

- `gender`:

  factor: gender indicator. Coded as `male` and `female`.

- `prevOI`:

  factor: opportunistic infection indicator. Coded as `AIDS = `AIDS
  diagnosis at study entry, and `noAIDS = `no previous infection.

- `AZT`:

  factor: AZT intolerance/failure indicator. Coded as `intolerance` or
  `failure`.

## Source

Guo X, Carlin B. Separate and joint modeling of longitudinal and event
time data using standard computer packages. *The American Statistician*.
2004; **58**: 16-24

## References

Goldman A, Carlin B, Crane L, Launer C, Korvick J, Deyton L, Abrams D.
Response of CD4 and clinical consequences to treatment using ddI or ddC
in patients with advanced HIV infection. *Journal of Acquired Immune
Deficiency Syndromes and Human Retrovirology*. 1996; **11**: 161-169
URL:
[http://www.biostat.umn.edu/~brad/data.html](http://www.biostat.umn.edu/~brad/data.md).

## See also

[`heart.valve`](https://graemeleehickey.github.io/joineR/reference/heart.valve.md),
[`epileptic`](https://graemeleehickey.github.io/joineR/reference/epileptic.md),
[`mental`](https://graemeleehickey.github.io/joineR/reference/mental.md),
[`liver`](https://graemeleehickey.github.io/joineR/reference/liver.md).
