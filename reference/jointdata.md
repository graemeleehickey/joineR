# Creates an object of class `jointdata`

This function creates an object of class `jointdata`. This is an object
with information on at least one of, longitudinal data or survival data.
Moreover, it can also have data on baseline covariates.

## Usage

``` r
jointdata(
  longitudinal = NA,
  survival = NA,
  baseline = NA,
  id.col = "ID",
  time.col = NA
)
```

## Arguments

- longitudinal:

  a data frame or matrix in the unbalanced format (one row per
  observation), with subject identification, time of measurements, and
  longitudinal measurements and/or time dependent covariates. This must
  be given if no `survival` argument is.

- survival:

  a data frame or matrix with survival data for all the subjects. This
  must be given if no `longitudinal` argument is.

- baseline:

  a data frame or matrix with baseline covariates, or non-time dependent
  covariates, for the same subjects as in `survival` and/or
  `longitudinal`. This has to be in the balanced format (one row per
  subject). By default an object of this class does not include baseline
  covariates.

- id.col:

  an element of class `character` with the name identification of
  subject. This is to identify the subject identification in the data
  frames.

- time.col:

  an element of class `character` with the time measurements
  identification. This is to identify the time column in the data
  frames.

## Value

A list of length six. The first element is the vector of subjects
identification. The second is, if exists a data frame of the
longitudinal data. The third element of the list is, if exists a data
frame of the survival data. The fourth element of the list is, if exists
a data frame on the baseline covariates. The fifth is, if longitudinal
data is given, the column name identification of longitudinal times. And
the sixth and last element of the list is the column name identification
of subjects.

## Details

This function creates an object of class `jointdata`. This is a list
with elements used in joint modelling, mainly longitudinal and/or
survival data. The output has to have at least one of the data sets,
longitudinal or survival. However, for joint modelling is necessary to
have both data sets. Moreover, a third data frame is possible to be
given as input, for the baseline (non-time dependent) covariates. The
subject identification and time measurement column names are necessary.

## Author

Ines Sousa

## Examples

``` r
data(heart.valve)
heart.surv <- UniqueVariables(heart.valve,
                              var.col = c("fuyrs", "status"),
                              id.col = "num")
heart.valve.jd <- jointdata(survival = heart.surv,
                            id.col = "num",
                            time.col = "time")
```
