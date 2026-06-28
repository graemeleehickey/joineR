# Extracts the unique non-time dependent variables per patient, from an unbalanced data set

This function extracts a set of unique variables within a patient,
returning a data frame with columns, patient identification and
variables selected. Each row corresponds to the data for each
individual.

## Usage

``` r
UniqueVariables(data, var.col, id.col = "ID")
```

## Arguments

- data:

  data frame, or matrix, with at least a column of patient
  identification and a covariate column.

- var.col:

  vector of column names or column numbers, of the variables (non-time
  dependent). Cannot have mix of numbers and column names.

- id.col:

  column name or column number of the patient identification.

## Value

A data frame with patient identification and covariates selected. Each
row corresponds to the data for each individual. Note that, this can be
only used for non-time dependent covariates. If extracting unique time
dependent covariates, the function gives an error, because it can't
select what is the unique covariate.

## Details

This function can be used, when longitudinal data is in the unbalanced
format, and it is necessary, for example, to extract the set of unique
baseline covariates, or any non-time dependent variables, that in the
unbalanced format, are repeated for each observation row. Also, if the
original data frame has survival data, this can also be used to extract
the survival information from the original data set.

## Author

Ines Sousa

## Examples

``` r
data(heart.valve)
heart.cov <- UniqueVariables(heart.valve,
                             c(2, 3, 5, 6, 12:25),
                             id.col = "num")
```
