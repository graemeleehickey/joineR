# Fitted `joint` object

An object returned by the `joint` function, inheriting from class
`joint` and representing a fitted joint model for longitudinal and
time-to-event (or competing risks) data.

## Usage

``` r
joint.object
```

## Format

An object of class `NULL` of length 0.

## Value

A list with the following components.

- `coefficients`:

  a list with the estimated coefficients. The components of this list
  are:

  `fixed`

  :   longitudinal and survival sub-model fixed effects.

  `random`

  :   the BLUPs of the random effects.

  `latent`

  :   the latent association parameter(s) from the time-to-event
      sub-model.

- `sigama.z`:

  a numeric double for the residual standard error.

- `sigma.u`:

  the variance-covariance matrix of the random effects.

- `hazard`:

  a vector of the (centered) baseline hazards at each unique failure
  time.

- `log.lik`:

  the log-likelihood from the joint model fit and sub-model
  contributions.

- `numIter`:

  the number of EM algorithm iterations.

- `convergence`:

  a logical value of whether convergence was achieved or not.

- `model`:

  see
  [`joint`](https://graemeleehickey.github.io/joineR/reference/joint.md)
  for details.

- `sepassoc`:

  see
  [`joint`](https://graemeleehickey.github.io/joineR/reference/joint.md)
  for details.

- `sepests`:

  see
  [`joint`](https://graemeleehickey.github.io/joineR/reference/joint.md)
  for details.

- `compRisk`:

  a logical value indicating whether competing risks were detected or
  not.

- `sep.loglike`:

  the log-likelihood from the joint model fit (with association set to
  zero) and separately fitted sub-model contributions.

- `formulae`:

  a list of model formulae. See
  [`joint`](https://graemeleehickey.github.io/joineR/reference/joint.md)
  for details.

- `data`:

  a
  [`jointdata`](https://graemeleehickey.github.io/joineR/reference/jointdata.md)
  object. See
  [`joint`](https://graemeleehickey.github.io/joineR/reference/joint.md)
  for details.

- `call`:

  the model call. Can be used by
  [`update`](https://rdrr.io/r/stats/update.html).

## See also

[`joint`](https://graemeleehickey.github.io/joineR/reference/joint.md).

## Author

Graeme L. Hickey
