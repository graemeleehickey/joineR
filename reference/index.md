# Package index

## Package

Package-level documentation and object descriptions.

- [`joineR`](https://graemeleehickey.github.io/joineR/reference/joineR.md)
  : joineR

- [`joint.object`](https://graemeleehickey.github.io/joineR/reference/joint.object.md)
  :

  Fitted `joint` object

## Fitting joint models

Functions to fit and obtain inference for joint models.

- [`joint()`](https://graemeleehickey.github.io/joineR/reference/joint.md)
  : Fit joint model for survival and longitudinal data measured with
  error
- [`jointSE()`](https://graemeleehickey.github.io/joineR/reference/jointSE.md)
  : Standard errors via bootstrap for a joint model fit

## Data preparation

Functions to create and manipulate `jointdata` objects.

- [`jointdata()`](https://graemeleehickey.github.io/joineR/reference/jointdata.md)
  :

  Creates an object of class `jointdata`

- [`UniqueVariables()`](https://graemeleehickey.github.io/joineR/reference/UniqueVariables.md)
  : Extracts the unique non-time dependent variables per patient, from
  an unbalanced data set

- [`sample.jointdata()`](https://graemeleehickey.github.io/joineR/reference/sample.jointdata.md)
  :

  Sample from a `jointdata` x

- [`subset(`*`<jointdata>`*`)`](https://graemeleehickey.github.io/joineR/reference/subset.jointdata.md)
  :

  Subsetting object of class `jointdata`

- [`to.balanced()`](https://graemeleehickey.github.io/joineR/reference/to.balanced.md)
  : Transform data to the longitudinal balanced format

- [`to.unbalanced()`](https://graemeleehickey.github.io/joineR/reference/to.unbalanced.md)
  : Transform data to the longitudinal unbalanced format

## Visualisation

Plotting functions for longitudinal and joint data.

- [`jointplot()`](https://graemeleehickey.github.io/joineR/reference/jointplot.md)
  : Joint plot of longitudinal and survival data

- [`plot(`*`<jointdata>`*`)`](https://graemeleehickey.github.io/joineR/reference/plot.jointdata.md)
  : Plot longitudinal data

- [`points(`*`<jointdata>`*`)`](https://graemeleehickey.github.io/joineR/reference/points.jointdata.md)
  :

  Add points to an existing `jointdata` plot

- [`lines(`*`<jointdata>`*`)`](https://graemeleehickey.github.io/joineR/reference/lines.jointdata.md)
  :

  Add lines to an existing `jointdata` plot

- [`plot(`*`<vargm>`*`)`](https://graemeleehickey.github.io/joineR/reference/plot.vargm.md)
  : Plots the empirical variogram for longitudinal data

## Summaries

Summary and print methods.

- [`summary(`*`<joint>`*`)`](https://graemeleehickey.github.io/joineR/reference/summary.joint.md)
  : Summarise a random effects joint model fit

- [`summary(`*`<jointdata>`*`)`](https://graemeleehickey.github.io/joineR/reference/summary.jointdata.md)
  :

  Summarise a `jointdata` object

- [`summarybal()`](https://graemeleehickey.github.io/joineR/reference/summarybal.md)
  : Summary of a balanced longitudinal data set

## Simulation and diagnostics

Functions to simulate joint model data and compute variograms.

- [`simjoint()`](https://graemeleehickey.github.io/joineR/reference/simjoint.md)
  : Simulate data from a joint model
- [`variogram()`](https://graemeleehickey.github.io/joineR/reference/variogram.md)
  : Empirical variogram for longitudinal data

## Data sets

- [`heart.valve`](https://graemeleehickey.github.io/joineR/reference/heart.valve.md)
  : Aortic valve replacement surgery data
- [`epileptic`](https://graemeleehickey.github.io/joineR/reference/epileptic.md)
  : Dose calibration of anti-epileptic drugs data
- [`liver`](https://graemeleehickey.github.io/joineR/reference/liver.md)
  : Liver cirrhosis drug trial data
- [`mental`](https://graemeleehickey.github.io/joineR/reference/mental.md)
  : Mental health trial data
- [`aids`](https://graemeleehickey.github.io/joineR/reference/aids.md) :
  AIDS drug trial data
