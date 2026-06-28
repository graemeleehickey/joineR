# joineR: Joint Modelling of Repeated Measurements and Time-to-Event Data

Analysis of repeated measurements and time-to-event data via random
effects joint models. Fits the joint models proposed by Henderson and
colleagues
[doi:10.1093/biostatistics/1.4.465](https://doi.org/10.1093/biostatistics/1.4.465)
(single event time) and by Williamson and colleagues (2008)
[doi:10.1002/sim.3451](https://doi.org/10.1002/sim.3451) (competing
risks events time) to a single continuous repeated measure. The
time-to-event data is modelled using a (cause-specific) Cox proportional
hazards regression model with time-varying covariates. The longitudinal
outcome is modelled using a linear mixed effects model. The association
is captured by a latent Gaussian process. The model is estimated using
an Expectation Maximization algorithm. Some plotting functions and the
variogram are also included. This project is funded by the Medical
Research Council (Grant numbers G0400615 and MR/M013227/1).

## See also

Useful links:

- <https://graemeleehickey.github.io/joineR/>

- <https://github.com/graemeleehickey/joineR>

- <https://CRAN.R-project.org/package=joineR>

- Report bugs at <https://github.com/graemeleehickey/joineR/issues>

## Author

**Maintainer**: Graeme L. Hickey <graemeleehickey@gmail.com>
([ORCID](https://orcid.org/0000-0002-4989-0054))

Authors:

- Pete Philipson <peter.philipson1@newcastle.ac.uk>
  ([ORCID](https://orcid.org/0000-0001-7846-0208))

- Ines Sousa <isousa@mct.uminho.pt>
  ([ORCID](https://orcid.org/0000-0002-2712-1713))

- Peter J. Diggle <p.diggle@lancaster.ac.uk>
  ([ORCID](https://orcid.org/0000-0003-3521-5020))

- Paula Williamson <p.r.williamson@liverpool.ac.uk>
  ([ORCID](https://orcid.org/0000-0001-9802-6636))

- Ruwanthi Kolamunnage-Dona <kdrr@liverpool.ac.uk>
  ([ORCID](https://orcid.org/0000-0003-3886-6208))

- Robin Henderson <robin.henderson@ncl.ac.uk>

Other contributors:

- Maria Sudell <m.e.sudell@liverpool.ac.uk> \[contributor\]

- Medical Research Council (Grant numbers: G0400615 and MR/M013227/1)
  \[funder\]
