\name{liver}
\alias{liver}
\docType{data}
\title{Liver cirrhosis longitudinal data}
\description{
  This dataset gives the longitudinal observations of prothrombin index,
  a measure of liver function, for patients from a
  controlled trial into prednisone treatment of liver cirrhosis. Time-to-event information in the form of the event time and associated censoring indicator are also recorded along with a solitary baseline covariate - the allocated treatment arm in this instance. The data are taken from Andersen et al (1993, p. 19) and were analysed in Henderson, Diggle and Dobson (2002). This is a subset of the full data where a number of variables were recorded both at entry and during the course of the trial.
}
\usage{data(liver)}
\format{
  A data frame in the unbalanced format with longitudinal observations from 488
  subjects. The column form of the data is subject identifier, prothrombin index measurement, time of prothrombin index measurement, treatment indicator and then the survival data. The column names are detailed below:
  \itemize{
\item{\bold{id} -}{number for patient identification}
\item{\bold{prothrombin} -}{prothrombin index measurement (?units)}
\item{\bold{time} -}{time of prothrombin index measurement}
\item{\bold{treatment} -}{patient treatment indicator (0 = placebo, 1 = prednisone)}
\item{\bold{survival} -}{patient survival time (in years)}
\item{\bold{cens} -}{censoring indicator (1 = died and 0 = censored)}
}}
\source{
  Andersen, P. K., Borgan O., Gill, R. D. and Kieding,
  N. (1993). Statistical Models Based on Counting Processes. New York: Springer.
}
\references{
  Andersen, P. K., Borgan O., Gill, R. D. and Kieding,
  N. (1993). Statistical Models Based on Counting Processes. New York: Springer.

  Henderson, R., Diggle, P. and Dobson, A. (2002). Identification and
  efficacy of longitudinal markers for survival. Biostatistics 3, 33-50.
}
\examples{
data(liver)
}
\keyword{datasets}

