\name{heart.valve}
\alias{heart.valve}
\docType{data}
\title{Heart Valve surgery}
\description{This is longitudinal data on an observational study on
  detecting effects of different heart valves, differing on type of
  tissue. The data consists of longitudinal measurements on three
  different heart function outcomes, after surgery occurred. There are
  several baseline covariates available, and also survival data.}
\usage{data(heart.valve)}
\format{This is a data frame in the unbalanced format, that is, with one row per
  observation. The data consists in columns for patient identification,
  time of measurements, longitudinal multiple longitudinal measurements, baseline covariates,
  and survival data. The column names are identified as follows:
  \itemize{
\item{\bold{num} -}{number for patient identification}
\item{\bold{sex} -}{gender of patient (0=Male and 1=Female)}
\item{\bold{age} - }{age of patient at day of surgery}
\item{\bold{time} - }{observed time point, with surgery date as the time zero (/years)}
\item{\bold{fuyrs} - }{maximum follow up time , with surgery date as the time zero (/years)}
\item{\bold{status} -}{censoring indicator (1=died and 0=lost of follow up)}
\item{\bold{grad} -}{Gradient, heart function longitudinal outcome}
\item{\bold{log.grad} -}{logarithm transformation, with base e, of the Gradient longitudinal outcome}
\item{\bold{lvmi} -}{Left Ventricular Mass Index, standardised by mass index, heart function longitudinal outcome}
\item{\bold{log.lvmi} -}{logarithm transformation, with base e, of the lvmi longitudinal outcome}
\item{\bold{ef} -}{Ejection Fraction, heart function longitudinal outcome}
\item{\bold{bsa} -}{body surface area, baseline covariate}
\item{\bold{lvh} -}{Left Ventricular pre-surgery hypertrophy, baseline covariate  (0=good and 1=bad)}
\item{\bold{prenyha} -}{pre-surgery New York Heart Association (NYHA) Classification, baseline covariate (1=I/II and 3=III/IV)}
\item{\bold{redo} -}{revision procedure, baseline covariates (0=no and 1=yes)}
\item{\bold{size} -}{size of the valve , baseline covariate}
\item{\bold{con.cabg} -}{concomitant coronary artery bypass, baseline covariate (0=no and 1=yes)}
\item{\bold{creat} -}{creatinine at baseline }
\item{\bold{dm} -}{diabetes at baseline (0=no and 1=yes)}
\item{\bold{acei} -}{ace inhibitor at baseline (0=no and 1=yes)}
\item{\bold{lv} -}{left ventricular pre-surgery function, baseline covariate (1=good and 2=moderate and 3=poor)}
\item{\bold{emergenc} -}{operative urgency, baseline covariate (0=elective and 1=urgent and 3=emergency)}
\item{\bold{hc} -}{high cholesterol , baseline covariate (0=absent and 1=present treated and 2=present untreated)}
\item{\bold{sten.reg.mix} -}{aortic type (1=stenosis and 2=regurgitation and 3=mixed)}
\item{\bold{hs} -}{valve type used in the surgery (1=Homograft=human tissue and 0=Stentless=pig tissue)}
}}
\source{Eric Lim - Royal Brompton Hospital}
\references{
Lim E., Ali A., Theodorou P., Sousa I., Ashrafian H., Chamageorgakis T., Duncan M., Diggle P. and Pepper J. (2007), A longitudinal study of the profile and predictors of left ventricular mass regression after stentless aortic valve replacement, The Annals of Thoracic Surgery 85 (6), June 2008, 2026-2029
}
%\seealso{\code{}}
\examples{
data(heart.valve)
}
\keyword{datasets}
