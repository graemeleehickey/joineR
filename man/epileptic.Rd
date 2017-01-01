\name{epileptic}

\alias{epileptic}

\docType{data}

\title{Dose calibration of anti-epileptic drugs}

\description{The SANAD (Standard and New Antiepileptic Drugs) study (Marson et al, 2007) is a randomised control trial of standard and new antiepileptic drugs, comparing effects on longer term clinical outcomes. The data consists of longitudinal measurements of calibrated dose for the groups randomised to a standard drug (CBZ) and a new drug (LTG). The objective of the analysis is to investigate the effect of drug titration on the relative effects of LTG and CBZ on treatment failure (withdrawal of the randomized drug). There are several baseline covariates available, and also data on the time to withdrawal from randomized drug.}

\usage{data(epileptic)}

\format{
This is a data frame in the unbalanced format, that is, with one
row per observation. The data consists of columns for patient
identifier, time of measurement, calibrated dose, baseline covariates,
and survival data. The column names are identified as follows:

\itemize{
	\item{\bold{[,1]} - id - }{patient identifier}
	\item{\bold{[,2]} - dose - }{calibrated dose}
	\item{\bold{[,3]} - time - }{timing of clinic visit at which dose 		recorded}
	\item{\bold{[,4]} - with.time - }{time of drug withdrawal/maximum 		follow up time}
	\item{\bold{[,5]} - with.status - }{censoring indicator (1 = 		withdrawal of randomised drug and 0 = not withdrawn from 		randomised drug/lost to follow up)}
	\item{\bold{[,6]} - with.status.uae - }{1 if withdrawal due to 		unacceptable adverse effects, 0 otherwise}
	\item{\bold{[,7]} - with.status.isc - }{1 if withdrawal due to 		inadequate seizure control, 0 otherwise}
	\item{\bold{[,8]} - treat - }{randomized treatment (CBZ or LTG)}
	\item{\bold{[,9]} - age - }{age of patient at randomization}
	\item{\bold{[,10]} - gender - }{gender of patient}
	\item{\bold{[,11]} - learn.dis - }{learning disability}
	}}

\source{SANAD Trial - University of Liverpool}
\references{Williamson P.R. , Kolamunnage-Dona R, Philipson P, Marson A. G. Joint modelling of longitudinal and competing risks data. Statistics in Medicine, . 27, No. 30. (2008), pp. 6426-6438.}

\keyword{datasets}
