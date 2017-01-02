

#' Dose calibration of anti-epileptic drugs
#' 
#' The SANAD (Standard and New Antiepileptic Drugs) study (Marson et al, 2007)
#' is a randomised control trial of standard and new antiepileptic drugs,
#' comparing effects on longer term clinical outcomes. The data consists of
#' longitudinal measurements of calibrated dose for the groups randomised to a
#' standard drug (CBZ) and a new drug (LTG). The objective of the analysis is
#' to investigate the effect of drug titration on the relative effects of LTG
#' and CBZ on treatment failure (withdrawal of the randomized drug). There are
#' several baseline covariates available, and also data on the time to
#' withdrawal from randomized drug.
#' 
#' 
#' @name epileptic
#' @docType data
#' @format This is a data frame in the unbalanced format, that is, with one row
#' per observation. The data consists of columns for patient identifier, time
#' of measurement, calibrated dose, baseline covariates, and survival data. The
#' column names are identified as follows:
#' 
#' \itemize{ \item\bold{[,1]} - id - patient identifier \item\bold{[,2]} - dose
#' - calibrated dose \item\bold{[,3]} - time - timing of clinic visit at which
#' dose recorded \item\bold{[,4]} - with.time - time of drug withdrawal/maximum
#' follow up time \item\bold{[,5]} - with.status - censoring indicator (1 =
#' withdrawal of randomised drug and 0 = not withdrawn from randomised
#' drug/lost to follow up) \item\bold{[,6]} - with.status.uae - 1 if withdrawal
#' due to unacceptable adverse effects, 0 otherwise \item\bold{[,7]} -
#' with.status.isc - 1 if withdrawal due to inadequate seizure control, 0
#' otherwise \item\bold{[,8]} - treat - randomized treatment (CBZ or LTG)
#' \item\bold{[,9]} - age - age of patient at randomization \item\bold{[,10]} -
#' gender - gender of patient \item\bold{[,11]} - learn.dis - learning
#' disability }
#' @references Williamson P.R. , Kolamunnage-Dona R, Philipson P, Marson A. G.
#' Joint modelling of longitudinal and competing risks data. Statistics in
#' Medicine, . 27, No. 30. (2008), pp. 6426-6438.
#' @source SANAD Trial - University of Liverpool
#' @keywords datasets
NULL





#' Heart Valve surgery
#' 
#' This is longitudinal data on an observational study on detecting effects of
#' different heart valves, differing on type of tissue. The data consists of
#' longitudinal measurements on three different heart function outcomes, after
#' surgery occurred. There are several baseline covariates available, and also
#' survival data.
#' 
#' 
#' @name heart.valve
#' @docType data
#' @format This is a data frame in the unbalanced format, that is, with one row
#' per observation. The data consists in columns for patient identification,
#' time of measurements, longitudinal multiple longitudinal measurements,
#' baseline covariates, and survival data. The column names are identified as
#' follows: \itemize{ \item\bold{num} -number for patient identification
#' \item\bold{sex} -gender of patient (0=Male and 1=Female) \item\bold{age} -
#' age of patient at day of surgery \item\bold{time} - observed time point,
#' with surgery date as the time zero (/years) \item\bold{fuyrs} - maximum
#' follow up time , with surgery date as the time zero (/years)
#' \item\bold{status} -censoring indicator (1=died and 0=lost of follow up)
#' \item\bold{grad} -Gradient, heart function longitudinal outcome
#' \item\bold{log.grad} -logarithm transformation, with base e, of the Gradient
#' longitudinal outcome \item\bold{lvmi} -Left Ventricular Mass Index,
#' standardised by mass index, heart function longitudinal outcome
#' \item\bold{log.lvmi} -logarithm transformation, with base e, of the lvmi
#' longitudinal outcome \item\bold{ef} -Ejection Fraction, heart function
#' longitudinal outcome \item\bold{bsa} -body surface area, baseline covariate
#' \item\bold{lvh} -Left Ventricular pre-surgery hypertrophy, baseline
#' covariate (0=good and 1=bad) \item\bold{prenyha} -pre-surgery New York Heart
#' Association (NYHA) Classification, baseline covariate (1=I/II and 3=III/IV)
#' \item\bold{redo} -revision procedure, baseline covariates (0=no and 1=yes)
#' \item\bold{size} -size of the valve , baseline covariate
#' \item\bold{con.cabg} -concomitant coronary artery bypass, baseline covariate
#' (0=no and 1=yes) \item\bold{creat} -creatinine at baseline \item\bold{dm}
#' -diabetes at baseline (0=no and 1=yes) \item\bold{acei} -ace inhibitor at
#' baseline (0=no and 1=yes) \item\bold{lv} -left ventricular pre-surgery
#' function, baseline covariate (1=good and 2=moderate and 3=poor)
#' \item\bold{emergenc} -operative urgency, baseline covariate (0=elective and
#' 1=urgent and 3=emergency) \item\bold{hc} -high cholesterol , baseline
#' covariate (0=absent and 1=present treated and 2=present untreated)
#' \item\bold{sten.reg.mix} -aortic type (1=stenosis and 2=regurgitation and
#' 3=mixed) \item\bold{hs} -valve type used in the surgery (1=Homograft=human
#' tissue and 0=Stentless=pig tissue) }
#' @references Lim E., Ali A., Theodorou P., Sousa I., Ashrafian H.,
#' Chamageorgakis T., Duncan M., Diggle P. and Pepper J. (2007), A longitudinal
#' study of the profile and predictors of left ventricular mass regression
#' after stentless aortic valve replacement, The Annals of Thoracic Surgery 85
#' (6), June 2008, 2026-2029
#' @source Eric Lim - Royal Brompton Hospital
#' @keywords datasets
#' @examples
#' 
#' data(heart.valve)
#' 
NULL





#' Liver cirrhosis longitudinal data
#' 
#' This dataset gives the longitudinal observations of prothrombin index, a
#' measure of liver function, for patients from a controlled trial into
#' prednisone treatment of liver cirrhosis. Time-to-event information in the
#' form of the event time and associated censoring indicator are also recorded
#' along with a solitary baseline covariate - the allocated treatment arm in
#' this instance. The data are taken from Andersen et al (1993, p. 19) and were
#' analysed in Henderson, Diggle and Dobson (2002). This is a subset of the
#' full data where a number of variables were recorded both at entry and during
#' the course of the trial.
#' 
#' 
#' @name liver
#' @docType data
#' @format A data frame in the unbalanced format with longitudinal observations
#' from 488 subjects. The column form of the data is subject identifier,
#' prothrombin index measurement, time of prothrombin index measurement,
#' treatment indicator and then the survival data. The column names are
#' detailed below: \itemize{ \item\bold{id} -number for patient identification
#' \item\bold{prothrombin} -prothrombin index measurement (?units)
#' \item\bold{time} -time of prothrombin index measurement
#' \item\bold{treatment} -patient treatment indicator (0 = placebo, 1 =
#' prednisone) \item\bold{survival} -patient survival time (in years)
#' \item\bold{cens} -censoring indicator (1 = died and 0 = censored) }
#' @references Andersen, P. K., Borgan O., Gill, R. D. and Kieding, N. (1993).
#' Statistical Models Based on Counting Processes. New York: Springer.
#' 
#' Henderson, R., Diggle, P. and Dobson, A. (2002). Identification and efficacy
#' of longitudinal markers for survival. Biostatistics 3, 33-50.
#' @source Andersen, P. K., Borgan O., Gill, R. D. and Kieding, N. (1993).
#' Statistical Models Based on Counting Processes. New York: Springer.
#' @keywords datasets
#' @examples
#' 
#' data(liver)
#' 
NULL





#' Mental Health Trial Data
#' 
#' The data is obtained from a trial in which chronically ill mental health
#' patients were randomised across two treatments: placebo and an active drug.
#' A questionnaire instrument was used to assess each patient's mental state at
#' weeks 0, 1, 2, 4, 6 and 8 post-randomisation, a high recorded score implying
#' a severe condition. Some of the 100 patients dropped out of the study for
#' reasons that were thought to be related to their mental state, and therefore
#' potentially informative; others dropped out for reasons unrelated to their
#' mental state.
#' 
#' 
#' @name mental
#' @docType data
#' @format A balanced data set with respect to the times at which observations
#' recorded. The data consists of the following variables on each patient:
#' \itemize{ \item\bold{[,1]} - id - patient identifier (1,2,...,100)
#' \item\bold{[,2]} - Y.t0 - mental state assessment in week 0 (coded NA if
#' missing) \item\bold{[,3]} - Y.t1 - mental state assessment in week 1
#' \item\bold{[,4]} - Y.t2 - mental state assessment in week 2 \item\bold{[,5]}
#' - Y.t4 - mental state assessment in week 4 \item\bold{[,6]} - Y.t6 - mental
#' state assessment in week 6 \item\bold{[,7]} - Y.t8 - mental state assessment
#' in week 8 \item\bold{[,8]} - treat - treatment allocation (0=placebo;
#' 1=active drug) \item\bold{[,9]} - n.obs - number of non-missing mental state
#' assessments \item\bold{[,10]} - event.time - imputed dropout time in weeks
#' (coded 8.002 for completers) \item\bold{[,11]} - status - censoring
#' indicator (0=completer or non-informative dropout, 1=potentially informative
#' dropout) }
#' @keywords datasets
NULL



