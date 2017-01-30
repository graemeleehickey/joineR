#' Dose calibration of anti-epileptic drugs data
#' 
#' The SANAD (Standard and New Anti-epileptic Drugs) study (Marson et al., 2007) 
#' is a randomized control trial of standard and new anti-epileptic drugs, 
#' comparing effects on longer term clinical outcomes. The data consists of 
#' longitudinal measurements of calibrated dose for the groups randomized to a 
#' standard drug (CBZ) and a new drug (LTG). The objective of the analysis is to
#' investigate the effect of drug titration on the relative effects of LTG and 
#' CBZ on treatment failure (withdrawal of the randomized drug). There are 
#' several baseline covariates available, and also data on the time to 
#' withdrawal from randomized drug.
#' 
#' @usage data(epileptic)
#' @format This is a data frame in the unbalanced format, that is, with one row 
#'   per observation. The data consists of columns for patient identifier, time 
#'   of measurement, calibrated dose, baseline covariates, and survival data. 
#'   The column names are identified as follows:
#'   
#'   \describe{
#'   
#'   \item{\code{id}}{integer: patient identifier.}
#'   
#'   \item{\code{dose}}{numeric: calibrated dose.}
#'   
#'   \item{\code{time}}{integer: timing of clinic visit at which dose recorded
#'   (days).}
#'   
#'   \item{\code{with.time}}{integer: time of drug withdrawal/maximum follow up 
#'   time (days).}
#'   
#'   \item{\code{with.status}}{censoring indicator (\code{1 = }withdrawal of 
#'   randomized drug and \code{0 = }not withdrawn from randomized drug/lost to
#'   follow up).}
#'   
#'   \item{\code{with.status.uae}}{\code{1 = }withdrawal due to unacceptable 
#'   adverse effects, \code{0 = }otherwise.}
#'   
#'   \item{\code{with.status.isc}}{\code{1 = }withdrawal due to inadequate 
#'   seizure control, \code{0 = }otherwise.}
#'   
#'   \item{\code{treat}}{factor: randomized treatment (CBZ or LTG).}
#'   
#'   \item{\code{age}}{numeric: age of patient at randomization (years).}
#'   
#'   \item{\code{gender}}{factor: gender of patient. \code{F = }female, \code{M
#'   = }male.}
#'   
#'   \item{\code{learn.dis}}{factor: learning disability.}
#'   
#'   }
#' @keywords datasets
#' @seealso \code{\link{heart.valve}}, \code{\link{liver}}, 
#'   \code{\link{mental}}.
#' @source SANAD Trial - University of Liverpool
#' @references
#' 
#' Marson AG, Appleton R, Baker GA, et al. A randomised controlled trial 
#' examining longer-term outcomes of standard versus new antiepileptic drugs. 
#' The SANAD Trial. \emph{Health Technology Assessment}. 2007; \strong{11(37)}.
#' 
#' Marson AG, Al-Kharusi AM, Alwaidh M, et al. The SANAD study of effectiveness 
#' of carbamazepine, gabapentin, lamotrigine, oxcarbazepine, or topiramate for 
#' treatment of partial epilepsy: an unblinded randomised controlled trial. 
#' \emph{Lancet}. 2007; \strong{365}: 2007-2013.
#' 
#' Williamson PR, Kolamunnage-Dona R, Philipson P, Marson AG. Joint modelling of
#' longitudinal and competing risks data. \emph{Stats Med.} 2008;
#' \strong{27(30)}: 6426-6438.
"epileptic"
