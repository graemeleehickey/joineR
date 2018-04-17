#' Liver cirrhosis drug trial data
#' 
#' @description This dataset gives the longitudinal observations of prothrombin 
#'   index, a measure of liver function, for patients from a controlled trial 
#'   into prednisone treatment of liver cirrhosis. Time-to-event information in 
#'   the form of the event time and associated censoring indicator are also 
#'   recorded along with a solitary baseline covariate - the allocated treatment
#'   arm in this instance. The data are taken from Andersen et al. (1993, p. 19)
#'   and were analyzed in Henderson et al. (2002). This is a subset of the full 
#'   data where a number of variables were recorded both at entry and during the
#'   course of the trial.
#'   
#' @usage data(liver)
#' @format A data frame in the unbalanced format with longitudinal observations 
#'   from 488 subjects. The columns are:
#'   
#'   \describe{
#'   
#'   \item{\code{id}}{integer: number for patient identification.}
#'   
#'   \item{\code{prothrombin}}{integer: prothrombin index measurement (\%).}
#'   
#'   \item{\code{time}}{numeric: time of prothrombin index measurement (years).}
#'   
#'   \item{\code{treatment}}{integer: patient treatment indicator. Coded as 
#'   \code{0 = }placebo; \code{1 = }prednisone.}
#'   
#'   \item{\code{survival}}{numeric: patient survival time (years).}
#'   
#'   \item{\code{cens}}{integer: censoring indicator. Coded as \code{1 = }died; 
#'   \code{0 = }censored.}
#'   
#'   }
#' @keywords datasets
#' @seealso \code{\link{heart.valve}}, \code{\link{epileptic}}, 
#'   \code{\link{mental}}, \code{\link{aids}}.
#' @source Skrondal A, Rabe-Hesketh S. \emph{Generalized Latent Variable
#'   Modeling: Multilevel, Longitudinal and Structural Equation Models.} Chapman
#'   & Hall/CRC. 2004. URL: \url{http://www.gllamm.org/books/readme.html#14.6}.
#' @docType data
#' @references
#' 
#' Andersen PK, Borgan O, Gill RD, Kieding N. \emph{Statistical Models Based on 
#' Counting Processes}. New York: Springer. 2003.
#' 
#' Henderson R, Diggle PJ, Dobson A. Identification and efficacy of longitudinal
#' markers for survival. \emph{Biostatistics} 2002; \strong{3}: 33-50.
"liver"