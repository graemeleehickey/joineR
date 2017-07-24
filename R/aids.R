#' AIDS drug trial data
#' 
#' @description This dataset describes a randomized clinical trial (Goldman et 
#'   al., 1996) in which both survival and longitudinal data were collected to 
#'   compare the efficacy and safety of two antiretroviral drugs, namely ddI 
#'   (didanosine) and ddC (zalcitabine), in treating HIV-infected patients 
#'   intolerant or failing zidovudine (AZT) therapy.
#'   
#' @usage data(aids)
#' @format A \code{data.frame} in the unbalanced format with 1405 longitudinal 
#'   observations from 467 subjects. The columns are:
#'   
#'   \describe{
#'   
#'   \item{\code{id}}{integer: number for patient identification.}
#'   
#'   \item{\code{time}}{numeric: time to death (or censoring).}
#'   
#'   \item{\code{death}}{integer: event indicator. Coded as \code{0 = 
#'   }right-censoring, and \code{1 = }death.}
#'   
#'   \item{\code{obstime}}{numeric: measurement times for the repeated CD4 count
#'   measurements.}
#'   
#'   \item{\code{CD4}}{numeric: CD4 cell counts measured at \code{obstime}.}
#'   
#'   \item{\code{drug}}{factor: drug indicator. Coded as \code{ddI = }didanosine
#'   and \code{ddC = }zalcitabine.}
#'   
#'   \item{\code{gender}}{factor: gender indicator. Coded as \code{male} and 
#'   \code{female}.}
#'   
#'   \item{\code{prevOI}}{factor: opportunistic infection indicator. Coded as 
#'   \code{AIDS = }AIDS diagnosis at study entry, and \code{noAIDS = }no 
#'   previous infection.}
#'   
#'   \item{\code{AZT}}{factor: AZT intolerance/failure indicator. Coded as 
#'   \code{intolerance} or \code{failure}.} }
#' @keywords datasets
#' @seealso \code{\link{heart.valve}}, \code{\link{epileptic}}, 
#'   \code{\link{mental}}, \code{\link{liver}}.
#' @source Guo X, Carlin B. Separate and joint modeling of longitudinal and
#'   event time data using standard computer packages. \emph{The American
#'   Statistician}. 2004; \strong{58}: 16–24.
#' @docType data
#' @references
#' 
#' Goldman A, Carlin B, Crane L, Launer C, Korvick J, Deyton L, Abrams K.
#' Response of CD4+ and clinical consequences to treatment using ddI or ddC in
#' patients with advanced HIV infection. \emph{Journal of Acquired Immune
#' Deficiency Syndromes and Human Retrovirology}. 1996; \strong{11}: 161–169.
#' URL: \url{http://www.biostat.umn.edu/~brad/data.html}.
"aids"