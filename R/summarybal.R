#' Summary of a balanced longitudinal data set
#' 
#' For a balanced longitudinal data set a vector of the mean response and 
#' variances at defined time points is returned along with the correlation 
#' matrix of the responses across the time points.
#' 
#' @param object a longitudinal data set in the balanced format.
#' @param Y.col the column numbers of the longitudinal measurements at each 
#'   design time point in the \code{object}. This does not have to be all of the
#'   longitudinal measurements taken and may be a subset instead.
#' @param times a vector of unique time points of the longitudinal measurements.
#'   This does not have to be all of the study time points and may be a subset
#'   instead, but should match the columns defined in \code{Y.col}.
#' @param use an optional character string giving a method for computing 
#'   covariances in the presence of missing values. This must be (an
#'   abbreviation of) one of the strings \code{"all.obs"}, \code{"complete.obs"}
#'   or \code{"pairwise.complete.obs"}. Defaults to \code{use="all.obs"}.
#' @param na.rm logical. Should missing values be removed? By default, 
#'   \code{na.rm=FALSE}.
#' @param ... further arguments for the summary.
#'   
#' @author Ines Sousa (\email{isousa@@math.uminho.pt})
#' @seealso \code{\link{to.balanced}}.
#' @keywords methods
#'   
#' @return A \code{list} with three elements:
#'   
#'   \item{\code{mean.vect}}{a matrix with the time points in the first column
#'   and the mean response vector as the second column.}
#'   
#'   \item{\code{variance}}{The vector of variances for the response at the time
#'   points.}
#'   
#'   \item{\code{cor.mtx}}{Containing the correlation matrix of the responses
#'   between each pair of time points.}
#' @export
#' 
#' @examples
#' 
#' data(mental)
#' summarybal(mental, Y.col = 2:7, times = c(0, 1, 2, 4, 6, 8), na.rm = TRUE)
summarybal <- function(object, Y.col, times, use = "all.obs", na.rm, ...) {
  
  if (is.numeric(Y.col)) {
    resp <- object[, Y.col]
  } else {
    resp <- object[[Y.col]]
  }
  times <- unique(times)
  m.t <- apply(resp, 2, function(x) mean(x, na.rm = na.rm))
  times <- times[order(times)]
  mean.vect <- data.frame(x = times, y = m.t)
  vv <- apply(resp, 2, function(x) var(x, na.rm = na.rm, ...))        
  dt <- resp
  lt <- dim(resp)[2]
  cr <- matrix(ncol = lt, nrow = lt)
  diag(cr) <- 1
  
  for (i in 1:(lt - 1)){
    for (j in (i + 1):lt) {
      id <- !is.na( dt[, i]) & !is.na(dt[, j] )
      cr[i, j] <- cor(dt[id, i], dt[id, j], use = use, ...)
    }
  }
  cr[lower.tri(cr)] <- t(cr)[lower.tri(t(cr))]	    	    	
  re <- list(mean.vect = mean.vect, variance = vv, cor.mtx = cr)
  
  return(re)
  
}
