#' Add lines to an existing \code{jointdata} plot
#' 
#' @description Add lines to an existing plot of an object of class
#'   \code{jointdata}, for a longitudinal variable. It is possible to plot all
#'   the subjects in the data set, or just a selected \code{subset}. See
#'   \code{\link{subset.jointdata}}.
#' 
#' @inheritParams plot.jointdata
#'   
#' @author Ines Sousa (\email{isousa@@math.uminho.pt})
#' @keywords aplot
#' @seealso Other functions are useful to be used with this such as
#'   \code{\link{plot}} and \code{\link{points}}.
#'   
#' @return A graphical device with a plot for longitudinal data.
#' @export
#' 
#' @examples
#' data(heart.valve)
#' heart.surv <- UniqueVariables(heart.valve, 
#'                               var.col = c("fuyrs", "status"), 
#'                               id.col = "num")
#' heart.long <- heart.valve[, c(1, 4, 5, 7, 8, 9, 10, 11)]
#' heart.jd <- jointdata(longitudinal = heart.long, 
#'                       survival = heart.surv,
#'                       id.col = "num",
#'                       time.col = "time")
#'                       
#' # Randomly select a pair of subjects to plot profiles of
#' take <- sample(1:max(heart.jd$survival$num), 2)
#' heart.jd.1 <- subset(heart.jd, take[1])
#' heart.jd.2 <- subset(heart.jd, take[2])
#' 
#' plot(heart.jd.1, Y.col = 4)
#' lines(heart.jd.2, Y.col = 4, lty = 2)
lines.jointdata <- function(x, Y.col, ...) {
  
  if (!inherits(x, "jointdata")) {
    stop("Data must be of class 'jointdata'\n")
  }
  
  object <- x
  if (!is.vector(Y.col) | length(Y.col) > 1) {
    stop("Only one longitudinal response is possible to plot")
  }
  if (is.numeric(Y.col)) {
    resp <- object$longitudinal[, Y.col]
  } else {
    resp <- object$longitudinal[[Y.col]]
    Y.col <- which(names(object$longitudinal) %in% Y.col)
  }
  subject = object$longitudinal[[object$subj.col]]
  time = object$longitudinal[[object$time.col]]
  ix <- is.na(resp) == F
  subj <- subject[ix]
  y <- resp[ix]
  t <- time[ix]
  o <- order(subj)
  subj <- subj[o]
  y <- y[o]
  t <- t[o]
  subj.uni <- unique(subj)
  n <- length(subj)
  X <- as.data.frame(cbind(subj, y, t))
  names(X) <- c("subj", "y", "t")
  
  for (i in 1:(length(subj.uni))) {
    iX <- X[X$subj == subj.uni[i], ]
    io <- order(iX$t)
    lines(iX$t[io], iX$y[io], ...)
  }
  
}
