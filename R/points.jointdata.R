#' Add points to an existing \code{jointdata} plot
#' 
#' Add points to an existing plot of an object of class \code{jointdata}, for a 
#' longitudinal variable. It is possible plot all the subjects in the data set, 
#' or just a selected \code{subset}. See \code{\link{subset.jointdata}}.
#' 
#' @inheritParams plot.jointdata
#'   
#' @author Ines Sousa (\email{isousa@@math.uminho.pt})
#' @keywords aplot
#'   
#' @return A graphical device with a plot for longitudinal data. Other functions
#'   are useful to be used with this as \code{\link[graphics]{plot}} and 
#'   \code{\link[graphics]{lines}}.
#' @import graphics
#' @export
#' 
#' @examples
#' 
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
#' take <- sample(1 : max(heart.jd$survival$num), 2)
#' heart.jd.1 <- subset(heart.jd, take[1])
#' heart.jd.2 <- subset(heart.jd, take[2])
#' 
#' plot(heart.jd.1, Y.col = "grad", type = "p")
#' points(heart.jd.2, Y.col = "grad", col = "blue", pch = 20)
points.jointdata <- function(x, Y.col, ...) {
  
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
  ix <- !is.na(resp)
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
    points(iX$t[io], iX$y[io], ...)
  }
  
}
