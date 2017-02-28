#' Joint plot of longitudinal and survival data
#' 
#' This function views the longitudinal profile of each unit with the last 
#' longitudinal measurement prior to event-time (censored or not) taken as the 
#' end-point, referred to as time zero. In doing so, the shape of the profile 
#' prior to event-time can be inspected. This can be done over a user-specified 
#' number of time units.
#' 
#' @param object an object of class \code{jointdata}.
#' @param Y.col an element of class \code{character} identifying the 
#'   longitudinal response part of the \code{jointdata} object.
#' @param Cens.col an element of class \code{character} identifying the survival
#'   status or censoring indicator part of the \code{jointdata} object.
#' @param lag argument which specifies how many units in time we look back 
#'   through. Defaults to the maximum observation time across all units.
#' @param split logical argument which allows the profiles of units which 
#'   \emph{fail} and those which are \emph{censored} to be viewed in separate 
#'   panels of the same graph. This is the default option. Using \code{split =
#'   FALSE} will plot all profiles overlaid on a single plot.
#' @param col1 argument to choose the colour for the profiles of the 
#'   \emph{censored} units.
#' @param col2 argument to choose the colour for the profiles of the 
#'   \emph{failed} units.
#' @param xlab an element of class \code{character} indicating the title for the
#'   x-axis.
#' @param ylab an element of class \code{character} indicating the title for the
#'   x-axis.
#' @param gp1lab an element of class \code{character} for the group 
#'   corresponding to a censoring indicator of zero. Typically, the censored 
#'   group.
#' @param gp2lab an element of class \code{character} for the group 
#'   corresponding to a censoring indicator of one. Typically, the group 
#'   experiencing the event of interest.
#' @param smooth the smoother span. This gives the proportion of points in the 
#'   plot which influence the smooth at each value. Defaults to a value of 2/3. 
#'   Larger values give more smoothness. See \code{\link[stats]{lowess}} for 
#'   further details.
#' @param mean.profile draw mean profiles if TRUE. Only applies to the 
#'   \code{split = TRUE} case.
#' @param mcol1 argument to choose the colour for the mean profile of the units 
#'   with a censoring indicator of zero.
#' @param mcol2 argument to choose the colour for the mean profile of the units 
#'   with a censoring indicator of one.
#'   
#' @details The function tailors the \code{\link[lattice]{xyplot}} function to 
#'   produce a representation of joint data with longitudinal and survival 
#'   components.
#'   
#' @note If more than one cause of failure is present (i.e. competing risks
#'   data), then all failures are pooled together into a single failure type.
#'   
#' @author Pete Philipson (\email{pete.philipson@@northumbria.ac.uk})
#' @keywords dplot
#' @seealso \code{\link[lattice]{xyplot}}, \code{\link{joint}}, 
#'   \code{\link{jointdata}}.
#'   
#' @references
#' 
#' Wulfsohn MS, Tsiatis AA. A joint model for survival and longitudinal data 
#' measured with error. \emph{Biometrics.} 1997; \strong{53(1)}: 330-339.
#' 
#' @return A lattice plot.
#' @importFrom lattice xyplot
#' @export
#' 
#' @examples
#' 
#' data(heart.valve)
#' heart.surv <- UniqueVariables(heart.valve, 
#'                               var.col = c("fuyrs", "status"),
#'                               id.col = "num")
#' heart.long <- heart.valve[, c("num", "time", "log.lvmi")]
#' heart.cov <- UniqueVariables(heart.valve, 
#'                              c("age", "sex"), 
#'                              id.col = "num")
#' heart.valve.jd <- jointdata(longitudinal = heart.long, 
#'                             baseline = heart.cov, 
#'                             survival = heart.surv,
#'                             id.col = "num",
#'                             time.col = "time")
#' jointplot(heart.valve.jd, Y.col = "log.lvmi", 
#'           Cens.col = "status", lag = 5)
jointplot <- function(object, Y.col, Cens.col, lag, split = TRUE, 
                      col1, col2, xlab, ylab, gp1lab, gp2lab, 
                      smooth = 2/3, mean.profile = FALSE, mcol1, mcol2) {
  
  if (!inherits(object, "jointdata")) {
    stop("Data must be of class 'jointdata'\n")
  }
  
  if (!is.vector(Y.col) | length(Y.col) > 1) {
    stop("Only one longitudinal response is possible to plot")
  }
  
  longdat <- object$longitudinal #[complete.cases(object$longitudinal), ]
  survdat <- object$survival
  
  if (is.numeric(Y.col)) {
    Y <- longdat[, Y.col]
  } else {
    Y <- longdat[[Y.col]]
    Y.col <- which(names(longdat) %in% Y.col)
  }
  
  t <- longdat[[object$time.col]]
  id <- longdat[[object$subj.col]]
  nobs <- diff(match(unique(id), id))
  nobs[length(nobs) + 1] <- length(id) - sum(nobs)
  index <- cumsum(nobs)
  cens <- survdat[, Cens.col]
  ft <- rep(t[index], nobs)
  t0 <- t - ft
  hue <- length(id)
  
  if (length(unqiue(cens)) == 3) {
    warning("jointplot does not display profiles for different failure events")
  }
  
  if (missing(lag)) {
    lag <- max(t)
  }
  if (missing(col1)) {
    col1 <- "blue"
  }
  if (missing(col2)) {
    col2 <- "red"
  }
  if (missing(xlab)) {
    xlab <- "Time"
  }
  if (missing(ylab)) {
    ylab <- "Y"
  }
  if (missing(gp1lab)) {
    gp1lab <- "Censored"
  }
  if (missing(gp2lab)) {
    gp2lab <- "Failed"
  }
  if (missing(mcol1)) {
    mcol1 <- "black"
  }
  if (missing(mcol2)) {
    mcol2 <- "black"
  }
  if (missing(smooth)) {
    smooth <- 2/3
  }
  
  ii <- (cens == 0)
  hue[ii] <- col1
  hue[!ii] <- col2
  fac <- rep(cens, nobs)
  ii <- (fac == 0)
  fac[ii] <- gp1lab
  fac[!ii] <- gp2lab
  
  if (mean.profile == FALSE) {
    if (split == TRUE) {
      xyplot(Y ~ t0 | fac, groups = id, type = "l", lty = 1, 
             xlim = c(-lag, 0), col = hue, xlab = xlab, ylab = ylab)
    } else {
      xyplot(Y ~ t0, groups = id, type = "l", 
             xlim = c(-lag, 0), col = hue, xlab = xlab, ylab = ylab)
    }
  } 
  else {
    s1 <- lowess(t0[ii], Y[ii], f = smooth)
    s2 <- lowess(t0[!ii], Y[!ii], f = smooth)
    if (!is.character(summary(object)$times)) {
      mean_cens <- unique(s1$y)
      mean_fail <-unique(s2$y)
      t_mean_cens <- unique(s1$x)
      t_mean_fail <- unique(s2$x)
    } else {
      mean_cens <- s1$y
      mean_fail <- s2$y
      t_mean_cens <- seq(min(t0[ii]), max(t0[ii]), length = length(mean_cens))
      t_mean_fail <- seq(min(t0[!ii]), max(t0[!ii]), length = length(mean_fail))
    }
    
    Y <- c(Y,mean_cens, mean_fail)
    fac <- c(fac,rep(gp1lab, length(mean_cens)), rep(gp2lab, length(mean_fail)))
    id <- c(id, rep(max(id) + 1, length(mean_cens)), 
            rep(max(id) + 2, length(mean_fail)))
    t0 <- c(t0, t_mean_cens, t_mean_fail)
    hue <- c(hue, mcol1, mcol2)
    
    if (split == TRUE) {
      xyplot(Y ~ t0 | fac, groups = id, type = "l", lty = 1, 
             lwd = c(rep(1, length(cens)), 2, 2),
             xlim = c(-lag, 0), col = hue, xlab = xlab, ylab = ylab, 
             scales = list(alternating = FALSE))
    } else {
      xyplot(Y ~ t0, groups = id, type = "l", 
             xlim = c(-lag, 0), col = hue, xlab = xlab, ylab = ylab)
    }
  }
  
}
