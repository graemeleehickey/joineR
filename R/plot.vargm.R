#' Plots the empirical variogram for longitudinal data
#' 
#' Plots the empirical variogram for observed measurements, of an object of
#' class 'vargm', obtained by using function \code{\link{variogram}}.
#' 
#' @param x object of class \code{vargm} obtained by using function.
#' \code{\link{variogram}}
#' @param smooth logical value to use a non-parametric estimator to calculate
#' the variogram of all \eqn{v_ijk}. The default is \code{FALSE}, as it uses time averages.
#' @param bdw bandwidth to use in the time averages. The default is
#' \code{NULL}, because this is calculated automatically.
#' @param follow.time the interval of time we want to construct the variogram
#' for. When \code{NULL} this is the maximum of the data.
#' @param points logical value if the points $v_ijk$ should be plotted.
#' @param list() other graphical options as in \code{\link[graphics]{par}}.
#' 
#' @author Ines Sousa (\eail{isousa@@math.uminho.pt})
#' @keywords variogram
#' 
#' @return A graphical device with the plot of empirical
#' variogram.
#' @export
#' 
#' @examples
#' 
#' data(mental)
#' mental.unbalanced <- to.unbalanced(mental, id.col = 1, 
#'                                    times = c(0, 1, 2, 4, 6, 8),
#'                                    Y.col = 2:7, 
#'                                    other.col = c(8, 10, 11))
#' names(mental.unbalanced)[3] <- "Y"
#' vgm <- variogram(indv = tail(mental.unbalanced[, 1], 30),
#'                  time = tail(mental.unbalanced[, 2], 30),
#'                  Y = tail(mental.unbalanced[, 3], 30))
#' plot(vgm)
plot.vargm <- function(x, smooth = FALSE, bdw = NULL, 
                       follow.time = NULL, points = TRUE, ...) {
  
  vargm <- x
  svar <- vargm$svar
  sigma2 <- vargm$sigma2
  
  if (is.null(follow.time)) {
    left <- min(svar[, 1])
    right <- max(svar[, 1])
  } else {
    left <- follow.time[1]
    right <- follow.time[2]
  }
  
  if (is.null(bdw)) {
    bdw <- (right - left)/10
  }
  
  nbdw <- (right - left)/bdw
  Mid <- c()
  Mean <- c()
  Count <- c()
  
  for (it in 1:(nbdw)) {
    lt <- left + (it - 1) * bdw
    rt <- left + it * bdw
    mid <- lt + (rt - lt)/2
    if (length(svar[svar[, 1] < rt & svar[, 1] >= lt, 2]) > 0) {
      mean <- mean(svar[svar[, 1] < rt & svar[, 1] >= lt, 2])
      Count <- c(Count, length(svar[svar[, 1] < rt & svar[, 1] >= lt, 2]))
    } else {
      mean <- NA
      Count <- c(Count, 0)
    }
    Mid <- c(Mid, mid)
    Mean <- c(Mean, mean)
  }
  mean.line <- cbind(Mid[!is.na(Mean)], Mean[!is.na(Mean)])
  Count <- Count[!is.na(Mean)]
  
  if (points) {
    plot(svar[, 1], svar[, 2], xlab = "u", ylab = "Variogram", pch = ".", ...)
    if (smooth) {
      lines(smooth.spline(svar[, 1], svar[, 2]), lty = 1, lwd = 1.3)
    } else {
      lines(mean.line, lwd = 1.3, lty = 1)
    }
    abline(h = sigma2, lwd = 1.3, lty = 1)
  } else {
    if (smooth) {
      plot(smooth.spline(svar[, 1], svar[, 2]), xlab = "u", ylab = "Variogram", ...)
    } else {
      plot(mean.line, type = "l", xlab = "u", ylab = "Variogram", ...)
    }
    abline(h = sigma2, lwd = 2, lty = 1)
    abline(h = min(mean.line[mean.line[, 1] <= right & 
                               mean.line[, 1] >= left, 2]), lty = 2)
    abline(h = max(mean.line[mean.line[, 1] <= right & 
                               mean.line[, 1] >= left, 2]), lty = 2)
  }
  
}
