#' Empirical variogram for longitudinal data
#' 
#' @description Calculates the variogram for observed measurements, with two
#'   components, the total variability in the data, and the variogram for all
#'   time lags in all individuals.
#' 
#' @param indv vector of individual identification, as in the longitudinal data,
#'   repeated for each time point.
#' @param time vector of observation time, as in the longitudinal data.
#' @param Y vector of observed measurements. This can be a vector of 
#'   longitudinal data, or residuals after fitting a model for the mean
#'   response.
#'   
#' @details The empirical variogram in this function is calculated from observed
#'   half-squared-differences between pairs of measurements, \eqn{v_ijk = 0.5 *
#'   (r_ij-r_ik)^2} and the corresponding time differences
#'   \eqn{u_ijk=t_ij-t_ik}. The variogram is plotted for averages of each time
#'   lag for the \eqn{v_ijk} for all \eqn{i}.
#'   
#' @note There is a function \code{\link{plot.vargm}} which should be used to 
#'   plot the empirical variogram.
#'   
#' @author Ines Sousa (\email{isousa@@math.uminho.pt})
#' @keywords smooth models
#'   
#' @return An object of class \code{vargm} and \code{list} with two elements.
#'   The first \code{svar} is a matrix with columns for all values
#'   \eqn{(u_ijk,v_ijk)}, and the second \code{sigma2} is the total variability
#'   in the data.
#' @export
#' 
#' @examples
#' data(mental)
#' mental.unbalanced <- to.unbalanced(mental, id.col = 1, 
#'                                    times = c(0, 1, 2, 4, 6, 8),
#'                                    Y.col = 2:7, 
#'                                    other.col = c(8, 10, 11))
#' names(mental.unbalanced)[3] <- "Y"
#' 
#' vgm <- variogram(indv = tail(mental.unbalanced[, 1], 30),
#'                  time = tail(mental.unbalanced[, 2], 30),
#'                  Y = tail(mental.unbalanced[, 3], 30))
variogram <- function(indv, time, Y) {
  
    id <- as.vector(indv[!is.na(Y)])
    time <- as.vector(time[!is.na(Y)])
    y <- as.vector(Y[!is.na(Y)])
    subject <- unique(id)
    m <- length(subject)
    vv <- c()
    vt <- c()
    vtot <- c()
    
    for (i in 1:m) {
        j1 <- (id == subject[i])
        rr1 <- y[j1]
        tt <- time[j1]
        dr <- outer(rr1, rr1, function(x, y) {
            0.5 * (x - y)^2
        })
        vv <- c(vv, dr[upper.tri(dr)])
        dt <- outer(tt, tt, function(x, y) {
            abs(x - y)
        })
        vt <- c(vt, dt[upper.tri(dt)])
        l <- i + 1
        while (l <= m) {
            j2 <- id == subject[l]
            rr2 <- y[j2]
            dtot <- outer(rr1, rr2, function(x, y) {
                0.5 * (x - y)^2
            })
            vtot <- c(vtot, c(dtot))
            l <- l + 1
        }
    }
    
    svar <- cbind(vt, vv)
    sigma2 <- mean(vtot)
    vrgm <- list(svar = svar, sigma2 = sigma2)
    
    class(vrgm) <- c("vargm", "list")
    return(vrgm)
    
}
