#' Summarise a \code{jointdata} object
#' 
#' @description Generic function used to produce summaries of objects of class 
#'   \code{jointdata}.
#' 
#' @inheritParams summary.joint
#'   
#' @author Ines Sousa
#' @seealso \code{\link{jointdata}}, \code{\link{UniqueVariables}}.
#' @keywords methods
#'   
#' @return A list with five elements. Each summarises an element of the
#'   \code{jointdata} object:
#'   
#'   \item{\code{subjects}}{Gives the number of subjects in the data set.}
#'   
#'   \item{\code{longitudinal}}{If longitudinal data is available, it gives the
#'   names and class, of the longitudinal variables.}
#'   
#'   \item{\code{survival}}{If survival data is available, it gives the number
#'   of subjects with failure and censored survival times.}
#'   
#'   \item{\code{baseline}}{If baseline covariates is available, it gives the
#'   names and class, of the baseline covariates.}
#'   
#'   \item{\code{times}}{If longitudinal data is available, it gives the unique
#'   longitudinal time measurements, if it is a balanced study. In case of
#'   unbalanced study, it will only state it is an unbalanced study.}
#' @export
#' 
#' @examples
#' data(heart.valve)
#' heart.surv <- UniqueVariables(heart.valve,
#'                               var.col = c("fuyrs", "status"), 
#'                               id.col = "num")
#' heart.valve.jd <- jointdata(survival = heart.surv, 
#'                             id.col = "num",
#'                             time.col = "time")
#' summary(heart.valve.jd)
summary.jointdata <- function(object, ...) {
  
  if (!inherits(object, "jointdata")) {
    stop("Data must be of class 'jointdata'\n")
  }
  
  out <- as.list(rep(NA, 5))
  names(out) <- c("subjects", "longitudinal", "survival", "baseline", "times")
  out[[1]] <- paste("Number of subjects:", length(object$subject))
  
  if (any(is.na(object$longitudinal)) & !is.data.frame(object$longitudinal)) {
    out[[2]] <- paste0("No longitudinal data available")
  } else {
    out[[2]] <- as.data.frame(
      matrix(0, ncol = 1, nrow = dim(object$longitudinal)[2] - 1))
    names(out[[2]]) <- c("class")
    row.names(out[[2]]) <- names(object$longitudinal)[2:(dim(object$longitudinal)[2])]
    for (j in 2:(dim(object$longitudinal)[2])) {
      out[[2]][(j - 1), 1] <- class(object$longitudinal[, j])
    }
  }
  
  if (any(is.na(object$survival)) & !is.data.frame(object$survival)) {
    out[[3]] <- paste0("No survival data available")
  } else {
    nn <- names(which(lapply(apply(object$survival, 2, unique), length) <= 3))
    if (length(unique(object$survival[[nn]])) %in% c(1L, 2L)) {
      out[[3]] <- data.frame(c(sum(object$survival[[nn]]), 
                               sum(object$survival[[nn]] == 0)))
      row.names(out[[3]]) <- c("Number of subjects that fail:",
                               "Number of subjects censored:")
      } else {
        out[[3]] <- data.frame(c(sum(object$survival[[nn]] == 1),
                                 sum(object$survival[[nn]] == 2),
                                 sum(object$survival[[nn]] == 0)))
        row.names(out[[3]]) <- c("Number of subjects that fail due to cause 1:",
                                 "Number of subjects that fail due to cause 2:",
                                 "Number of subjects censored:")
    }
    colnames(out[[3]]) <- NULL
  }
  
  if (any(is.na(object$baseline)) & !is.data.frame(object$baseline)) {
    out[[4]] <- paste0("No baseline covariates data available")
  } else {
    out[[4]] <- as.data.frame(matrix(0, ncol = 1, nrow = dim(object$baseline)[2] - 1))
    names(out[[4]]) <- c("class")
    row.names(out[[4]]) <- names(object$baseline)[2:(dim(object$baseline)[2])]
    for (j in 2:(dim(object$baseline)[2])) {
      out[[4]][(j - 1), 1] <- class(object$baseline[, j])
    }
  }
  
  if (is.na(object$time)) {
    out[[5]] <- "No longitudinal data available"
  } else {
    tt <- sort(unique(object$longitudinal[[object$time]]))
    if (length(tt) > 20) {
      out[[5]] <- "Unbalanced longitudinal study or more than twenty observation times"
    } else {
      out[[5]] <- tt
    }
  }
  
  return(out)
  
}
