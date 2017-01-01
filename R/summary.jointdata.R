summary.jointdata <- function (object, ...) {
  
  out <- as.list(rep(NA, 5))
  names(out) <- c("subjects", "longitudinal", "survival", "baseline", "times")
  out[[1]] <- paste("Number of subjects: ", length(object$subject), sep = "")
  
  if (any(is.na(object$longitudinal)) & !is.data.frame(object$longitudinal)) {
    out[[2]] <- paste0("No longitudinal data available")
  } else {
    out[[2]] <- as.data.frame(matrix(0, ncol = 1, 
                                     nrow = dim(object$longitudinal)[2] - 1))
    names(out[[2]]) <- c("class")
    row.names(out[[2]]) <- names(object$longitudinal)[2:(dim(object$longitudinal)[2])]
    for (j in 2:(dim(object$longitudinal)[2])) {
      out[[2]][j - 1, 1] <- class(object$longitudinal[, j])
    }
  }
  if (any(is.na(object$survival)) & !is.data.frame(object$survival)) {
    out[[3]] <- paste0("No survival data available")
  } else {
    nn <- names(which(lapply(apply(object$survival, 2, unique), 
                             FUN = function(x) length(x) <= 2)))
    out[[3]] <- paste("There are ", sum(object$survival[[nn]]), 
                      " subjects that fail", "; there are ", length(object$subject) - 
                        sum(object$survival[[nn]]), " subjects censored", 
                      sep = "")
  }
  if (any(is.na(object$baseline)) & !is.data.frame(object$baseline)) {
    out[[4]] <- paste0("No baseline covariates data available")
  } else {
    out[[4]] <- as.data.frame(matrix(0, ncol = 1, nrow = dim(object$baseline)[2] - 1))
    names(out[[4]]) <- c("class")
    row.names(out[[4]]) <- names(object$baseline)[2:(dim(object$baseline)[2])]
    for (j in 2:(dim(object$baseline)[2])) {
      out[[4]][j - 1, 1] <- class(object$baseline[, j])
    }
  }
  
  if (is.na(object$time)) {
    out[[5]] <- paste0("No longitudinal data available")
  } else {
    tt <- sort(unique(object$longitudinal[[object$time]]))
    if (length(tt) > 20) {
      out[[5]] <- paste0("Unbalanced longitudinal study or more than twenty observation times")
    } else {
      out[[5]] <- tt
    }
  }
  
  return(out)
  
}
