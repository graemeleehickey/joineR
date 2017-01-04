#' Creates an object of class \code{jointdata}
#' 
#' This function creates an object of class \code{jointdata}. This is an object
#' with information on at least one of, longitudinal data or survival data.
#' Moreover, it can also have data on baseline covariates.
#' 
#' @param longitudinal a data frame or matrix in the unbalanced format (one row
#' per observation), with subject identification, time of measurements, and
#' longitudinal measurements and/or time dependent covariates. This must be
#' given if no \code{survival} argument is.
#' @param survival a data frame or matrix with survival data for all the
#' subjects. This must be given if no \code{longitudinal} argument is.
#' @param baseline a data frame or matrix with baseline covariates, or non-time
#' dependent covariates, for the same subjects as in \code{survival} and/or
#' \code{longitudinal}. This has to be in the balanced format (one row per
#' subject). By default an object of this class does not include baseline
#' covariates.
#' @param id.col an element of class \code{character} with the name
#' identification of subject. This is to identify the subject identification in
#' the data frames.
#' @param time.col an element of class \code{character} with the time
#' measurements identification. This is to identify the time column in the data
#' frames.
#' 
#' @details This function creates an object of class \code{jointdata}. This is a list
#' with elements used in joint modelling, mainly longitudinal and/or survival
#' data. The output has to have at least one of the data sets, longitudinal or
#' survival. However, for joint modelling is necessary to have both data sets.
#' Moreover, a third data frame is possible to be given as input, for the
#' baseline (non-time dependent) covariates. The subject identification and
#' time measurement column names are necessary.
#' 
#' @author Ines Sousa (\email{isousa@@math.uminho.pt})
#' @keywords survival
#' 
#' @return A list of length six. The first element is the
#' vector of subjects identification. The second is, if exists a data frame of
#' the longitudinal data. The third element of the list is, if exists a data
#' frame of the survival data. The fourth element of the list is, if exists a
#' data frame on the baseline covariates. The fifth is, if longitudinal data is
#' given, the column name identification of longitudinal times. And the sixth
#' and last element of the list is the column name identification of subjects.
#' @export
#' 
#' @examples
#' 
#' data(heart.valve)
#' heart.surv <- UniqueVariables(heart.valve,
#'                               var.col = c("fuyrs", "status"), 
#'                               id.col = "num")
#' heart.valve.jd <- jointdata(survival = heart.surv, 
#'                             id.col = "num",
#'                             time.col = "time")
jointdata <- function(longitudinal = NA, survival = NA, baseline = NA, id.col = "ID", 
                      time.col = NA) {
  
  # Checks
  if (!(is.matrix(longitudinal) | is.data.frame(longitudinal)) & 
      notNA(longitudinal)) {
    stop("longitudinal object must be a matrix or a data.frame")
  }
  
  if (!(is.matrix(survival) | is.data.frame(survival)) & notNA(survival)) {
    stop("survival object must be a matrix or a data.frame")
  }
  
  if (!(is.matrix(baseline) | is.data.frame(baseline) | is.character(baseline)) & 
      notNA(baseline)) {
    stop("baseline object must be a matrix or a data.frame or a vector of names of baseline covariates")
  }
  
  if (is.na(id.col)) {
    stop("It is necessary to specify an subject identification column name")
  }
  
  nm <- names(which(!is.na(list(longitudinal = longitudinal, survival = survival))))
  if (length(nm) == 0) {
    stop("Longitudinal and Survival data not available")
  }
  
  if (length(nm) == 1) {
    if (!id.col %in% names(get(nm))) {
      stop(paste("ID column does not exist in ", nm, " object", sep = ""))
    }
    pp <- (get(nm))[[id.col]]
    patid <- unique(pp)
    if (nm == "survival") {
      if (length(patid) != dim(survival)[1]) {
        stop("Same patient with different survival data")
      }
    }
    if (!(any(is.na(baseline)))) {
      if (!id.col %in% names(baseline)) {
        stop("ID column does not exist in baseline object")
      }
      bb <- unique(baseline[[id.col]])
      if (sum(order(patid) != order(bb)) > 0) {
        stop("Number of subjects different in the baseline data frame")
      }
      if (length(bb) != dim(baseline)[1]) {
        stop("Same patient with different covariate data")
      }
    }
  }
  
  if (length(nm) == 2) {
    if (!id.col %in% names(longitudinal)) {
      stop("ID column does not exist in longitudinal object")
    }
    pp <- longitudinal[[id.col]]
    patid <- unique(pp)
    if (!id.col %in% names(survival)) {
      stop("ID column does not exist in survival object")
    }
    ss <- unique(survival[[id.col]])
    if (sum(order(patid) != order(ss)) > 0) {
      stop("Number of subjects different in the longitudinal and survival data frames")
    }
    if (length(ss) != dim(survival)[1]) {
      stop("Same patient with different survival data")
    }
    if (!(any(is.na(baseline)))) {
      if (!id.col %in% names(baseline)) {
        stop("ID column does not exist in baseline object")
      }
      bb <- unique(baseline[[id.col]])
      if (sum(order(patid) != order(bb)) > 0) {
        stop("Number of subjects different in the longitudinal and covariates data frame")
      }
      if (length(bb) != dim(baseline)[1]) {
        stop("Same patient with different covariate data")
      }
    }
  }
  
  # Create joint data object
  npat <- length(patid)
  new <- list(subject = NA, longitudinal = NA, survival = NA, 
              baseline = NA, time.col = NA, subj.col = NA)
  new[["subject"]] <- patid
  new[["subj.col"]] <- id.col
  
  if (notNA(longitudinal)) {
    if (!id.col %in% names(longitudinal)) {
      stop("ID column name does not exist in longitudinal object")
    }
    
    if (!time.col %in% names(longitudinal)) {
      stop("Time column does not exist in longitudinal object")
    }
    
    longitudinal <- longitudinal[order(longitudinal[[id.col]]), ]
    row.names(longitudinal) <- 1:(dim(longitudinal)[1])
    
    for (i in 1:(npat)) {
      tmp <- longitudinal[[id.col]] == patid[i]
      lt <- longitudinal[tmp, ]
      longitudinal[tmp, ] <- (lt)[order(lt[[time.col]]), ]
    }
    
    row.names(longitudinal) <- 1:(dim(longitudinal)[1])
    new[["longitudinal"]] <- longitudinal
    new[["time.col"]] <- time.col
  }
  
  if (notNA(survival)) {
    if (!id.col %in% names(survival)) {
      stop("ID column name does not exist in survival object")
    }
    survival <- survival[order(survival[[id.col]]), ]
    row.names(survival) <- 1:(dim(survival)[1])
    new[["survival"]] <- survival
  }
  
  if (notNA(baseline)) {
    if (!id.col %in% names(baseline)) {
      stop("ID column name does not exist in baseline object")
    }
    baseline <- baseline[order(baseline[[id.col]]), ]
    row.names(baseline) <- 1:(dim(baseline)[1])
    new[["baseline"]] <- baseline
  }
  
  class(new) <- c("jointdata", "list")
  return(new)
  
}
