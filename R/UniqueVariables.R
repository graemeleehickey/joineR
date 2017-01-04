#' Extracts the unique non-time dependent variables per patient, from an 
#' unbalanced data set
#' 
#' This function extracts a set of unique variables within a patient, returning 
#' a data frame with columns, patient identification and variables selected. 
#' Each row corresponds to the data for each individual.
#' 
#' @param data data frame, or matrix, with at least a column of patient 
#'   identification and a covariate column.
#' @param var.col vector of column names or column numbers, of the variables 
#'   (non-time dependent). Cannot have mix of numbers and column names.
#' @param id.col column name or column number of the patient identification.
#'   
#' @details This function can be used, when longitudinal data is in the
#'   unbalanced format, and it is necessary, for example, to extract the set of
#'   unique baseline covariates, or any non-time dependent variables, that in
#'   the unbalanced format, are repeated for each observation row. Also, if the 
#'   original data frame has survival data, this can also be used to extract the
#'   survival information from the original data set.
#'   
#' @author Ines Sousa (\email{isousa@@uminho.pt})
#' @keywords data manip
#'   
#' @return A data frame with patient identification and covariates selected.
#'   Each row corresponds to the data for each individual. Note that, this can
#'   be only used for non-time dependent covariates. If extracting unique time
#'   dependent covariates, the function gives an error, because it can't select
#'   what is the unique covariate.
#' @export
#' 
#' @examples
#' 
#' data(heart.valve)
#' heart.cov <- UniqueVariables(heart.valve,
#'                              c(2, 3, 5, 6, 12:25),
#'                              id.col = "num")
UniqueVariables <- function(data, var.col, id.col = "ID") {
  
  patid <- unique(data[[id.col]])
  npat <- length(patid)
  n.cov <- length(var.col)
  n.col <- n.cov + 1
  new <- matrix(ncol = n.col, nrow = npat)
  new <- as.data.frame(new)
  
  if (is.numeric(id.col)) {
    names(new)[1] <- names(data)[id.col]
  } else {
    names(new)[1] <- id.col
  }
  
  if (is.numeric(var.col)) {
    names(new)[2:n.col] <- names(data)[var.col]
  } else {
    names(new)[2:n.col] <- var.col
  }
  
  id.col <- names(new)[1]
  var.col <- names(new)[2:n.col]
  new <- new[, c(1, order(names(new)[2:n.col]) + 1)]
  
  for (i in 1:n.col) {
    tt <- names(new)[i]
    if (class(data[[tt]]) == "factor") {
      new[[tt]] <- as.factor(new[[tt]])
      levels(new[[tt]]) <- levels(data[[tt]])
    } else {
      class(new[[tt]]) <- class(data[[tt]])
    }
  }
  var.col <- names(new)[2:n.col]
  new[[id.col]] <- patid
  
  for (i in 1:(npat)) {
    data.i <- data[data[[id.col]] == patid[i], names(data) %in% var.col]
    if (length(var.col) == 1) {
      names(data.i) <- var.col
      data.i <- data.i[order(names(data.i))]
      tt.i <- unique(data.i)
    } else {
      data.i <- data.i[, order(names(data.i))]
      tt.i <- apply(data.i, 2, unique)
    }
    if (sum(unlist(lapply(tt.i, function(x) length(x) > 1))) > 0) {
      stop("No consistency on the variables information")
    }
    if (length(var.col) == 1) {
      new[new[[id.col]] == patid[i], -which(names(new) == id.col)] <- data.i[1]
    } else {
      new[new[[id.col]] == patid[i], -which(names(new) == id.col)] <- data.i[1, ]
    }
  }
  
  return(new)
  
}
