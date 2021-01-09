#' Subsetting object of class \code{jointdata}
#' 
#' @description Returns an object of class \code{jointdata} which is a subset of
#'   an original object of class \code{jointdata}.
#' 
#' @param x an object of class \code{jointdata}.
#' @param subj.subset vector of subject identifiers, to include in the data 
#'   subset. This must be a unique vector of patient identifiers.
#' @param ... further arguments to be passed to or from other methods.
#'   
#' @author Ines Sousa
#' @keywords data
#'   
#' @return An object of class \code{jointdata}, with data only on a subset of
#'   subjects.
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
#' take <- heart.jd$survival$num[heart.jd$survival$status == 0]
#' heart.jd.cens <- subset(heart.jd, take)
subset.jointdata <- function(x, subj.subset, ...) {
  
  if (!inherits(x, "jointdata")) {
    stop("Data must be of class 'jointdata'\n")
  }
  
  id <- subj.subset
  re <- x
  re$subject <- re$subject[re$subject %in% id]
  
  if (is.data.frame(re$longitudinal)) {
    re$longitudinal <- re$longitudinal[re$longitudinal[[re$subj.col]] %in% id, ]
    row.names(re$longitudinal) <- 1:(dim(re$longitudinal)[1])
  }
  if (is.data.frame(re$survival)) {
    re$survival <- re$survival[re$survival[[re$subj.col]] %in% id, ]
    row.names(re$survival) <- 1:(dim(re$survival)[1])
  }
  if (is.data.frame(re$baseline)) {
    re$baseline <- re$baseline[re$baseline[[re$subj.col]] %in% id, ]
    row.names(re$baseline) <- 1:(dim(re$baseline)[1])
  }
  
  class(re) <- c("jointdata", "list")
  return(re)
  
}
