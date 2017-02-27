#' @keywords internal
prepLongData <- function(long.formula, data, id, time.long) {
  
  long.data <- merge(data$longitudinal, data$baseline, by = id, sort = FALSE)
  long.frame <- model.frame(long.formula, data = long.data)
  long.cov <- model.matrix(long.formula, long.frame)
  long.terms <- terms(long.formula, data = long.data)
  long.names <- colnames(long.cov)
  rll <- !is.na(data$longitudinal[[names(long.frame[1])]])
  longdat <- cbind(data$longitudinal[[id]][rll],
                   long.frame[, 1],
                   data$longitudinal[[time.long]][rll],
                   long.cov)
  longdat <- as.data.frame(longdat)
  names(longdat) <- c(id,
                      names(long.frame)[1],
                      time.long,
                      long.names)
  
  out <- list(longdat = longdat,
              long.data = long.data)
  return(out)
  
}