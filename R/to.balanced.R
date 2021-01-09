#' Transform data to the longitudinal balanced format
#' 
#' @description Transforms a longitudinal data set in the unbalanced format to
#'   the balanced format.
#' 
#' @param data a data frame with longitudinal data in the unbalanced format. 
#'   That is, in the format of 'one row per observation'.
#' @param id.col a column number, or column name, in the data frame \code{data},
#'   where the patient identifier is located.
#' @param time.col a column number, or column name, in the data frame 
#'   \code{data}, where the time measurements are.
#' @param Y.col a vector of column numbers, or column names, of longitudinal 
#'   variables, and/or time dependent covariates in the data frame \code{data}.
#' @param other.col a vector of column numbers, or column names, of baseline 
#'   covariates, and/or other subject level data, as for example, survival data.
#'   Default does not include \code{other.col}.
#'   
#' @author Ines Sousa
#' @seealso \code{\link{to.unbalanced}}.
#' @keywords data manip
#'   
#' @return A data frame with longitudinal data in the balanced format. The
#'   balanced format is considered in this context as the format where each row
#'   has data on each subject. Notice that in this format we will have multiple
#'   columns for the same longitudinal variable, each corresponding to the
#'   variable observed at each time point.
#' @export
#' 
#' @examples
#' simul <- data.frame(num = 1:10,
#'                     Y1.1 = rnorm(10), Y1.2 = rnorm(10),
#'                     Y2.1 = rnorm(10), Y2.2 = rnorm(10),
#'                     age = rnorm(10))
#' simul <- to.unbalanced(simul, id.col = 1, times = c(1, 2), 
#'                        Y.col = 2:5, other.col = 6)
#' simul <- to.balanced(simul, id.col = "num", time.col = "time",
#'                      Y.col = c("Y1.1", "Y2.1"), other.col = "age")
to.balanced <- function(data, id.col, time.col, Y.col, other.col = NA) {
  
  if (length(id.col) > 1) {
    stop("Only a single vector of subject identification is possible")
  }
  if (is.numeric(id.col)) {
    pat <- data[, id.col]
  } else {
    pat <- data[[id.col]]
    id.col <- which(names(data) %in% id.col)
  }
  pat <- unique(pat)
  if (length(time.col) > 1) {
    stop("Only a single vector of time identification is possible")
  }
  if (is.numeric(time.col)) {
    times <- data[, time.col]
  } else {
    times <- data[[time.col]]
    time.col <- which(names(data) %in% time.col)
  }
  times <- unique(times)
  ltt <- length(times)
  y <- c()
  logother <- !identical(NA, other.col)
  if (logother) {
    if (!is.numeric(other.col)) {
      other.col <- which(names(data) %in% other.col)
    }
    other <- matrix(nrow = length(pat), ncol = length(other.col))
  }
  if (!is.numeric(Y.col)) {
    Y.col <- which(names(data) %in% Y.col)
  }
  nY <- length(Y.col)
  
  for (i in 1:(length(pat))) {
    id <- data[, id.col] == pat[i]
    yid <- as.data.frame(data[id, Y.col])
    tid <- data[id, time.col]
    Yid <- as.data.frame(matrix(NA, ncol = dim(yid)[2], nrow = ltt))
    Yid[times %in% tid, ] <- yid
    if (logother) {
      other.id <- as.data.frame(data[id, other.col])
      other[i, ] <- as.vector(apply(other.id, 2, unique))
    }
    y <- c(y, as.vector(unlist(c(Yid))))
  }
  
  Y.t <- as.data.frame(matrix(y, ncol = length(times) * nY, byrow = TRUE))
  
  for (j in 1:nY) {
    names(Y.t)[(ltt * (j - 1) + 1):(ltt * j)] <- paste0((names(data)[Y.col])[j], 
                                                       ".t", times)
  }
  
  data.trans <- data.frame(pat, Y.t)
  names(data.trans)[1] <- names(data)[id.col]
  
  if (logother) {
    other <- as.data.frame(other)
    names(other) <- names(data)[other.col]
    data.trans <- data.frame(cbind(data.trans, other))
  }
  
  return(data.trans)
  
}
