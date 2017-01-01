\name{summarybal}

\alias{summarybal}

\title{Summary of a balanced longitudinal data set}

\description{For a balanced longitudinal data set a vector of the mean response and variances at defined time points is returned along with the correlation matrix of the responses across the time points. 
}

\usage{summarybal(object, Y.col, times, use = "all.obs", na.rm, ...)
}

\arguments{
  \item{object}{a longitudinal data set in the balanced format.}
  \item{Y.col}{the column numbers of the longitudinal measurements at each design time point in the \code{object}. This does not have to be all of the longitudinal measurements taken and may be a subset instead.}
  \item{times}{a vector of unique time points of the longitudinal measurements. This does not have to be all of the study time points and may be a subset instead, but should match the columns defined in \code{Y.col}}
  \item{use}{an optional character string giving a method for computing
    covariances in the presence of missing values. This must be (an
    abbreviation of) one of the strings \code{"all.obs"}, \code{"complete.obs"} or
    \code{"pairwise.complete.obs"}. Defaults to \code{use = "all.obs"}}
  \item{na.rm}{logical. Should missing values be removed? By default, \code{na.rm = FALSE}.}
  \item{\ldots}{further arguments for the summary}
}

\value{The function returns a \code{list} with three elements: \code{mean.vect} - a matrix with the time points in the first column and the mean response vector as the second column; \code{variance} - the vector of variances for the response at the time points and \code{cor.mtx} containing the correlation matrix of the responses between each pair of time points.}

\author{Ines Sousa (isousa@math.uminho.pt)}
\seealso{\code{to.balanced}.}
\examples{
data(mental)
summarybal(mental, Y.col = 2:7, times = c(0, 1, 2, 4, 6, 8), na.rm = TRUE)
}

\keyword{mean}
