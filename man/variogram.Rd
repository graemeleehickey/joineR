\name{variogram}

\alias{variogram}

\title{Empirical variogram for longitudinal data}

\description{Calculates the variogram for observed measurements, with two
  components, the total variability in the data, and the variogram for
  all time lags in all individuals.
}

\usage{variogram(indv, time, Y)
}

\arguments{
  \item{indv}{vector of individual identification, as in the
    longitudinal data, repeated for each time point}
  \item{time}{vector of observation time, as in the longitudinal data}
  \item{Y}{vector of observed measurements. This can be a vector of
    longitudinal data, or residuals after fitting a model for the mean
    response}
}

\details{The empirical variogram in this function is calculated from
  observed half-squared-differences between pairs of measurements,
  $$v_{ijk} = 0.5 * (r_{ij}-r_{ik})^2$$
  and the corresponding time differences
  $$u_{ijk}=t_{ij}-t_{ik}$$. The variogram is plotted for averages of
  each time lag for the $v_{ijk}$ for all $i$
}

\value{The function returns a list with two elements. The first
  \code{svar} is a matrix with columns for all values
  $(u_{ijk},v_{ijk})$, and the second \code{sigma2} is the total
  variability in the data. This is an object of class \code{vargm}}

\author{Ines Sousa (isousa@math.uminho.pt)}

\examples{
data(mental)
mental.unbalanced <- to.unbalanced(mental, id.col = 1, 
                                   times = c(0,1,2,4,6,8),
                                   Y.col = 2:7, 
                                   other.col = c(8,10,11))
names(mental.unbalanced)[3] <- "Y"
vgm <- variogram(indv = tail(mental.unbalanced[, 1], 30),
                       time = tail(mental.unbalanced[, 2], 30),
                       Y = tail(mental.unbalanced[, 3], 30))
}

\note{
There is a function \code{\link{plot.vargm}} which should be used to plot
the empirical variogram.
}

\keyword{variogram}
