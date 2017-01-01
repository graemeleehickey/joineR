\name{plot.vargm}

\alias{plot.vargm}

\title{Plots the empirical variogram for longitudinal data}

\description{Plots the empirical variogram for observed measurements, of
  an object of class 'vargm', obtained by using function \code{\link{variogram}}.
}

\usage{
\method{plot}{vargm}(x, smooth = FALSE, bdw = NULL, follow.time = NULL, points = TRUE, \dots)
}

\arguments{
  \item{x}{object of class \code{vargm} obtained by using function \code{\link{variogram}}}
  \item{smooth}{Logical value to use a non-parametric estimator to
    calculate the variogram of all $v_{ijk}$. The default is FALSE, as
    it uses time averages}
  \item{bdw}{bandwidth to use in the time averages. The default is
    \code{NULL}, because this is calculated automatically.}
  \item{follow.time}{the interval of time we want to construct the
    variogram for. When \code{NULL} this is the maximum of the data}
  \item{points}{Logical value if the points $v_{ijk}$ should be plotted}
  \item{\ldots}{other graphical options as in \code{par}}
}

\value{The function returns a graphical device with the plot of
  empirical variogram}

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
plot(vgm)
}

\keyword{variogram}
