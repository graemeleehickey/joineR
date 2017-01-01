\name{points.jointdata}

\alias{points.jointdata}

\title{Add points to an existing jointdata plot}

\description{Add points to an existing plot of an object of class
  \code{jointdata}, for a longitudinal variable. It is possible
  plot all the subjects in the data set, or just a selected
  \code{subset}. See \code{\link{subset.jointdata}}}

\usage{
\method{points}{jointdata}(x, Y.col, \dots)
}

\arguments{
  \item{x}{object of class \code{jointdata}}
  \item{Y.col}{column number, or column name, of longitudinal variable
    to be plotted}
  \item{\ldots}{other graphical arguments}
}

\value{ A graphical device with a plot for longitudinal data. Other
  functions are useful to be used with this as \code{\link{plot}} and
  \code{\link{lines}}
  }

\author{Ines Sousa (isousa@math.uminho.pt)}

\examples{
data(heart.valve)
heart.surv <- UniqueVariables(heart.valve, var.col = c("fuyrs", "status"), 
      id.col = "num")
heart.long <- heart.valve[, c(1, 4, 5, 7, 8, 9, 10, 11)]
heart.jd <- jointdata(longitudinal = heart.long, 
      survival = heart.surv, id.col = "num", time.col = "time")
# Randomly select a pair of subjects to plot profiles of
take <- sample(1 : max(heart.jd$survival$num), 2)
heart.jd.1 <- subset(heart.jd, take[1])
heart.jd.2 <- subset(heart.jd, take[2])
plot(heart.jd.1, Y.col = "grad", type = "p")
points(heart.jd.2, Y.col = "grad", col = "blue", pch = 20)
}

\keyword{points, longitudinal, jointdata}
