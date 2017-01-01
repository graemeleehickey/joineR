\name{plot.jointdata}

\alias{plot.jointdata}

\title{Plot longitudinal data}

\description{ Plot longitudinal data of an object of class
  \code{jointdata}, for a longitudinal variable. It is possible to
  plot all the subjects in the data set, or just a selected
  \code{subset}. See \code{\link{subset.jointdata}}.}

\usage{
\method{plot}{jointdata}(x, Y.col, type, xlab, xlim, ylim, main, pty, \dots)
}

\arguments{
  \item{x}{object of class \code{jointdata}}
  \item{Y.col}{column number, or column name, of longitudinal variable
    to be plotted. Defaults to \code{Y.col = NA}, 
    plotting all longitudinal variables}
  \item{type}{the type of line to be plotted, see \code{plot} for further details.}
  \item{xlab}{a title for the x-axis, see \code{title}.}
  \item{xlim, ylim}{numeric vectors of length 2, giving the x and y coordinates ranges, see \code{plot.window} for further details.}
  \item{main}{an overall title for the plot: see \code{title}.}
  \item{pty}{A character specifying the type of plot region to be used, see \code{par} for details.}
  \item{\ldots}{other graphical arguments (see \code{plot})}
}

\value{ A graphical device with a plot for longitudinal data. Other
  functions are useful to be used with this as \code{\link{lines}} and
  \code{\link{points}}
  }

\author{Ines Sousa (isousa@math.uminho.pt)}

\examples{
data(heart.valve)
heart.surv <- UniqueVariables(heart.valve, var.col = c("fuyrs", "status"), 
      id.col = "num")
heart.long <- heart.valve[, c(1, 4, 5, 7, 8, 9, 10, 11)]
heart.jd <- jointdata(longitudinal = heart.long, 
      survival = heart.surv, id.col = "num", time.col = "time")
plot(heart.jd, Y.col = "grad", col = "grey")
}

\keyword{plot, longitudinal, jointdata}
