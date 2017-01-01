\name{sample.jointdata}

\alias{sample.jointdata}

\title{Sample from a jointdata object}

\description{Generic function used to sampling a subset of data from an
  object of class \code{jointdata}, with a specific size of number of subjects.}

\usage{
%\method{sample}{jointdata}(object, size, replace = FALSE)
sample.jointdata(object, size, replace = FALSE)
}

\arguments{
  \item{object}{an object of class \code{jointdata}}
  \item{size}{number of subjects to include in the sampled subset}
  \item{replace}{should sampling be with replacement?}}

\value{The function returns an object of class \code{jointdata}, with
  data only on the subjects sampled.}

\author{Ines Sousa (isousa@math.uminho.pt)}
\seealso{\code{sample}, \code{jointdata}, \code{UniqueVariables}.}
\examples{
data(heart.valve)
heart.surv <- UniqueVariables(heart.valve, 
                              var.col=c("fuyrs", "status"), 
                              id.col = "num")
heart.valve.jd <- jointdata(survival = heart.surv, 
                            id.col = "num", 
                            time.col = "time")
sample.jointdata(heart.valve.jd, size = 10)
}

\keyword{survival}
