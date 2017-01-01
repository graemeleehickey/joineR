\name{summary.jointdata}

\alias{summary.jointdata}
%\alias{summary}

\title{Summarise a jointdata object}

\description{Generic function used to produce summaries of objects of
  class \code{jointdata}}

\usage{
\method{summary}{jointdata}(object, \dots)
}

\arguments{
  \item{object}{an object of class \code{jointdata}}
  \item{\ldots}{further arguments for the summary}
}

\value{The function returns a list with five elements. Each summarises
  each element of the \code{jointdata} object.

  \item{subjects}{Gives the number of subjects in the data set.}
  \item{longitudinal}{If longitudinal data is available, it gives
    the names and class, of the longitudinal variables.}
  \item{survival}{If survival data is available, it gives the
    number of subjects with failure and censored survival times.}
  \item{baseline}{If baseline covariates is available, it gives
    the names and class, of the baseline covariates.}
  \item{times}{If longitudinal data is available, it gives the unique
    longitudinal time measurements, if it is a balanced study. In case
    of unbalanced study , it will only state it is an unbalanced study.}
}

\author{Ines Sousa (isousa@math.uminho.pt)}
\seealso{\code{jointdata}, \code{UniqueVariables}.}
\examples{
data(heart.valve)
heart.surv <- UniqueVariables(heart.valve,
                              var.col = c("fuyrs", "status"), 
                              id.col = "num")
heart.valve.jd <- jointdata(survival = heart.surv, 
                            id.col = "num",
                            time.col = "time")
summary(heart.valve.jd)}

\keyword{survival}
