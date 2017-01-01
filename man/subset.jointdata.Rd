\name{subset.jointdata}

\alias{subset.jointdata}

\title{Subsetting object of class 'jointdata'}

\description{Returns an object of class \code{jointdata} which is a
  subset of an original object of class \code{jointdata}.}

\usage{
\method{subset}{jointdata}(x, subj.subset, \dots)
}

\arguments{
  \item{x}{an object of class \code{jointdata}}
  \item{subj.subset}{vector of subject identifiers, to include in
    the data subset. This must be a unique vector of patient identifiers.}
  \item{\ldots}{further arguments to be passed to or from other methods.}
}

\value{The function returns an object of class \code{jointdata}, with
  data only on a subset of subjects.}

\author{Ines Sousa (isousa@math.uminho.pt)}

\examples{
data(heart.valve)
heart.surv <- UniqueVariables(heart.valve, var.col = c("fuyrs", "status"), 
      id.col = "num")
heart.long <- heart.valve[, c(1, 4, 5, 7, 8, 9, 10, 11)]
heart.jd <- jointdata(longitudinal = heart.long, 
      survival = heart.surv, id.col = "num", time.col = "time")
take <- heart.jd$survival$num[heart.jd$survival$status == 0]
heart.jd.cens <- subset(heart.jd, take)
}

\keyword{jointdata}
