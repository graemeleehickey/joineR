\name{summary.joint}

\alias{summary.joint}

\title{Summarise a random effects joint model fit}

\description{Generic function used to produce summary information from a fitted random effects joint model as represented by \code{object} of class \code{joint}.}

\usage{
\method{summary}{joint}(object, variance, \dots)
}

\arguments{
  \item{object}{an object inheriting from class \code{joint} representing a fitted random effects joint model.}
  \item{variance}{should the variance components be output as variances or standard deviations? Defaults to \code{variance = TRUE}.}
  \item{\ldots}{further arguments for the summary}
}

\value{
an object inheriting from class \code{summary.joint} with all components
     included in \code{object} (see \code{joint} for a full description of
     the components) plus the following components:

\item{nobs}{the total number of (typically longitudinal) observations (i.e. rows in an unbalanced data set).}
\item{ngrps}{the number of groups in the analysed dataset, often individual subjects.}
}

\author{Pete Philipson (pete.philipson@northmbria.ac.uk)}
\examples{
data(heart.valve)
heart.surv <- UniqueVariables(heart.valve, 
                              var.col=c("fuyrs","status"),
                              id.col="num")
heart.long <- heart.valve[, c("num", "time", "log.lvmi")]
heart.cov <- UniqueVariables(heart.valve, 
                             c("age", "hs", "sex"), 
                             id.col = "num")
heart.valve.jd <- jointdata(longitudinal = heart.long, 
                            baseline = heart.cov, 
                            survival = heart.surv, 
                            id.col = "num", 
                            time.col = "time")
fit <- joint(data = heart.valve.jd, 
             long.formula = log.lvmi ~ 1 + time + hs, 
             surv.formula = Surv(fuyrs,status) ~ hs, 
             model = "intslope")
summary(fit)
}
\keyword{models}
