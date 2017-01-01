\name{jointSE}
\alias{jointSE}

\title{Standard errors via bootstrap for a joint model fit}
\description{
  This function takes a model fit from a joint model and calculates
  standard errors, with optional confidence intervals, for the main
  longitudinal and survival covariates.
  }
\usage{
jointSE(fitted, n.boot, gpt, lgpt, max.it, tol, 
         print.detail = FALSE)
}

\arguments{
  
  \item{fitted}{A list containing as components the parameter estimates obtained by fitting a joint model along with the respective formulae for the longitudinal and survival sub-models and the model chosen, see \code{joint} for further details.}  
  \item{n.boot}{Argument specifying the number of bootstrap samples to use in order to obtain the standard error estimates and confidence intervals. Note that at least n.boot=100 is required in order for the function to return non-zero confidence intervals.}
  \item{gpt}{the number of quadrature points across which the
    integration with respect to the random effects will be
    performed. Defaults to \code{gpt=3} which produces stable estimates in most
    datasets.}
  \item{lgpt}{the number of quadrature points which the log-likelihood
    is evaluated over following a model fit. This defaults to \code{lgpt = 10},
    though \code{lgpt = 3} is often sufficient.}
  \item{max.it}{the maximum number of iterations of the EM algorithm that the function will
    perform. Defaults to \code{max.it = 200}, though more iterations may be
    necessary for large, complex data.}
  \item{tol}{the tolerance level before convergence of the algorithm is
    deemed to have occurred. Default value is \code{tol = 0.001}.}
  \item{print.detail}{This argument determines the level of printing that is done during the bootstrapping. If \code{TRUE} then the parameter estimates from each bootstrap sample are output.} 
}
\details{
  Standard errors and confidence intervals are obtained by repeated
  fitting of the requisite joint model to bootstrap samples of the
  original longitudinal and survival data. It is rare that more than
  200 bootstrap samples are needed for estimating a standard error. The
  number of bootstrap samples needed for accurate confidence intervals
  can be as large as 1000.
}

\references{
  Wulfsohn, M. S. and Tsiatis, A. A. (1997) \sQuote{A Joint Model for Survival
    and Longitudinal Data Measured with Error}. \emph{Biometrics},
  \bold{53}, 330-339.

  Efron, B. and Tibshirani, J. (1994) \sQuote{An Introduction to the Bootstrap}. Chapman & Hall.
}

  \author{Ruwanthi Kolamunnage-Dona (Ruwanthi.Kolamunnage-Dona@liverpool.ac.uk) and Pete Philipson (pete.philipson@northumbria.ac.uk)}
  

\seealso{\code{lme}, \code{coxph}, \code{joint}, \code{jointdata}.}

\examples{
data(heart.valve)
heart.surv <- UniqueVariables(heart.valve, 
                              var.col = c("fuyrs", "status"), 
                              id.col="num")
heart.long <- heart.valve[, c("num", "time", "log.lvmi")]
heart.cov <- UniqueVariables(heart.valve, 
                             c("age", "hs", "sex"), 
                             id.col="num")
heart.valve.jd <- jointdata(longitudinal = heart.long, 
                            baseline = heart.cov, 
                            survival = heart.surv, 
                            id.col = "num", 
                            time.col = "time")
fit <- joint(heart.valve.jd, 
             long.formula = log.lvmi ~ 1 + time + hs, 
             surv.formula = Surv(fuyrs,status) ~ hs, 
             model = "int")
jointSE(fitted = fit, n.boot = 1)
}

\keyword{models}% at least one, from doc/KEYWORDS
\keyword{survival}
