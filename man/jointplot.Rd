\name{jointplot}
\alias{jointplot}

\title{Joint plot of longitudinal and survival data}
\description{
  This function views the longitudinal profile of each unit with the last longitudinal measurement prior to event-time (censored or not) taken as the end-point, referred to as time zero. In doing so, the shape of the profile prior to event-time can be inspected. This can be done over a user-specified number of time units.
}
\usage{
jointplot(object, Y.col, Cens.col, lag, split = TRUE, col1, col2, 
          xlab, ylab, gp1lab, gp2lab, smooth, 
          mean.profile = FALSE, mcol1, mcol2) 
}

\arguments{
  \item{object}{Name of the \code{jointdata} object}
  \item{Y.col}{An element of class \code{character} identifying the longitudinal response part of the \code{jointdata} object.}
  \item{Cens.col}{An element of class \code{character} identifying the survival status or censoring indicator part of the \code{jointdata} object.} 
  \item{lag}{Argument which specifies how many units in time we look
    back through. Defaults to the maximum observation time across all units.}
  \item{split}{\code{TRUE}/\code{FALSE} argument which allows the profiles of units which \sQuote{fail} and
    those which are \sQuote{censored} to be viewed in separate panels of the
    same graph. This is the default option. Using \code{split = FALSE} will plot all
    profiles overlaid on a single plot.}
  \item{col1}{argument to choose the colour for the profiles of the
    \sQuote{censored} units.}
  \item{col2}{argument to choose the colour for the profiles of the
    \sQuote{failed} units.}
  \item{xlab}{An element of class \code{character} indicating the title for the x-axis.}
  \item{ylab}{An element of class \code{character} indicating the title for the x-axis.}
  \item{gp1lab}{An element of class \code{character} for the group corresponding to a censoring indicator of zero. Typically, the censored group.}
  \item{gp2lab}{An element of class \code{character} for the group corresponding to a censoring indicator of one. Typically, the group experiencing the event of interest.}
  \item{smooth}{the smoother span. This gives the proportion of points in the plot which influence the smooth at each value. Defaults to a value of 2/3. Larger values give more smoothness. See \code{lowess}for further details.}
  \item{mean.profile}{draw mean profiles if TRUE. Only applies to the \code{split = TRUE} case.}
  \item{mcol1}{argument to choose the colour for the mean profile of the units with a censoring indicator of zero.}
  \item{mcol2}{argument to choose the colour for the mean profile of the units with a censoring indicator of one.}
}
\details{
  The function tailors the \code{xyplot} function in \pkg{lattice} to
  produce a representation of joint data with longitudinal and survival
  components. 
}

\references{Wulfsohn, M. S. and Tsiatis, A. A. (1997) \sQuote{A Joint Model for Survival
  and Longitudinal Data Measured with Error}, \emph{Biometrics}, \bold{53}, 330-339.}

\author{Pete Philipson \email{pete.philipson@northumbria.ac.uk}}

\seealso{\code{xyplot}, \code{joint}, \code{jointdata}}
\examples{
data(heart.valve)
heart.surv <- UniqueVariables(heart.valve, 
                              var.col = c("fuyrs", "status"),
                              id.col = "num")
heart.long <- heart.valve[,c("num", "time", "log.lvmi")]
heart.cov <- UniqueVariables(heart.valve, 
                             c("age", "sex"), 
                             id.col = "num")
heart.valve.jd <- jointdata(longitudinal = heart.long, 
                            baseline = heart.cov, 
                            survival = heart.surv,
                            id.col = "num",
                            time.col = "time")
jointplot(heart.valve.jd, Y.col = "log.lvmi", 
          Cens.col = "status", lag = 5)
}

\keyword{models}
\keyword{survival}
