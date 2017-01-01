\name{to.balanced}
\alias{to.balanced}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Transform data to the longitudinal balanced format}
\description{Transforms a longitudinal data set in the unbalanced format
\code{TO} the balanced format}
\usage{
to.balanced(data, id.col, time.col, Y.col, other.col = NA)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
\item{data}{a data frame with longitudinal data in the unbalanced
format. That is, in the format of 'one row per observation'.}
\item{id.col}{a column number, or column name, in the data frame
\code{data}, where the patient identifier is located.}
\item{time.col}{a column number, or column name, in the data frame
\code{data}, where the time measurements are.}
\item{Y.col}{a vector of column numbers, or column names, of
longitudinal variables, and/or time dependent covariates in the data
frame \code{data}.}
\item{other.col}{a vector of column numbers, or column names, of
baseline covariates, and/or other subject level data, as for
example, survival data. Default does not include \code{other.col}.}
}

%\details{}

\value{
The function returns a data frame with longitudinal data in the
balanced format. The balanced format is considered in this context
as the format where each row has data on each subject. Notice that in
this format we will have multiple columns for the same longitudinal
variable, each corresponding to the variable observed at each time point.
}

%\references{}

\author{Ines Sousa (isousa@math.uminho.pt)}
  
%\note{}

\seealso{\code{to.unbalanced}.}

\examples{
simul <- data.frame(num = 1:10, Y1.1 = rnorm(10), Y1.2 = rnorm(10),
                    Y2.1 = rnorm(10),Y2.2 = rnorm(10), age = rnorm(10))
simul <- to.unbalanced(simul, id.col = 1, times = c(1,2), 
                       Y.col = 2:5, other.col = 6)
simul<-to.balanced(simul, id.col = "num", time.col = "time",
                   Y.col = c("Y1.1","Y2.1"), other.col = "age")
}

\keyword{balanced}% at least one, from doc/KEYWORDS
