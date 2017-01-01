\name{to.unbalanced}

\alias{to.unbalanced}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{Transform data to the longitudinal unbalanced format}

\description{
Transforms a longitudinal data set in the balanced format
\code{TO} the unbalanced format
}

\usage{to.unbalanced(data, id.col, times, Y.col, other.col = NA)}

%- maybe also 'usage' for other objects documented here.
\arguments{
\item{data}{a data frame with longitudinal data in the balanced
format. That is, in the format of 'one row per subject'.}
\item{id.col}{a column number, or column name, in the data frame
\code{data}, where the patient identifications is.}
\item{times}{a vector with the unique time points where the patients
are observed. This is the study design time points in a balanced
data set.}
\item{Y.col}{a vector of column numbers, or column names, of
longitudinal variables, and/or time dependent covariates in the data}
frame \code{data}.
\item{other.col}{a vector of column numbers, or column names, of
baseline covariates, and/or other subject level data, as for
example, survival data. Default does not include \code{other.col}.}
}

%\details{}

\value{
The function returns a data frame with longitudinal data in the
unbalanced format. The unbalanced format is considered in this context
as the format where each row has data on each subject observation.
}

%\references{}

\author{Ines Sousa (isousa@uminho.pt)}
  
%\note{}

\seealso{\code{to.balanced}.}

\examples{
simul <- data.frame(num = 1:10,Y1.1 = rnorm(10),Y1.2 = rnorm(10),
                    Y2.1 = rnorm(10),Y2.2 = rnorm(10), age = rnorm(10))
to.unbalanced(simul, id.col = 1, times = c(1,2), Y.col = 2:5,
              other.col = 6)
}

\keyword{balanced}% at least one, from doc/KEYWORDS
