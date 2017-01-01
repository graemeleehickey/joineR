\name{mental}

\alias{mental}

\docType{data}

\title{Mental Health Trial Data}

\description{The data is obtained from a trial in which chronically ill
  mental health patients were randomised across two treatments: placebo
  and an active drug. A questionnaire instrument was used to assess each
  patient's mental state at weeks 0, 1, 2, 4, 6 and 8
  post-randomisation, a high recorded score implying a severe
  condition. Some of the 100 patients dropped out of the study for
  reasons that were thought to be related to their mental state, and
  therefore potentially informative; others dropped out for reasons
  unrelated to their mental state.}
\usage{data(mental)}
\format{A balanced data set with respect to the times at which
  observations recorded. The data consists of the following variables on
  each patient:
  \itemize{
    \item{\bold{[,1]} - id - }{patient identifier (1,2,...,100)}
    \item{\bold{[,2]} - Y.t0 - }{mental state assessment in week 0
      (coded NA if missing)}
    \item{\bold{[,3]} - Y.t1 - }{mental state assessment in week 1}
    \item{\bold{[,4]} - Y.t2 - }{mental state assessment in week 2}
    \item{\bold{[,5]} - Y.t4 - }{mental state assessment in week 4}
    \item{\bold{[,6]} - Y.t6 - }{mental state assessment in week 6}
    \item{\bold{[,7]} - Y.t8 - }{mental state assessment in week 8}
    \item{\bold{[,8]} - treat - }{treatment allocation (0=placebo;
      1=active drug)}
    \item{\bold{[,9]} - n.obs - }{number of non-missing mental state
      assessments}
    \item{\bold{[,10]} - event.time - }{imputed dropout time in weeks
      (coded 8.002 for completers)}
    \item{\bold{[,11]} - status - }{censoring indicator (0=completer
      or non-informative dropout, 1=potentially informative dropout)}
  }
}

\keyword{datasets}
