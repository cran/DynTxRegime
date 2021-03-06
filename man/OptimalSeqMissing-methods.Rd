% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/H_class_OptimalSeqMissing.R
\name{OptimalSeqMissing-methods}
\alias{OptimalSeqMissing-methods}
\alias{Call,OptimalSeqMissing-method}
\alias{coef,OptimalSeqMissing-method}
\alias{DTRstep,OptimalSeqMissing-method}
\alias{estimator,OptimalSeqMissing-method}
\alias{fitObject,OptimalSeqMissing-method}
\alias{genetic,OptimalSeqMissing-method}
\alias{optTx,OptimalSeqMissing,data.frame-method}
\alias{optTx,OptimalSeqMissing,missing-method}
\alias{outcome,OptimalSeqMissing-method}
\alias{plot,OptimalSeqMissing,ANY-method}
\alias{print,OptimalSeqMissing-method}
\alias{propen,OptimalSeqMissing-method}
\alias{regimeCoef,OptimalSeqMissing-method}
\alias{show,OptimalSeqMissing-method}
\alias{summary,OptimalSeqMissing-method}
\title{Methods Available for Objects of Class \code{OptimalSeqMissing}}
\usage{
\S4method{Call}{OptimalSeqMissing}(name, ...)

\S4method{coef}{OptimalSeqMissing}(object, ...)

\S4method{DTRstep}{OptimalSeqMissing}(object)

\S4method{estimator}{OptimalSeqMissing}(x, ...)

\S4method{fitObject}{OptimalSeqMissing}(object, ...)

\S4method{genetic}{OptimalSeqMissing}(object, ...)

\S4method{optTx}{OptimalSeqMissing,data.frame}(x, newdata, ...)

\S4method{optTx}{OptimalSeqMissing,missing}(x, newdata, ...)

\S4method{outcome}{OptimalSeqMissing}(object, ...)

\S4method{plot}{OptimalSeqMissing,ANY}(x, suppress = FALSE, ...)

\S4method{print}{OptimalSeqMissing}(x, ...)

\S4method{propen}{OptimalSeqMissing}(object, ...)

\S4method{regimeCoef}{OptimalSeqMissing}(object, ...)

\S4method{show}{OptimalSeqMissing}(object)

\S4method{summary}{OptimalSeqMissing}(object, ...)
}
\description{
Methods Available for Objects of Class \code{OptimalSeqMissing}

\code{Call(name)}
  returns the unevaluated call to method

Retrieve coefficients of fits

\code{DTRstep(x)}
  print statement indicating the coarsened data perspective

\code{estimator(x)}
  retrieves the estimated value.  Calls method defined for 
  \code{OptimalSeq}.

\code{fitObject(object)}
  retrieves value objects of model functions. Calls method defined for 
  \code{OptimalSeq}.

\code{genetic(object)}
  retrieves genetic algorithm results. Calls method defined for 
  \code{OptimalSeq}.

Predict Optimal Treatment and Decision Function Based on a 
Missing Data AIPW Analysis

\code{optTx(x)}
  retrieves the optimal tx. Calls method defined for \code{OptimalSeq}.

\code{outcome(object)}
  retrieves value object returned by outcome model functions. Calls method 
  defined for \code{OptimalSeq}.

\code{plot(x,suppress)}
  generates plot for model functions. Calls method defined for 
  \code{OptimalSeq}.

\code{print(x)}
  Extends method defined for \code{OptimalSeq} to include DTRStep()

\code{propen(object)}
  retrieves value object returned by propensity model functions. Calls method 
  defined for \code{OptimalSeq}.

\code{regimeCoef(object)}
  retrieves estimated tx regime parameters. Calls method defined for 
  \code{OptimalSeq}.

\code{show(object)}
  Extends method defined for \code{OptimalSeq} to include DTRStep()

\code{summary(object)}
  retrieves summary information. Calls method defined for \code{OptimalSeq}.
}
\keyword{internal}
