\name{OptimalSeqAIPWE_MDP-class}
\Rdversion{1.1}
\docType{class}
\alias{OptimalSeqAIPWE_MDP-class}

\title{Class \code{"OptimalSeqAIPWE_MDP"}}
\description{
Implementation of multiple-decision-point coarsened data
perspective with AIPWE.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("OptimalSeqAIPWE_MDP", ...)}.
These objects are for convenience in package development
and should not be created by users.
}
\section{Slots}{
  \describe{
    \item{\code{genetic}:}{An object of class \code{"list."} 
      Results returned by genetic algorithm. }

    \item{\code{regime}:}{An object of class \code{"Regime_DecisionPointList."}
      The regime class definition. }
  }
}
\section{Extends}{
Classes \code{"\link{OptimalSeq-class}"} and \code{"\link{PropensityOnly-class}"}, directly.

Class \code{"\link{DynTxRegime-class}"} by class \code{"\link{OptimalSeq-class}"}, distance 2.
}
\section{Methods}{
  \describe{
    \item{DTRstep}{\code{signature(object = "OptimalSeqAIPWE_MDP")}: 
      Retrieve description of method used to create object. }
    \item{genetic}{\code{signature(object = "OptimalSeqAIPWE_MDP")}: 
      Retrieve the value object returned by rgenoud. }
    \item{optTx}{\code{signature(x = "OptimalSeqAIPWE_MDP", newdata = "data.frame")}: 
      Estimate optimal treatment for newdata. }
    \item{optTx}{\code{signature(x = "OptimalSeq_MDP", newdata = "missing")}: 
      Retrieve estimated optimal treatment for training data. }
    \item{print}{\code{signature(x = "OptimalSeqAIPWE_MDP")}: 
      Print key results of method. }
    \item{regimeCoef}{\code{signature(x = "OptimalSeqAIPWE_MDP")}: 
      Retrieve regime parameter estimates. }
    \item{show}{\code{signature(object = "OptimalSeqAIPWE_MDP")}: 
      Show key results of method. }
    \item{summary}{\code{signature(object = "OptimalSeqAIPWE_MDP")}: 
      Retrieve key summary information of method. }

	 }
}
\author{
Shannon T. Holloway <sthollow@ncsu.edu>
}

\keyword{internal}
