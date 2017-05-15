\name{BOWL-class}
\Rdversion{1.1}
\docType{class}
\alias{BOWL-class}

\title{Class \code{"BOWL"}}
\description{
Results from a single step of the BOWL algorithm.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("BOWL", ...)}.
These objects are for convenience in package development
and should not be created by users.
}
\section{Slots}{
  \describe{

    \item{\code{shift}:     }{An object of class \code{"numeric."}
                              Rewards (or sum of rewards) must be
                              positive. This slot stores the 
                              amount rewards (or sum of rewards) was 
                              shifted to make all positive. }

    \item{\code{step}:      }{An object of class \code{"integer."} 
                              The step of the BOWL algorithm to which
                              this object pertains. }

    \item{\code{ind}:       }{An object of class \code{"logical."}
                              The indicator of adherence to the optimal
                              treatment regime from this step through
                              final decision point. }

    \item{\code{sumR}:      }{An object of class \code{"numeric."}
                              The unshifted total reward from this step through
                              final decision point.}

    \item{\code{prodPr}:    }{An object of class \code{"numeric."}
                              The product of propensities scores
                              for treatment received from this step
                              through final 
                              decision point.}

    \item{\code{txInfo}:    }{An object of class \code{"TxInfoBasic."}
                              Treatment information.}

    \item{\code{optim}:     }{An object of class 
                              \code{"BOWLObj."}
                              The optimization result(s). }
  }

}

\section{Extends}{
Classes \code{"\link{DynTxRegime-class}"} and \code{"\link{PropensityOnly-class}"} directly.
}

\section{Methods}{
  \describe{

    \item{cvInfo}{\code{signature(object = "BOWL")}: 
      Retrieve cross-validation results. }

    \item{DTRstep}{\code{signature(object = "BOWL")}: 
      Retrieve description of method used to create object. }

    \item{optimObj}{\code{signature(object = "BOWL")}: 
      Retrieve optimization results. }

    \item{optTx}{\code{signature(x = "BOWL", newdata = "data.frame")}: 
      Estimate optimal treatment for newdata at the decision
      point for which the provided BOWL analysis pertains. }

    \item{optTx}{\code{signature(x = "BOWL", newdata = "missing")}: 
      Retrieve optimal treatment for training data at the decision
      point for which the provided BOWL analysis pertains. }

    \item{print}{\code{signature(x = "BOWL")}: 
      Print key results of method. }

    \item{regimeCoef}{\code{signature(object = "BOWL")}: 
      Retrieve parameter estimates for regime. }

    \item{show}{\code{signature(object = "BOWL")}: 
      Show key results of method. }

    \item{summary}{\code{signature(object = "BOWL")}: 
      Retrieve key summary information of method. }
  }
}
\author{
Shannon T. Holloway <sthollow@nscu.edu>
}
\examples{
showClass("BOWL")
}

\keyword{internal}
