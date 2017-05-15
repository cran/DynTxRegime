\name{QLearn-class}
\docType{class}
\alias{QLearn-class}

\title{Class \code{"QLearn"}}

\description{
Class implementing a step of the Q-Learning Algorithm
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("QLearn", ...)}.
These objects are for convenience in package development
and should not be created by users.
}

\section{Slots}{
  \describe{
    \item{\code{step}:}{Object of class \code{"integer."} 
      The step of the Q-Learning algorithm. }

    \item{\code{decisionFunc}:}{Object of class \code{"matrix."} 
      The value of the decision function at each treatment option. }

  }
}

\section{Extends}{
Classes \code{"\link{OutcomeOnly-class}"} and 
\code{"\link{DynTxRegime-class},"} directly.
}

\section{Methods}{
  \describe{

    \item{DTRstep}{\code{signature(object = "QLearn")}: 
      Retrieve description of method implemented to obtain object. }

    \item{optTx}{\code{signature(x = "QLearn", newdata = "data.frame")}: 
      Predict optimal treatments for newdata. }

    \item{optTx}{\code{signature(x = "QLearn", newdata = "missing")}: 
      Retrieve optimal treatments for training data. }

    \item{print}{\code{signature(x = "QLearn")}: 
      Print key results of method. }

    \item{show}{\code{signature(object = "QLearn")}: 
      Show key results of method. }

    \item{summary}{\code{signature(object = "QLearn")}: 
      Retrieve key summary information of method. }

 }
}
\author{
Shannon T. Holloway <sthollow@ncsu.edu>
}
\examples{
showClass("QLearn")
}

\keyword{internal}
