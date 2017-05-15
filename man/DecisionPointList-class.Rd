\name{DecisionPointList-class}
\docType{class}
\alias{DecisionPointList-class}
\alias{coef,DecisionPointList-method}
\alias{fitObject,DecisionPointList-method}
\alias{plot,DecisionPointList,ANY-method}
\alias{predict,DecisionPointList-method}
\alias{print,DecisionPointList-method}
\alias{show,DecisionPointList-method}
\alias{summary,DecisionPointList-method}

\title{Class \code{"DecisionPointList"}}

\description{
A class to store results for multiple-decision-point methods.
}

\section{Objects from the Class}{
A virtual Class: No objects may be created from it.
}

\section{Extends}{
Classes \code{"\link{List-class}"} and
\code{"\link{MultipleDecisionPoint-class}"}, directly.
}

\section{Methods}{
  \describe{

    \item{coef}{\code{signature(object = "DecisionPointList")}: 
      Retrieve the regression parameter estimates for each decision point. }

    \item{fitObject}{\code{signature(object = "DecisionPointList")}: 
      Retrieve the value object returned by regression methods
      used for each decision point. }

    \item{plot}{\code{signature(x = "DecisionPointList")}: 
      Generate plots of the regression objects
      obtained in each decision point. }

    \item{predict}{\code{signature(object = "DecisionPointList")}: 
      Obtains predictions at each decision point. }

    \item{print}{\code{signature(x = "DecisionPointList")}: 
      Print the key results at each decision point. }

    \item{show}{\code{signature(object = "DecisionPointList")}: 
      Show key results of regression objects at each decision point. }

    \item{summary}{\code{signature(object = "DecisionPointList")}: 
      Retrieve key summary information from regression objects 
      at each decision point. }
	 }
}

\author{
Shannon T. Holloway <sthollow@ncsu.edu>
}

\examples{
showClass("DecisionPointList")
}

\keyword{internal}

