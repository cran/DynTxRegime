\name{TypedSimpleFit-class}
\docType{class}
\alias{TypedSimpleFit-class}
\alias{coef,TypedSimpleFit-method}
\alias{fitObject,TypedSimpleFit-method}
\alias{plot,TypedSimpleFit,ANY-method}
\alias{print,TypedSimpleFit-method}
\alias{show,TypedSimpleFit-method}
\alias{summary,TypedSimpleFit-method}

\title{Class \code{"TypedSimpleFit"}}

\description{
Results from a simple fit for an outcome regression step.
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("TypedSimpleFit", ...)}.
These objects are for convenience in package development
and should not be created by users.
}

\section{Slots}{
  \describe{
    \item{\code{type}:}{An object of class \code{"character."}
     Description of model; one of \{"Combined", "moMain", "moCont"\}. }

    \item{\code{txInfo}:}{An object of class \code{"TxInfoBasic."} 
      The treatment information. }

    \item{\code{model}:}{An object of class \code{"formula."} 
      The model. }

    \item{\code{solver}:}{An object of class \code{"methodObj."} 
      The information required to obtain parameter estimates. }

    \item{\code{predictor}:}{An object of class \code{"methodObj."} 
      The information required to obtain predictions. }
  }
}
\section{Extends}{
Classes \code{"modelObjFit"}, \code{"\link{SingleDecisionPoint-class}"}, 
\code{"\link{OutcomeRegression-class}"}, and \code{"\link{SubsetsNotModeled-class}"} directly.
}
\section{Methods}{
\describe{
    \item{.predictAllOutcome}{\code{signature(object = "TypedSimpleFit", data = "data.frame")}: 
    Predict outcome for all treatment options.
      }

    \item{coef}{\code{signature(object = "TypedSimpleFit")}: 
      Retrieve the parameter estimates for each regression. }

    \item{fitObject}{\code{signature(object = "TypedSimpleFit")}: 
      Retrieve the value object returned by each regression method. }

    \item{plot}{\code{signature(x = "TypedSimpleFit")}: 
      Generate plots of each regression object. }

    \item{predict}{\code{signature(object = "TypedSimpleFit")}: 
      Predict outcome for new data or training data. }

    \item{print}{\code{signature(x = "TypedSimpleFit")}: 
      Print each regression object. }

    \item{show}{\code{signature(object = "TypedSimpleFit")}: 
      Show key results of each regression object. }

    \item{summary}{\code{signature(object = "TypedSimpleFit")}: 
      Retrieve key summary information from each regression object. }
}}

\author{
Shannon T. Holloway <sthollow@ncsu.edu>
}

\examples{
showClass("TypedSimpleFit")
}

\keyword{internal}
