\name{PropensityFit-class}
\docType{class}
\alias{PropensityFit-class}
\alias{predict,PropensityFit-method}


\title{Class \code{"PropensityFit"}}

\description{
Results of a single propensity score regression step.
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("PropensityFit", ...)}.
These objects are for convenience in package development
and should not be created by users.
}

\section{Slots}{
  \describe{
    \item{\code{small}:}{Object of class \code{"logical."} 

      TRUE if none or smallest treatment option is missing
      from predictions.
      FALSE If largest treatment option is missing from predictions }

    \item{\code{levs}:}{Object of class \code{"character."} 

      Vector of treatment options present in data. }

  }
}

\section{Extends}{
Classes \code{"modelObjFit"}, \code{"\link{SingleDecisionPoint-class}"}, 
\code{"\link{PropensityRegression-class}"}, and \code{"\link{SubsetsNotModeled-class}"} directly.
}

\section{Methods}{
  \describe{

    \item{predict}{\code{signature(object = "PropensityFit")}: 
      Predict propensity for newdata or training data. }

	 }
}
\author{
Shannon T. Holloway <sthollow@ncsu.edu>
}

\examples{
showClass("PropensityFit")
}

\keyword{internal}

