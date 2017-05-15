\name{List-class}
\docType{class}
\alias{List-class}
\alias{[[<-,List-method}
\alias{[[,List-method}
\alias{length,List-method}
\alias{names,List-method}

\title{Class \code{"List"}}

\description{
Mimics a standard R list object.
}

\section{Objects from the Class}{
A virtual Class: No objects may be created from it.
}

\section{Slots}{
  \describe{
    \item{\code{loo}:}{An object of class \code{"list."} }
  }
}

\section{Methods}{
  \describe{

    \item{[[<-}{\code{signature(x = "List")}: 
       Set an element of the list. }

    \item{[[}{\code{signature(x = "List")}: 
       Retrieve an element of the list. }

    \item{length}{\code{signature(x = "List")}: 
      Retrieve the length of the list. }

    \item{names}{\code{signature(x = "List")}: 
      Retrieve the names of each element of the list. }
  }
}

\author{
Shannon T. Holloway <sthollow@ncsu.edu>
}

\examples{
showClass("List")
}

\keyword{internal}
