\name{.newRegime}
\docType{methods}
\alias{.newRegime}
\alias{.newRegime,function-method}
\alias{.newRegime,list-method}

\title{ Process and Store Decision Rule Information.}

\description{
Given a user defined function specifying the class
of decision rules being considered, process
and store information. Method is not exported.
}

\usage{
.newRegime(object)
\S4method{.newRegime}{function}(object)
\S4method{.newRegime}{list}(object)
}

\arguments{
\item{object}{A function defining the class of regimes or a list
of functions defining the class of regimes at each decision point}
}

\author{
  Shannon T. Holloway <sthollow@ncsu.edu>
}

\examples{

regime <- function(a,b,c,data){
            return(a*data$x1 + b*data$x2 + c*data$x3 < 0)
          }

regObj <- DynTxRegime:::.newRegime(object = regime)

is(regObj)

regime <- list()
regime[[1L]] <- function(a,data){
                  return(data$x1 < a)
                }

regime[[2L]] <- function(a,data){
                  return(data$x2 < a)
                }


regObj <- DynTxRegime:::.newRegime(object = regime)

is(regObj)
}

\keyword{internal}
