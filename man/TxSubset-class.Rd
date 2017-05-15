\name{TxSubset-class}
\docType{class}
\alias{TxSubset-class}

\title{Class \code{"TxSubset"}}

\description{
Stores basic treatment information for a single decision point
when subsets have been identified.
}

\section{Objects from the Class}{
A virtual Class: no object may be created from it.
}

\section{Slots}{
  \describe{

    \item{\code{ptsSubset}:}{An object of class \code{"character."} 
                             A vector of subset names indicating
                             the subset to which each patient
                             belongs. }

    \item{\code{subsetRule}:}{An object of class \code{"function."} 
                              A function defining how treatment
                              subsets are determined (from input
                              \code{fSet}) }

    \item{\code{subsets}:}{An object of class \code{"list."} 
                           A list of treatment subset names and their
                           available treatment options. }

    \item{\code{singleton}:}{An object of class \code{"logical."} 
                            A vector indicating of only a single
                            treatment is available to each patient. }

  }
}

\section{Methods}{
  \describe{
    \item{.getPtsSubset}{\code{signature(object = "TxSubset")}: 
      Retrieve patient subset identities.}

    \item{.getSingleton}{\code{signature(object = "TxSubset")}: 
      Retrieve singleton vector.}

    \item{.getSubsetRule}{\code{signature(object = "TxSubset")}: 
      Return function that defines subset rules.}

    \item{.getSubsets}{\code{signature(object = "TxSubset")}: 
      Retrieve list of subset names and treatment options.}

    \item{.validTx}{\code{signature(object = "TxSubset", txVec = "ANY")}: 
      Ensure that all elements of \code{txVec} are present in the superset of treatment
      options. Method is not exported.}

  }
}

\author{
Shannon T Holloway <sthollow@ncsu.edu>
}

\examples{
showClass("TxSubset")
}

\keyword{internal}
