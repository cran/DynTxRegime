\name{optTx}
\alias{optTx}
\alias{optTx,DynTxRegime,data.frame-method}
\alias{optTx,DynTxRegime,missing-method}

\title{ Methods to Retrieve Estimated or to Predict Optimal Treatment}

\description{
Retrieves the estimated or predicted optimal treatment.
}

\usage{
\S4method{optTx}{DynTxRegime,missing}(x, newdata, ...)  
\S4method{optTx}{DynTxRegime,data.frame}(x, newdata, ...)  
}

\arguments{
  \item{x }{an object of class DynTxRegime.}

  \item{newdata }{an object of class data.frame containing
                  covariates and treatment histories.}

  \item{\dots }{ignored.}

}

\details{
  For all DynTxRegime treatment regime methods, 
  if newdata is missing, optTx retrieves the estimated
  optimal treatment for the training data. 

  If newdata is provided, optTx estimates the
  optimal treatment for the new data based on the
  original analysis.

  For optimalSeq, an optinal argument, dp, must be set,
  which indicates the stage of treatment for which
  the estimated optimal treatment is desired.
}

\value{
       For outcome regression methods Q-Learning and IQ-Learning a list
       \item{qFunctions}{ a matrix of estimated Q-Functions.}
       \item{optimalTx}{a vector of estimated optimal treatments.}
       For value-search methods optimalClass and optimalSeq a 
       a matrix of estimated optimal treatments.
}

