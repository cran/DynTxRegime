\name{DynTxRegime-class}
\docType{class}
\alias{DynTxRegime-class}

\title{Class \code{"DynTxRegime"}}

\description{
A VIRTUAL class from which all methods implemented in \pkg{DynTxRegime} inherit.
}

\section{Objects from the Class}{
A virtual Class: No objects may be created from it.
}

\section{Slots}{
  \describe{
    \item{\code{call}:}{An object of class \code{"CallOrNull."} 
       The matched call. }

    \item{\code{estimatedValue}:}{
       An object of class \code{"LogicalOrNumeric."}
        The estimated value of the estimated optimal treatments
       for the training data. }

    \item{\code{optimalTx}:}{An object of class \code{"MatrixOrVector."} 
       The estimated optimal treatments for the training data. }
  }
}

\section{Methods}{
  \describe{
    \item{classif}{\code{signature(object = "DynTxRegime")}: 
       Retrieve value object from classification step. NA for
       all objects returned by \pkg{DynTxRegime} statistical methods 
       except for those returned by optimalClass(). }

    \item{coef}{\code{signature(object = "DynTxRegime")}: 
       Retrieve parameter estimates for all regression steps. }

    \item{cvInfo}{\code{signature(object = "DynTxRegime")}: 
       Retrieve cross-validation results. NA for
       all objects returned by \pkg{DynTxRegime} statistical methods 
       except for those returned by earl(), owl(), rwl(), and bowl(). }

    \item{DTRstep}{\code{signature(object = "DynTxRegime")}: 
       Retrieve description of method used to create object. }

    \item{estimator}{\code{signature(x = "DynTxRegime")}: 
       Retrieve the estimated value of the estimated
       optimal regime for the training data set. }

    \item{fitObject}{\code{signature(object = "DynTxRegime")}: 
       Retrieve value object returned by regression methods. }

    \item{fittedCont}{\code{signature(object = "DynTxRegime")}: 
       Retrieve estimated contrast component of outcome regression. NA for
       all objects returned by \pkg{DynTxRegime} statistical methods 
       except for those returned by iqLearnSS(). }

    \item{fittedMain}{\code{signature(object = "DynTxRegime")}: 
       Retrieve estimated main effects component of outcome regression. NA for
       all objects returned by \pkg{DynTxRegime} statistical methods 
       except for those returned by iqLearnSS(). }

    \item{genetic}{\code{signature(object = "DynTxRegime")}: 
       Retrieve value object returned by genoud(). NA for
       all objects returned by \pkg{DynTxRegime} statistical methods 
       except for those returned by optimalSeq(). }

    \item{optimObj}{\code{signature(object = "DynTxRegime")}: 
       Retrieve value object returned by optimization routine. NA for
       all objects returned by \pkg{DynTxRegime} statistical methods 
       except for those returned by earl(), owl(), rwl(), and bowl(). }

    \item{optTx}{\code{signature(x = "DynTxRegime", newdata = "missing")}: 
       Retrieve the estimated optimal treatment regime for
       training data set. }

    \item{optTx}{\code{signature(x = "DynTxRegime", newdata = "data.frame")}: 
       Estimate the optimal treatment regime for newdata. }

    \item{outcome}{\code{signature(x = "DynTxRegime")}: 
       Retrieve value object returned by outcome regression methods.  }

    \item{plot}{\code{signature(x = "DynTxRegime")}: 
       Generate plots for regression analyses.  }

    \item{print}{\code{signature(object = "DynTxRegime")}: 
       Print main results of analysis.}

    \item{propen}{\code{signature(x = "DynTxRegime")}: 
       Retrieve value object returned by propensity score regression methods.
       NA for objects returned by qLearn(), iqLearnSS, iqLearnFSM(),
       iqLearnFSC(), and iqLearnFSV().  }

    \item{qqPlot}{\code{signature(x = "DynTxRegime")}: 
       Generate a qq-plot.
       NA for
       all objects returned by \pkg{DynTxRegime} statistical methods 
       except for those returned by iqLearnFSV().  }

    \item{regimeCoef}{\code{signature(object = "DynTxRegime")}: 
       Retrieve the estimated decision function parameter estimates.
       NA for
       all objects returned by \pkg{DynTxRegime} statistical methods 
       except for those returned by optimalSeq(), earl(), owl(), rwl(), and bowl().  }

    \item{residuals}{\code{signature(object = "DynTxRegime")}: 
       Retrieve residuals used by statistical methods. NA for
       all objects returned by \pkg{DynTxRegime} statistical methods 
       except for those returned by iqLearnFSC(), iqLearnFSV(), and rwl().}

    \item{sd}{\code{signature(x = "DynTxRegime")}: 
       Retrieve the standard deviation of residuals. NA for
       all objects returned by \pkg{DynTxRegime} statistical methods 
       except for those returned by iqLearnFSC().}

    \item{show}{\code{signature(object = "DynTxRegime")}: 
       Show main results of analysis.}

    \item{summary}{\code{signature(object = "DynTxRegime")}: 
       Retrieve summary information from regression analyses.}

  }
}

\author{
Shannon T. Holloway <sthollow@ncsu.edu>
}

\examples{
showClass("DynTxRegime")
}


