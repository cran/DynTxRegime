% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/J_class_OptimalClass.R
\docType{class}
\name{OptimalClassObj-class}
\alias{OptimalClassObj-class}
\title{Class \code{OptimalClassObj}}
\description{
Class \code{OptimalClassObj} contains results for a single decision point 
  when estimates are obtained from the classification perspective.
  Objects of this class are returned by optimalClass().
}
\section{Slots}{

\describe{
\item{\code{class}}{Results of the classification step.}

\item{\code{outcome}}{Results of the outcome regression step.}

\item{\code{propen}}{Results of the propensity step.}

\item{\code{optimal}}{Estimated optimal tx and value}

\item{\code{Call}}{Unevaluated call.}
}}

\section{Methods For Post-Processing of Regression Analysis}{


 \describe{
   \item{outcome}{: 
      Retrieve value object returned by outcome regression methods.  }
}



 \describe{
   \item{propen}{: 
      Retrieve value object returned by propensity regression methods.  }
}



 \describe{
   \item{classif}{: 
      Retrieve value object returned by classification regression methods.  }
}



 \describe{
   \item{coef}{: 
     Retrieve parameter estimates for all regression steps. }
   \item{fitObject}{: 
      Retrieve value object returned by regression methods. }
   \item{plot}{: 
      Generate plots for regression analyses.  }
}
}

\section{Methods For Accessing Main Results}{


 \describe{
   \item{DTRstep}{: 
      Retrieve description of method used to create object. }
   \item{estimator}{: 
      Retrieve the estimated value of the estimated
      optimal regime for the training data set. }
   \item{optTx}{: 
      Retrieve/predict the estimated decision functions and/or optimal tx. }
   \item{print}{: 
      Print main results of analysis.}
   \item{show}{: 
      Show main results of analysis.}
   \item{summary}{: 
      Retrieve summary information.}
}
}

