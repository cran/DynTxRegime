\name{optimalSeq}
\alias{optimalSeq}

\title{
Regression Based Value-Search Estimation of Optimal Dynamic Treatment Regimes
}

\description{
A doubly robust Augmented Inverse Propensity Weighted Estimator (AIPWE) or Inverse Propensity Weighted Estimator (IPWE) for population mean outcome is optimized over a restricted class of regimes. Methods are available for both single-decision-point and multiple-decision-point regimes. This method requires the \pkg{rgenoud} package. 
}

\usage{
optimalSeq(..., moPropen, moMain, moCont, data, response, txName, regimes,
           fSet = NULL, refit = FALSE, iter = 0, verbose = TRUE)
}
\arguments{

 \item{...        }{additional arguments required by rgenoud. 
                    At a minimum,
                    this should include Domains, pop.size and starting.values. 
                    See ?rgenoud for more information.}

 \item{moPropen  }{An object of class \code{"modelObj,"} 
                   an object of class \code{"list"} containing 
                   objects of class \code{"modelObj,"} or
                   an object of class \code{"list"} containing 
                   objects of class \code{"modelObjSubset."}
                  This object specifies the model(s) of the
                  propensity score regression
                  and the R methods to be used to obtain parameter
                  estimates and predictions.
                  The method(s) specified to obtain predictions must return
                  the prediction on the scale of the probability,
                  i.e., returned values must be in the interval (0,1).
                  If performing a single-decision-point analysis,
                  \code{moPropen} is an object of class \code{"modelObj"} or
                  a list of objects of class \code{"modelObjSubset."}
                  If performing a multiple-decision-point analysis,
                  \code{moPropen} is a list of objects of class \code{"modelObj"} or
                  a list of objects of class \code{"modelObjSubset."}
                  See \link{moPropen} for further information.}

 \item{moMain   }{An object of class \code{"modelObj,"} 
                   an object of class \code{"list"} containing 
                   objects of class \code{"modelObj,"} or
                   an object of class \code{"list"} containing 
                   objects of class \code{"modelObjSubset."}
                  This object specifies the model(s) of the
                  main effects component of the outcome regression(s)
                  and the R methods to be used to obtain parameter
                  estimates and predictions.
                  The method chosen to obtain predictions must return
                  the prediction on the scale of the response variable.
                  If performing a single-decision-point analysis,
                  moMain is an object of class \code{"modelObj"} or
                  a list of objects of class \code{"modelObjSubset."}
                  If performing a multiple-decision-point analysis,
                  moMain is a list of objects of class \code{"modelObj"} or
                  a list of objects of class \code{"modelObjSubset."}}

 \item{moCont   }{An object of class \code{"modelObj,"} 
                   an object of class \code{"list"} containing 
                   objects of class \code{"modelObj,"} or
                   an object of class \code{"list"} containing 
                   objects of class \code{"modelObjSubset."}
                  This object specifies the model(s) of the
                  contrasts component of the outcome regression(s)
                  and the R methods to be used to obtain parameter
                  estimates and predictions.
                  The method chosen to obtain predictions must return
                  the prediction on the scale of the response variable.
                  If performing a single-decision-point analysis,
                  moCont is an object of class \code{"modelObj"} or
                  a list of objects of class \code{"modelObjSubset."}
                  If performing a multiple-decision-point analysis,
                  moCont is a list of objects of class \code{"modelObj"} or
                  a list of objects of class \code{"modelObjSubset."}}

 \item{data     }{An object of class \code{"data.frame."} 
                  The covariates and treatment histories.}

  \item{response}{An object of class \code{"vector."}
                  A vector of the outcome of interest.}

 \item{txName   }{An object of class \code{"character."}
                  For a single-decision-point analysis, the column 
                  header of the stage treatment variable
                  as given in input \code{data}.
                  For multiple-decision-point analyses, a vector, the
                  ith element of which gives the column header of \code{data}
                  containing the treatment variable for the ith stage.}

 \item{regimes  }{An object of class \code{"function"} or
                  an object of class \code{"list"} 
                  containing objects of class \code{"function."}
                  Function(s) defining the class of decision rule
                  to be considered. 
                  See Details section for further information}

 \item{fSet     }{An object of class \code{"function,"}
                  an object of class \code{"list"} 
                  containing objects of class \code{"function,"}
                  or NULL.
                  See \link{fSet} section for further information.

}

 \item{refit    }{No longer used.}

  \item{iter    }{An object of class \code{"numeric."}
                  If >0, the iterative method will be used to 
                  obtain parameter estimates in the outcome regression step.
                  See \link{iter} for further information.}

  \item{verbose }{An object of class \code{"logical."} 
                  If FALSE, screen prints will be suppressed.}

}

\details{

The IPWE estimator can be chosen by specifying moMain and moCont as NULL.

The method uses a genetic algorithm (R package \pkg{rgenoud}) to maximize the 
AIPWE/IPWE estimator over the class of treatment regimes specified by the
treatment rules. 

A regimes function defines the class of the decision rule
to be considered. The formal arguments of each 
function must include the parameters to be estimated
and the data.frame.
NOTE: THE LAST ARGUMENT OF THE FUNCTION MUST BE THE 
DATA FRAME. For example, 
for \deqn{d_i = I(eta_1 > x1),}
\preformatted{
regimes <- function(a,data)\{
  as.numeric(a > data\$x1)
\} }


For a single-decision-point analysis, \code{regimes}
is a single function. For multiple-decision-point analyses,
\code{regimes} is a list of functions where each element of 
the list corresponds to the decision point (1st element <-
1st decision point, etc.)]

}

\value{
       Returns an object of class \code{"OptimalSeq"} that inherits directly from class \code{"DynTxRegime."} 
}

\section{Methods}{
  \describe{
    \item{coef}{\code{signature(object = "OptimalSeq")}: 
       Retrieve parameter estimates for all regression steps. }

    \item{DTRstep}{\code{signature(object = "OptimalSeq")}: 
       Retrieve description of method used to create object. }

    \item{estimator}{\code{signature(x = "OptimalSeq")}: 
       Retrieve the estimated value of the estimated
       optimal regime for the training data set. }

    \item{fitObject}{\code{signature(object = "OptimalSeq")}: 
       Retrieve value object returned by regression methods. }

    \item{genetic}{\code{signature(object = "OptimalSeq")}: 
       Retrieve value object returned by genoud().  }

    \item{optTx}{\code{signature(x = "OptimalSeq", newdata = "missing")}: 
       Retrieve the estimated optimal treatment regime for
       training data set. }

    \item{optTx}{\code{signature(x = "OptimalSeq", newdata = "data.frame")}: 
       Estimate the optimal treatment regime for newdata. }

    \item{outcome}{\code{signature(x = "OptimalSeq")}: 
       Retrieve value object returned by outcome regression methods.  }

    \item{plot}{\code{signature(x = "OptimalSeq")}: 
       Generate plots for regression analyses.  }

    \item{print}{\code{signature(object = "OptimalSeq")}: 
       Print main results of analysis.}

    \item{propen}{\code{signature(x = "OptimalSeq")}: 
       Retrieve value object returned by propensity score regression methods.}

    \item{show}{\code{signature(object = "OptimalSeq")}: 
       Show main results of analysis.}

    \item{summary}{\code{signature(object = "OptimalSeq")}: 
       Retrieve summary information from regression analyses.}

  }
}

\references{
Zhang, B., Tsiatis, A. A., Laber, E. B., and Davidian, M.
(2012).
A Robust Method for Estimating Optimal Treatment Regimes.
Biometrics, 68, 1010--1018.

Zhang, B., Tsiatis, A. A., Laber, E. B., and Davidian, M.
(2013)
Robust Estimation of Optimal Dynamic Treatment Regimes for Sequential
Treatment Decisions.
Biometrika, 100, 681--694.

Mebane, W. and Sekhon, J. S.
(2011).
Genetic Optimization Using Derivatives : The rgenoud package for R.
Journal of Statistical Software, 42, 1--26.
}

\author{
Shannon T. Holloway <sthollow@ncsu.edu>
}


\examples{

# Load and process data set
  data(bmiData)

  # define response y to be the negative 12 month
  # change in BMI from baseline
  bmiData$y <- -100*(bmiData[,6] - bmiData[,4])/bmiData[,4]

# Define the propensity for treatment model and methods.
  moPropen <- buildModelObj(model =  ~ 1, 
                            solver.method = 'glm', 
                            solver.args = list('family'='binomial'),
                            predict.method = 'predict.glm',
                            predict.args = list(type='response'))


# Create modelObj object for main effect component
  moMain <- buildModelObj(model = ~ gender + parentBMI + month4BMI,
                          solver.method = 'lm')

# Create modelObj object for contrast component
  moCont <- buildModelObj(model = ~ parentBMI + month4BMI,
                          solver.method = 'lm')

# treatment regime rules at each decision point.
  regimes <- function(a,b,c,data){
               tst <- a + b*data$parentBMI + c*data$month4BMI > 0 
               res <- character(nrow(data))
               res[tst] <- "MR"
               res[!tst] <- "CD"
               return(res)
             }

# genoud requires some additional information 
  c1 <- c(-1,-1,-1)
  c2 <- c( 1, 1, 1)
  Domains <- cbind(c1,c2)
  starts <- c(0,0,0)

#!! A LARGER VALUE FOR POP.SIZE IS RECOMMENDED
#!! THIS VALUE WAS CHOSEN TO MINIMIZE RUN TIME OF EXAMPLES
  pop.size <- 50
\dontrun{
  ft <- optimalSeq(moPropen = moPropen, moMain = moMain, moCont = moCont, 
                   data = bmiData,  response = bmiData$y,  txName = "A2", 
                   regimes = regimes, iter = 0L,
                   pop.size = pop.size, starting.values = starts, 
                   Domains = Domains, solution.tolerance = 0.0001)

## Available methods

  # Coefficients of the propensity score and outcome regression
  coef(ft)

  # Description of method used to obtain object
  DTRstep(ft)

  # Estimated value of estimated optimal treatment for training data
  estimator(ft)

  # Value object returned by outcome regression method
  fitObject(ft)

  # Value object returned by genetic algorithm
  genetic(ft)

  # Estimated optimal treatment for training data
  optTx(ft)

  # Estimated optimal treatment for new data
  optTx(ft, newdata = bmiData)

  # Value object returned by outcome regression method
  outcome(ft)

  # Plots if defined by outcome regression method
  dev.new()
  par(mfrow = c(2,4))
  plot(ft)

  dev.new()
  par(mfrow = c(2,4))
  plot(ft, suppress = TRUE)

  # Value object returned by propensity score regression method
  propen(ft)

  # Estimated decision function parameters
  regimeCoef(ft)

  # Show main results of method
  show(ft)

  # Show summary results of method
  summary(ft)

}

}
