\name{qLearn}
\alias{qLearn}

\title{ Q-learning}

\description{Regresses the outcome on
             stage history and treatment to estimate the optimal 
             decision rule for the given stage using Q-learning.
}

\usage{ qLearn(..., moMain, moCont, data, response, txName, fSet=NULL, iter=0L,
               suppress = FALSE) }

\arguments{
  \item{\dots   }{ignored}

 \item{moMain   }{a single object of class modelObj or a list of objects of 
                  class {modelObj} or {modelObjSubset}.
                  This object defines the models and R
                  methods to be used to obtain parameter estimates and
                  predictions for for the main effects component of the
                  outcome regression.
                  The method chosen to obtain predictions must return
                  the prediction on the scale of the response variable.
                  See ?modelObj and/or ?modelObjSubset for details.}

 \item{moCont   }{a single object of class modelObj or a list of objects of 
                  class {modelObj} or {modelObjSubset}.
                  This object defines the models and R
                  methods to be used to obtain parameter estimates and
                  predictions for for the contrasts component of the
                  outcome regression.
                  The method chosen to obtain predictions must return
                  the prediction on the scale of the response variable.
                  See ?modelObj and/or ?modelObjSubset for details.}

 \item{data     }{an object of class \code{data.frame} containing the
                  covariates and treatment histories.}

  \item{response }{an object of class {vector} or {qLearn}.
                   If performing the first STEP of the Q-Learning algorithm,
                   i.e., the final-stage regression, response is
                   the vector outcome of interest. For all other
                   steps of the Q-Learning algorithm, response is
                   the value object returned by the previous Q-Learning step.}

 \item{txName   }{an object of class \code{character};
                  the column header of \code{data} containing the 
                  treatment variable.}

 \item{fSet     }{Object of class {function}.
                  The function defines the conditions under which only
                  a subset of treatment options is available to a patient
                  or a subset of patients, for whom different models will be
                  used.
                  The formal arguments of the function should include either 
                  the name of the data.frame or individual covariate names
                  as given by the column headers of {data}. The function
                  must return a list. The first element of the list
                  is a character nickname. The
                  second element is a vector of available treatment options.

                  For example

                        \preformatted{ 
fSet <- function(data)
\{ 
   if(data\$a1 > 1)\{ 
     subset <- list("A",c(1,2))
   \} else \{ 
     subset <- list("B",c(3,4) )
   \} 
   return(subset) 
\} }
                        or 
                        \preformatted{
fSet <- function(a1)
\{
  if(a1 > 1)\{
    subset <- list("A",c(1,2))
  \} else \{
    subset <- list("B",c(3,4) )
  \}
  return(subset)
\}}

                     }

  \item{iter    }{Object of class {numeric}.

                 >=1 if {moMain} and {moCont} are to be
                 fitted separately, iter is the maximum number of iterations.
                 Assume Y = Ymain + Ycont;
                 the iterative algorithm is as follows:
                  (1) hat(Ycont) = 0;
                  (2) Ymain = Y - hat(Ycont);
                  (3) fit Ymain ~ moMain;
                  (4) set Ycont = Y - hat(Ymain)
                  (5) fit Ycont ~ A*moCont;
                  (6) Repeat steps (2) - (5) until convergence or
                  a maximum of iter iterations.

                 <=0 if the components of the conditional expectation 
                 {moMain} and {moCont} will be combined and 
                 fit as a single object.
                 Note that if iter <= 0, all non-model components of the
                 {moMain} and {moCont} must be
                 identical. By default, the choices in moMain are used.}


  \item{suppress }{Object of class logical. If TRUE, all screen prints will be 
                   suppressed.}
}

\value{
       Returns an object that inherits directly from class {DynTxRegime}.
}

\references{
  Laber, E. B., Linn, K. A., and Stefanski, L. A. 
(2014). 
  Interactive Q-learning. 
Biometrika, in press.
}
\author{
  Kristin A. Linn, Eric B. Laber, Leonard A. Stefanski, and
  Shannon T. Holloway <sthollow@ncsu.edu>
}

\examples{
  ##########################################################
  # Load and process data set
  ##########################################################
    data(bmiData)

    #----------------------------------------------------#
    # Recast treatment variables to (0,1)
    #----------------------------------------------------#
    bmiData$A1[which (bmiData$A1=="MR")] <- 1L
    bmiData$A1[which (bmiData$A1=="CD")] <- 0L
    bmiData$A2[which (bmiData$A2=="MR")] <- 1L
    bmiData$A2[which (bmiData$A2=="CD")] <- 0L
    bmiData$A1 <- as.integer(bmiData$A1)
    bmiData$A2 <- as.integer(bmiData$A2)

    #----------------------------------------------------#
    # define response y to be the negative 12 month
    # change in BMI from baseline
    #----------------------------------------------------#
    bmiData$y <- -100*(bmiData[,6] - bmiData[,4])/bmiData[,4]

  ##########################################################
  # Second-stage regression
  ##########################################################
    #----------------------------------------------------#
    # Create modeling object for main effect component
    #----------------------------------------------------#
    moMain <- buildModelObj(model = ~ gender + parentBMI + month4BMI,
                       solver.method='lm')

    #----------------------------------------------------#
    # Create modeling object for contrast component
    #----------------------------------------------------#
    moCont <- buildModelObj(model = ~ parentBMI + month4BMI,
                       solver.method='lm')

    fitQ2 <- qLearn(moMain = moMain, 
                    moCont = moCont, 
                    data = bmiData,  
                    response = bmiData$y, 
                    txName = "A2", 
                    iter = 0)
 
  ##########################################################
  # First-stage regression
  ##########################################################

    #----------------------------------------------------#
    # Create modeling object for main effect component
    #----------------------------------------------------#
    moMain <- buildModelObj(model = ~ gender + race + parentBMI + baselineBMI,
                       solver.method='lm')

    #----------------------------------------------------#
    # Create modeling object for contrast component
    #----------------------------------------------------#
    moCont <- buildModelObj(model = ~ gender + parentBMI,
                      solver.method='lm')

    fitQ1 <- qLearn(moMain = moMain, 
                    moCont = moCont,  
                    response = fitQ2, 
                    data = bmiData,  
                    txName = "A1",  
                    iter = 100)

    # Coefficients of regressions
    coef(fitQ1)

    # Residuals
    head(residuals(fitQ1))

    # Summary
    #summary(fitQ1)

    # Plots
    plot(fitQ1)

    # List of value objects returned by modeling function
    fitObj <- fitObject(fitQ1)
    fitObj

    # All standard lm  methods can be applied to this fit object
    summary(fitObj$MainEffect)
    coef(fitObj$MainEffect)
    head(residuals(fitObj$MainEffect))

    summary(fitObj$Contrast)
    coef(fitObj$Contrast)
    head(residuals(fitObj$Contrast))

}
