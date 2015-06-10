\name{iqLearnFSC}
\alias{iqLearnFSC}

\title{IQ-Learning: First-Stage Regression of Second-Stage Estimated Contrasts}

\description{
Estimate an optimal dynamic treatment regime using the 
Interactive Q-learning (IQ-learning) algorithm when the data has been 
collected from a two-stage randomized trial with binary treatments
coded as \{-1,1\}. 
iqLearnFSC implements the first-stage regression of the 
estimated second-stage contrasts in the IQ-Learning algorithm (IQ3).
}

\usage{ iqLearnFSC(..., moMain, moCont, data, response, txName, 
                   iter = 0, suppress = FALSE) }

\arguments{

  \item{\dots   }{ignored}

  \item{moMain  }{an object of class \code{modelObj} defining the
                  regression model and procedures to be used 
                  for the main effects component of this regression step.
                  The object must be created by a call to 
                  modelObj::buildModelObj().
                  The method chosen to obtain predictions must return
                  the prediction on the scale of the response variable.
                  See ?modelObj for details.}

  \item{moCont  }{an object of class \code{modelObj} defining the
                  regression model and procedures to be used 
                  for the contrast component of this regression step.
                  The object must be created by a call to 
                  modelObj::buildModelObj().
                  The method chosen to obtain predictions must return
                  the prediction on the scale of the response variable.
                  See ?modelObj for details.}

  \item{data    }{an object of class \code{data.frame}
                  containing the covariates and treatment history.
                  Treatment must be an integer (or able to be cast to an integer)
                  and take values \{-1,1\}.}

  \item{response}{an object is of class \code{iqLearnSS}: 
                  the value object returned by a previous call to 
                  \code{iqLearnSS()}.}

  \item{txName  }{an object of class \code{character}:
                  the column header of the
                  first-stage treatment variable in \code{data}.}

  \item{iter   }{an object of class \code{integer}.

                 >=1 if \code{moMain} and \code{moCont} are to be
                 fitted separately, iter is the maximum number of iterations.
                 Assume Y = Ymain + Ycont;
                 the iterative algorithm is as follows:
                  (1) hat(Ycont) = 0;
                  (2) Ymain = Y - hat(Ycont);
                  (3) fit Ymain ~ moMain;
                  (4) set Ycont = Y - hat(Ymain);
                  (5) fit Ycont ~ A*moCont;
                  (6) Repeat steps (2) - (5) until convergence or
                  a maximum of iter iterations.

                 <=0 if the components of the conditional expectation 
                 \code{moMain} and \code{moCont} will be combined and 
                 fit as a single object.
                 Note that if iter <= 0, all non-model components of the
                 \code{moMain} and \code{moCont} must be
                 identical. By default, the choices in moMain are used.}

  \item{suppress }{an object of class \code{logical}. If TRUE, final
                   screen prints are suppressed.}

}

\details{

There are standard regression analysis tools available for the
object returned by this function. In general, these tools 
simply extend the methods defined by the regression function. 
If defined, \code{coef()} returns the model
parameter estimates; \code{plot()} generates the standard x-y plots; 
\code{residuals} returns model residuals for the combined model; and
\code{summary} returns summary information. 

Other tools, such as \code{fitted()} for the \code{lm}
regression function, can be accessed using \code{fitObject()}. 
\code{fitObject()} retrieves the standard value object returned by the
regression method, which can be passed as input to other functions. 
See ?fitObject for details.

}

\value{
       Returns an object that inherits directly from class \code{DynTxRegime}.
}

\references{
  Laber, E. B., Linn, K. A., and Stefanski, L.A. 
  (2014). 
  Interactive Q-learning. 
  Biometrika, in press.
}

\author{
  Kristin A. Linn, Eric B. Laber, Leonard A. Stefanski, and
  Shannon T. Holloway <sthollow@ncsu.edu>
}

\seealso{
  \code{\link{iqLearnSS}}, 
  \code{\link{iqLearnFSM}}
  \code{\link{iqLearnFSV}}
}

\examples{
  ##########################################################
  # Load and process data set
  ##########################################################
    data(bmiData)

    #----------------------------------------------------#
    # Recast treatment variables to (-1,1) --- required.
    #----------------------------------------------------#
    bmiData$A1[which (bmiData$A1=="MR")] <- 1L
    bmiData$A1[which (bmiData$A1=="CD")] <- -1L
    bmiData$A2[which (bmiData$A2=="MR")] <- 1L
    bmiData$A2[which (bmiData$A2=="CD")] <- -1L
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
    # Create modelObj object for main effect component
    #----------------------------------------------------#
    moMain <- buildModelObj(model = ~ gender + parentBMI + month4BMI,
                            solver.method = 'lm')

    #----------------------------------------------------#
    # Create modelObj object for contrast component
    #----------------------------------------------------#
    moCont <- buildModelObj(model = ~ parentBMI + month4BMI,
                            solver.method = 'lm')

    iqSS <- iqLearnSS(moMain = moMain, 
                      moCont = moCont, 
                      data = bmiData,
                      response = bmiData$y, 
                      txName = "A2", 
                      iter = 0)

  ##########################################################
  # First-stage regression - Single Regression Analysis
  ##########################################################
    #----------------------------------------------------#
    # Create modelObj object for main effect component
    #----------------------------------------------------#
    moMain <- buildModelObj(model = ~ gender + race + parentBMI + baselineBMI,
                            solver.method = 'lm')

    #----------------------------------------------------#
    # Create modelObj object for contrast component
    #----------------------------------------------------#
    moCont <- buildModelObj(model = ~ parentBMI + month4BMI,
                            solver.method = 'lm')

    iqFSC <- iqLearnFSC(moMain = moMain, 
                        moCont = moCont, 
                        data = bmiData,
                        response = iqSS, 
                        txName = "A1", 
                        iter = 0)

    # Estimated Q-functions
    vals <- qFuncs(iqFSC)
    head(vals)

    # Residuals
    res <- residuals(iqFSC)
    head(res)

    # Model parameter estimates
    coef(iqFSC)

    # Summary information for fit object
    summary(iqFSC)

    # Standard x-y plots
    plot(iqFSC)

    # Value objects returned by modeling function
    fitObj <- fitObject(iqFSC)
    fitObj

    # All standard lm  methods can be applied to the elements of this list.
    summary(fitObj[[ "Combined" ]])
    coef(fitObj[[ "Combined" ]])
    head(residuals(fitObj[[ "Combined" ]]))
    head(fitted.values(fitObj[[ "Combined" ]]))
    plot(fitObj[[ "Combined"]])

  ##########################################################
  # First-stage regression - Iterative Regression Analysis
  ##########################################################
    #----------------------------------------------------#
    # Create modelObj object for main effect component
    #----------------------------------------------------#
    moMain <- buildModelObj(model = ~ gender + race + parentBMI + baselineBMI,
                            solver.method = 'lm')

    #----------------------------------------------------#
    # Create modelObj object for contrast component
    #----------------------------------------------------#
    moCont <- buildModelObj(model = ~ parentBMI + month4BMI,
                            solver.method = 'lm')

    iqFSC <- iqLearnFSC(moMain = moMain, 
                        moCont = moCont, 
                        data = bmiData,
                        response = iqSS, 
                        txName = "A1", 
                        iter = 100)

    # Estimated Q-functions
    vals <- qFuncs(iqFSC)
    head(vals)

    # Residuals
    res <- residuals(iqFSC)
    head(res)

    # Model parameter estimates
    coef(iqFSC)

    # Summary information for fit object
    summary(iqFSC)

    # Standard x-y plots
    plot(iqFSC)

    # Value objects returned by modeling function
    fitObj <- fitObject(iqFSC)
    fitObj

    # All standard lm  methods can be applied to the elements of this list.
    summary(fitObj[[ "MainEffect" ]])
    coef(fitObj[[ "MainEffect" ]])
    head(residuals(fitObj[[ "MainEffect" ]]))
    head(fitted.values(fitObj[[ "MainEffect" ]]))
    plot(fitObj[[ "MainEffect"]])

    summary(fitObj[[ "Contrast" ]])
    coef(fitObj[[ "Contrast" ]])
    head(residuals(fitObj[[ "Contrast" ]]))
    head(fitted.values(fitObj[[ "Contrast" ]]))
    plot(fitObj[[ "Contrast"]])

}
