\name{plugInValue}
\alias{plugInValue}
\title{Estimate Plug-in Value }

\description{Estimate the plug-in value of a fixed treatment regime.}
\usage{ plugInValue(optTx1, optTx2, response, tx1, tx2) }

\arguments{
  \item{optTx1   }{Object of class \code{vector}.
                   First-stage treatments corresponding to the 
                   first-stage decision rule of the proposed regime}
  \item{optTx2   }{Object of class \code{vector}.
                   Second-stage treatments corresponding to the 
                   second-stage decision rule of the proposed regime}
  \item{response }{Object of class \code{vector}.
                   Responses}
  \item{tx1      }{Object of class \code{vector}.
                   First-stage randomized treatments}
  \item{tx2      }{Object of class \code{vector}.
                   Second-stage randomized treatments}
}

\details{
The formula for the plug-in value estimate is

\deqn{\frac{\sum_i Y_i*ind1_i*ind1_i}{\sum_i ind1_i*ind2_i }}{%
(\sum_i Y_i*ind1_i*ind2_i)/(\sum_i ind1_i*ind2_i)}

where \eqn{ind1} and \eqn{ind2} are indicators that the first- and
second-stage randomized treatments were consistent with the regime of interest.
}
\value{
  \item{value     }{estimated plug-in value of the regime}
  \item{fixedReg  }{estimated plug-in value of all possible fixed regimes}
}
\references{
  Laber, E.B., Linn, K.A., and Stefanski, L.A. 
(2014). 
  Interactive Q-learning. 
Biometrika, in press.
}
\author{
  Kristin A. Linn, Eric B. Laber, Leonard A. Stefanski,
  and Shannon T. Holloway <sthollow@ncsu.edu>
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
    # Create modeling object for main effect component
    #----------------------------------------------------#
    moMain <- buildModelObj(model = ~ gender + parentBMI + month4BMI,
                       solver.method = 'lm')

    #----------------------------------------------------#
    # Create modeling object for contrast component
    #----------------------------------------------------#
    moCont <- buildModelObj(model = ~ parentBMI + month4BMI,
                       solver.method='lm')

    fitIQ2 <- iqLearnSS(moMain = moMain, 
                        moCont = moCont,
                        data = bmiData, 
                        response = bmiData$y, 
                        txName = "A2", 
                        iter = 0)
 
  ##########################################################
  # Model conditional expected value of main effect term
  ##########################################################

    #----------------------------------------------------#
    # Create modeling object for main effect component
    #----------------------------------------------------#
    moMain <- buildModelObj(model = ~ gender + race + parentBMI + baselineBMI,
                       solver.method = 'lm')

    #----------------------------------------------------#
    # Create modeling object for contrast component
    #----------------------------------------------------#
    form <- 
    moCont <- buildModelObj(model = ~ gender + parentBMI,
                       solver.method = 'lm')

    fitIQ1main <- iqLearnFSM(moMain = moMain, 
                             moCont = moCont, 
                             response = fitIQ2, 
                             data = bmiData, 
                             txName = "A1", 
                             iter = 100) 

  ##########################################################
  # Model conditional mean of contrast function
  ##########################################################
    #----------------------------------------------------#
    # Create modeling object for main effect component
    #----------------------------------------------------#
    form <- 
    moMain <- buildModelObj(model = ~ gender + race + parentBMI + baselineBMI,
                       solver.method = 'lm')

    #----------------------------------------------------#
    # Create modeling object for contrast component
    #----------------------------------------------------#
    moCont <- buildModelObj(model = ~ gender + parentBMI + baselineBMI,
                       solver.method = 'lm')

    fitIQ1cm <- iqLearnFSC(moMain = moMain, 
                           moCont = moCont,  
                           response = fitIQ2,  
                           data = bmiData,  
                           txName = "A1",  
                           iter = 0)

  ##########################################################
  # Variance Modeling
  ##########################################################

    #----------------------------------------------------#
    # Create modeling object for main effect component
    #----------------------------------------------------#
    moMain <- buildModelObj(model = ~ gender + race + parentBMI + baselineBMI,
                       solver.method = 'lm')

    #----------------------------------------------------#
    # Create modeling object for contrast component      #
    #----------------------------------------------------#
    moCont <- buildModelObj(model = ~ parentBMI + baselineBMI,
                       solver.method='lm')


    fitIQ1var <- iqLearnFSV(object = fitIQ1cm,
                            moMain = moMain, 
                            moCont = moCont, 
                            data = bmiData, 
                            txName = "A1", 
                            iter = 100)

  ##########################################################
  # Optimal Treatment for first stage
  ##########################################################
    optimalTx <- optTx(x = fitIQ1main, 
                       y = fitIQ1cm, 
                       z = fitIQ1var, 
                       dens = "nonpar")

  ##########################################################
  # Plug-in Values
  ##########################################################
    plugInValue(optTx1 = optimalTx$optimalTx, 
                optTx2 = optTx(fitIQ2)$optimalTx, 
                response = bmiData$y,   
                tx1 = bmiData$A1,   
                tx2 = bmiData$A2)

}
