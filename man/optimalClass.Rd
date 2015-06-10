\name{optimalClass}
\alias{optimalClass}
\title{
Classification Based Robust Estimation of Optimal Dynamic Treatment Regimes
}
\description{
Implements a classification based alternative 
to Q- and A- learning that uses an Augmented Inverse Probability Weighted
Estimator (AIPWE) or an Inverse Probability Weighted Estimator (IPWE) 
of the contrast function to define a weighted classification
problem. The method is limited to single decision point binary treatment regimes. 
}
\usage{ optimalClass(..., moPropen, moMain, moCont, moClass, 
data, response, txName, iter=0L, suppress = FALSE)
}
\arguments{
 \item{...      }{ignored.}

 \item{moPropen }{an object of class \code{modelObj}.
                  This object defines the
                  models and R methods to be used to obtain parameter          
                  estimates and predictions for the propensity for treatment   
                  The method chosen to obtain predictions must return
                  the prediction on the scale of the response variable.
                  See ?modeling.object for details. 

                  moPropen objects differ slightly from the standard modelObj
                  in that an additional predict.args element may be required.
                  In the list of control parameters passed to the prediction
                  method, an additional list element, "propenMissing" can be
                  included. This element takes value "smallest" or "largest".

                  If the prediction method specified in moPropen returns
                  predictions for only a subset of the treatment data,
                  propenMissing indicates whether it is the smallest or the
                  largest treatment value that is missing, or base case.

                   For example, fitting a binary tx using
\preformatted{
moPropen@predict.method <- predict.glm
moPropen@predict.args <- list(type='response')
}
                   returns only P(A=1). P(A=0) is "missing", and thus
\preformatted{
moPropen@predict.args <- list("type"="response",
                              "propenMissing" = "smallest")
}
                  If the dimension of the value returned by the prediction
                  method is less than the number of treatment options
                  and no value is provided in propenMissing, it is
                  assumed that the smallest valued treatment option is missing.}

 \item{moMain   }{an object of class \code{modelObj}.
                  This object defines the
                  models and R methods to be used to obtain parameter
                  estimates and predictions for the main effects component
                  of the outcome regression.
                  The method chosen to obtain predictions must return
                  the prediction on the scale of the response variable.
                  See ?modelObj for details.}

 \item{moCont   }{an object of class \code{modelObj}.
                  This object defines the
                  models and R methods to be used to obtain parameter 
                  estimates and predictions for the contrast component of
                  the outcome regression. 
                  The method chosen to obtain predictions must return
                  the prediction on the scale of the response variable.
                  See ?modelObj for details.}

 \item{moClass  }{an object of class \code{modelObj}.
                  This object defines the
                  models and R methods to be used to obtain parameter
                  estimates and predictions for the classification.
                  The method chosen to obtain predictions must return
                  the predicted category.
                  See ?modelObj for details.}

 \item{data     }{an object of class \code{data.frame} containing the
                  covariates and treatment histories.}

 \item{response }{an object of class \code{vector} containing the response variable.}

 \item{txName   }{an object of class \code{character};
                  the column header of \code{data} containing the 
                  treatment variable.}

  \item{iter    }{an object of class \code{numeric}.

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

  \item{suppress }{an object of class \code{logical}. If TRUE,
                   final screen prints will be suppressed.}
}

\details{
A single decision point dynamic treatment regime is a decision rule for
assigning treatment based on a patient's history. Several regression
based approaches have been developed to estimate the "optimal dynamic
treatment regime". 'optimalClass' is an alternative approach that
uses a doubly robust Augmented Inverse Probability Weighted 
estimator (AIPWE) or an Inverse Probability Weighted 
estimator (IPWE) for the contrast function to define a weighted 
classification problem.
The resulting classifications scheme defines the optimal treatment regime.

The user provides a model for the propensity for treatment and a model for
the main effects and contrasts of the conditional expectation of the 
outcome given treatment and covariates.
Parameter estimates for these models are obtained using 
available R methods as specified by the user. The AIPWE contrast function
is defined by the fitted models and is used to 
define a weighted classification problem. The R method to fit the 
classification problem is specified by the user with some limitations.

The user can opt for the IPWE estimator by specifying moMain and moCont as NULL.

Generally speaking, there are few limitations on the models that can be 
employed.
However, classification methods must also accept "weights" as a formal 
argument.
In addition, predict methods must return predictions on the scale of the 
response
for propensity and conditional expectations and on the scale of class for
the classification methods. 

}

\value{
       Returns an object that inherits directly from class \code{DynTxRegime}.
}


\author{
Baqun Zhang, Anastasios A. Tsiatis, Eric B. Laber, Min Zhang, 
Marie Davidian, and Shannon T. Holloway \email{sthollow@ncsu.edu}
}

\references{
Zhang, B., Tsiatis, A. A., Davidian, M., Zhang, M., and Laber, E. B.
(2012).
Estimating Optimal Treatment Regimes from a Classification Perspective.
Stat, 1, 103--114
}


\examples{
#------------------------------------------------------------------------------#
#This is simulation study #1 described in Stat 2012; 1: 103-114.
#------------------------------------------------------------------------------#

  library(MASS)
  library(rpart)

  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#
  #Generate data
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#
  expit <- function(x) { exp(x)/(1+exp(x)) }
  n <- 200

  means <- rep(0,5)
  vmat <- matrix(0,nrow=5,ncol=5) + diag(5)

  X <- data.frame(mvrnorm(n,means,vmat))

  p1 <- expit(-0.1 + 0.5*X$X1 + 0.5*X$X2)
  a <- rbinom(n=n, size=1, prob=p1)

  tg <- I(X$X1 > -0.545) * I( X$X2 < 0.545)
  means <- exp( 2 + 0.25*X$X1 + 0.25*X$X2 - 0.25*X$X5 - 0.3*(a-tg)^2)

  y <- sapply(means,function(x){rnorm(n=1, mean=x, sd=1)})

  df <- data.frame(X,a)
  colnames(df) <- c("x1", "x2", "x3", "x4", "x5", "a1")

  #--------------------------------------------------------------------------#
  # Define the propensity for treatment model and methods.
  #--------------------------------------------------------------------------#
  propen <- buildModelObj(model =  ~ x1 + x2, 
                     solver.method = 'glm', 
                     solver.args = list('family'='binomial'),
                     predict.method = 'predict.glm',
                     predict.args = list(type='response'))

  #--------------------------------------------------------------------------#
  # Define the conditional expectation model.
  #--------------------------------------------------------------------------#
  expec.main <- buildModelObj(model = ~x1+x2+x3+x4+x5,
                         solver.method = 'lm')

  expec.cont <- buildModelObj(model = ~x1+x2+x3+x4+x5,
                         solver.method = 'lm')

  #--------------------------------------------------------------------------#
  # Define the classification model.
  #--------------------------------------------------------------------------#
  class <- buildModelObj(model = ~x1 + x2 + x3 + x4 + x5,
                    solver.method = 'rpart',
                    solver.args = list(method="class"),
                    predict.args = list(type='class'))

  #--------------------------------------------------------------------------#
  # Specify the column index of df corresponding to the treatment covariate
  #--------------------------------------------------------------------------#
  tx.vars <- "a1"

  estAIPWE <- optimalClass(moPropen = propen,
                           moMain = expec.main,
                           moCont = expec.cont,
                           moClass = class,
                           data = df,
                           response = y,
                           txName = tx.vars,
                           iter=0)

  estimator(estAIPWE)
  classif(estAIPWE)
  outcome(estAIPWE)
  propen(estAIPWE)
  head(optTx(estAIPWE))


  estIPWE <- optimalClass(moPropen = propen,
                          moMain = NULL,
                          moCont = NULL,
                          moClass = class,
                          data = df,
                          response = y,
                          txName = tx.vars,
                          iter=0)

  estimator(estIPWE)
  classif(estIPWE)
  propen(estIPWE)
  head(optTx(estIPWE))
}



\keyword{robust}
\keyword{tree}
