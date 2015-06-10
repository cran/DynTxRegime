\name{optimalSeq}
\alias{optimalSeq}
\title{
Regression Based Value-Search Estimation of Optimal Dynamic Treatment Regimes
}
\description{
A regression based value-search alternative to Q- and A- learning. A doubly robust Augmented Inverse Probability Weighted Estimator (AIPWE) or Inverse Probability Weighted Estimator (IPWE) for population mean outcome is optimized over a restricted class of regimes. Methods are available for both single-decision-point and multiple-decision-point regimes. This method requires the rgenoud package. 
}

\usage{
optimalSeq(..., moPropen, moMain, moCont, data, response, txName, regimes,
 fSet = NULL, refit = FALSE, iter = 0, suppress = FALSE)
}
\arguments{

 \item{...          }{additional arguments required by rgenoud. At a minimum,
                      this should include Domains, pop.size and starting.values. 
                      See ?rgenoud for more information.}

 \item{moPropen  }{a single object of class modelObj or a list of objects of 
                  class modelObj or {modelObjSubset}.
                  This object defines the, which define the models and R 
                  methods to be used to obtain parameter estimates and
                  predictions for the propensity for treatment. 
                  The method chosen to obtain predictions must return
                  the prediction on the scale of the response variable.
                  See ?modelObj and/or ?modelObjSubset for details. 

                  moPropen objects differ slightly from the standard modelObj 
                  in that an additional predict.args element may be required. 
                  In the list of control parameters passed to the prediction
                  method, an additional list element, "propenMissing" can be
                  included. This element takes value "smallest" or "largest". 

                  If the prediction method specified in moPropen returns
                  predictions for only a subset of the categorical tx data, 
                  propenMissing indicates if the smallest or largest category 
                  is missing category.
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

 \item{moMain   }{a single object of class modelObj or a list of objects of 
                  class modelObj or {modelObjSubset}.
                  This object defines the models and R
                  methods to be used to obtain parameter estimates and
                  predictions for for the main effects component of the
                  outcome regression.
                  The method chosen to obtain predictions must return
                  the prediction on the scale of the response variable.
                  See ?modelObj and/or ?modelObjSubset for details.}

 \item{moCont   }{a single object of class modelObj or a list of objects of 
                  class modelObj or {modelObjSubset}.
                  This object defines the models and R
                  methods to be used to obtain parameter estimates and
                  predictions for for the contrasts component of the
                  outcome regression.
                  The method chosen to obtain predictions must return
                  the prediction on the scale of the response variable.
                  See ?modelObj and/or ?modelObjSubset for details.}

 \item{data     }{an object of class {data.frame} containing the
                  covariates and treatment histories.}

 \item{response }{an object of class {vector} containing the response variable.}

 \item{txName   }{and object of class {numeric}.
                  For a single decision point analysis, the column
                  index of {data} containing the treatment variable.
                  For multiple decision point analyses, a vector, the
                  ith element of which gives the column of {data}
                  containing the treatment variable for the ith stage.}

 \item{regimes  }{an object of class {list} or {function}.
                  For single decision point analyses, {regimes}
                  is a single function. For multiple decision point analyses,
                  {regimes} is a list of functions;
                  each element of the list 
                  corresponds to the decision point (1st element <-
                  1st decision point, etc.) 
                  The function defines the treatment regime for the
                  decision point. 
                  For example, for \deqn{g_i <- I(eta_1 > x1),}
                       \preformatted{
regimes[[i]] = function(a,data)\{
  as.numeric(a > data\$x1)
\} }
 
                  The formal arguments of each function must include the
                  parameters to be estimated and the data.frame.
                  NOTE: THE LAST ARGUMENT OF EACH FUNCTION MUST BE THE 
                  DATA FRAME.}

 \item{fSet     }{Object of class {list} or {function}.
                  For single decision point analyses, {fSet}
                  is a single function. For multiple decision point analyses,
                  {fSet} is a list of functions;
                  each element of the list 
                  corresponds to the decision point (1st element <-
                  1st decision point, etc.) 
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

 \item{refit    }{an object of class {logical}.
                  TRUE/FALSE flag indicating if conditional expectations 
                  are to be refit for each new set of treatment regime
                  parameters (TRUE), or Q-learning is to be used (FALSE). 
                  Choosing TRUE significantly increases the run time.}

  \item{iter    }{an object of class {numeric}.

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

  \item{suppress}{an object of class logical. If TRUE, final screen prints 
                  will be suppressed.}

}

\details{

A dynamic treatment regime is a list of sequential decision rules for
assigning treatment based on a patient's history. Q- and A- learning
are common approaches to obtain estimates of the ``optimal treatment regime.''
\code{optimalSeq} is an alternative approach that maximizes a doubly
robust Augmented Inverse Probability Weighted estimator (AIPWE) for population
mean outcome over a restricted class of regimes. 

For a single decision point treatment regime, the problem is recast as a 
missing data problem. The user specifies models for the propensity for 
treatment and the conditional expectation of the outcome given
covariates and treatment. For sequential decision point 
treatment regimes, the problem is recast as a monotone coarsening problem. 
The user provides a model for the propensity for treatment for each decision
point (k) 
 and a model for the conditional expectation of the outcome given
covariates and treatment at each
decision point. Parameter estimates for these models are obtained using 
available R methods as specified by the user. The AIPWE estimator for 
E\{Y*(g_{eta})\} is constructed using the user-specified models. Parameter
estimates for the
user defined treatment regime are obtained by maximizing the AIPWE estimator 
via the R package \code{rgenoud}, a genetic algorithm developed by 
Walter Mebane, Jr. and Jasjeet S. Sekhon.

The IPWE estimator can be chosen by specifying moMain and moCont as NULL.

Treatment rules/regimes are provided by the user in the form of user-defined 
functions. 

The method uses a genetic algorithm (R package rgenoud) to maximize the 
AIPWE/IPWE estimator over the class of treatment regimes specified by the
treatment rules. In the maximization of the AIPWE,
conditional expectation models can either be refitted at each value of the
treatment regime or can be approximated by fitted Q-functions obtained
from Q-learning; the latter choice can lead to significant improvements in 
speed.

}

\value{
       Returns an object that inherits directly from class \code{DynTxRegime}.
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
Baqun Zhang, Anastasios A. Tsiatis, Eric B. Laber, 
Marie Davidian, and Shannon T. Holloway <sthollow@ncsu.edu>
}


\examples{

#------------------------------------------------------------------------------#
#  This is simulation study#1 described in Biometrics \bold{68} 4 pp 1010-1018
#------------------------------------------------------------------------------#

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#
#                               Generate data 
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#
  expit <- function(x) { exp(x)/(1+exp(x)) }

  n <- 100

  x1 <- runif(n, -1.5, 1.5)
  x2 <- runif(n, -1.5, 1.5)

  x12 <- x1*x1
  x22 <- x2*x2

  p1 <- expit(-1.0 + 0.8*x12 + 0.8*x22)
  a1 <- rbinom(n=n, size=1, prob=p1)

  mean <- exp(2.0 - 1.5*x12 - 1.5*x22 + 3.0*x1*x2 + a1*(-0.1 - x1 + x2))
  y <- abs(rnorm(n,mean,sd=1))

  data <- data.frame(x1,x12,x2,x22,a1,y)

#--------------------------------------------------------------------------#
# tx.var tells optimal which columns of data correspond to treatments.
#--------------------------------------------------------------------------#
  tx.vars <- "a1"

#--------------------------------------------------------------------------#
# modeling.object for propensity of treatment.
#--------------------------------------------------------------------------#
  moPropen <- buildModelObj(model = ~x12 + x22,
                       solver.method = 'glm',
                       solver.args = list('family'='binomial'),
                       predict.method = 'predict.glm',
                       predict.args = list(type='response'))


#--------------------------------------------------------------------------#
# modeling.object for conditional expectation models
#--------------------------------------------------------------------------#
  expec.main <- buildModelObj(model = ~ x12 + x22 + x1:x2 + a1 + a1:(x1 + x2),
                              solver.method = 'lm')
  expec.cont <- NULL


#--------------------------------------------------------------------------#
# treatment regime rules at each decision point.
#--------------------------------------------------------------------------#
  tx.rules <- function(a,b,c,data){
                 as.numeric(a + b*data$x1 + c*data$x2 > 0 )}

#--------------------------------------------------------------------------#
# genoud requires some additional information 
#--------------------------------------------------------------------------#
  c1 <- c(-1,-1,-1)
  c2 <- c( 1, 1, 1)
  Domains <- cbind(c1,c2)
  starts <- c(0,0,0)

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
#!!              A LARGER VALUE FOR POP.SIZE IS RECOMMENDED             !!#
#!!        THIS VALUE WAS CHOSEN TO MINIMIZE RUN TIME OF EXAMPLES       !!#
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
  pop.size <- 50

  ft <- optimalSeq(moPropen = moPropen, 
                moMain = expec.main, 
                moCont = expec.cont, 
                data = data, 
                response = y, 
                txName = tx.vars, 
                regimes = tx.rules, 
                pop.size = pop.size, 
                starting.values = starts, 
                Domains = Domains, 
                solution.tolerance = 0.0001, 
                iter = 0)

  # The  normalized estimated optimal treatment regime
  # True Regime (-0.07, -0.71, 0.71)
  est <- regimeCoef(ft)
  est <- est/sqrt(est \%*\% est)
  print(est)

  # The estimated mean potential outcome for the treatment regime 
  # defined by the regime
  estimator(ft)

  # Access value objects of regression steps
  genetic(ft)
  outcome(ft)
  propen(ft)

  # Retrieve optimal treatment for training data
  head(optTx(ft))

  summary(ft)


}


\keyword{robust}
\keyword{regression}
