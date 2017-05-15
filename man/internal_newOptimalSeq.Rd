\name{.newOptimalSeq}
\docType{methods}
\alias{.newOptimalSeq}
\alias{.newOptimalSeq,modelObj,NULL,NULL,Regime,NULL-method}
\alias{.newOptimalSeq,modelObj,NULL,NULL,Regime,function-method}
\alias{.newOptimalSeq,ModelObj_SubsetList,NULL,NULL,Regime,function-method}
\alias{.newOptimalSeq,modelObj,modelObj,modelObj,Regime,NULL-method}
\alias{.newOptimalSeq,modelObj,modelObj,NULL,Regime,NULL-method}
\alias{.newOptimalSeq,modelObj,NULL,modelObj,Regime,NULL-method}
\alias{.newOptimalSeq,modelObj,modelObj,modelObj,Regime,function-method}
\alias{.newOptimalSeq,modelObj,modelObj,NULL,Regime,function-method}
\alias{.newOptimalSeq,modelObj,NULL,modelObj,Regime,function-method}
\alias{.newOptimalSeq,ModelObj_SubsetList,modelObj,modelObj,Regime,function-method}
\alias{.newOptimalSeq,ModelObj_SubsetList,modelObj,NULL,Regime,function-method}
\alias{.newOptimalSeq,ModelObj_SubsetList,NULL,modelObj,Regime,function-method}
\alias{.newOptimalSeq,ModelObj_SubsetList,ModelObj_SubsetList,ModelObj_SubsetList,Regime,function-method}
\alias{.newOptimalSeq,ModelObj_SubsetList,ModelObj_SubsetList,NULL,Regime,function-method}
\alias{.newOptimalSeq,ModelObj_SubsetList,NULL,ModelObj_SubsetList,Regime,function-method}
\alias{.newOptimalSeq,modelObj,ModelObj_SubsetList,ModelObj_SubsetList,Regime,function-method}
\alias{.newOptimalSeq,modelObj,ModelObj_SubsetList,NULL,Regime,function-method}
\alias{.newOptimalSeq,modelObj,NULL,ModelObj_SubsetList,Regime,function-method}
\alias{.newOptimalSeq,ModelObj_DecisionPointList,NULL,NULL,Regime_DecisionPointList,list-method}
\alias{.newOptimalSeq,ModelObj_DecisionPointList,NULL,NULL,Regime_DecisionPointList,NULL-method}
\alias{.newOptimalSeq,ModelObj_SubsetList_DecisionPointList,NULL,NULL,Regime_DecisionPointList,list-method}
\alias{.newOptimalSeq,ModelObj_DecisionPointList,ModelObj_DecisionPointList,ModelObj_DecisionPointList,Regime_DecisionPointList,NULL-method}
\alias{.newOptimalSeq,ModelObj_DecisionPointList,ModelObj_DecisionPointList,NULL,Regime_DecisionPointList,NULL-method}
\alias{.newOptimalSeq,ModelObj_DecisionPointList,NULL,ModelObj_DecisionPointList,Regime_DecisionPointList,NULL-method}
\alias{.newOptimalSeq,ModelObj_DecisionPointList,ModelObj_DecisionPointList,ModelObj_DecisionPointList,Regime_DecisionPointList,list-method}
\alias{.newOptimalSeq,ModelObj_DecisionPointList,ModelObj_DecisionPointList,NULL,Regime_DecisionPointList,list-method}
\alias{.newOptimalSeq,ModelObj_DecisionPointList,NULL,ModelObj_DecisionPointList,Regime_DecisionPointList,list-method}
\alias{.newOptimalSeq,ModelObj_SubsetList_DecisionPointList,ModelObj_DecisionPointList,ModelObj_DecisionPointList,Regime_DecisionPointList,list-method}
\alias{.newOptimalSeq,ModelObj_SubsetList_DecisionPointList,ModelObj_DecisionPointList,NULL,Regime_DecisionPointList,list-method}
\alias{.newOptimalSeq,ModelObj_SubsetList_DecisionPointList,NULL,ModelObj_DecisionPointList,Regime_DecisionPointList,list-method}
\alias{.newOptimalSeq,ModelObj_SubsetList_DecisionPointList,ModelObj_SubsetList_DecisionPointList,ModelObj_SubsetList_DecisionPointList,Regime_DecisionPointList,list-method}
\alias{.newOptimalSeq,ModelObj_SubsetList_DecisionPointList,ModelObj_SubsetList_DecisionPointList,NULL,Regime_DecisionPointList,list-method}
\alias{.newOptimalSeq,ModelObj_SubsetList_DecisionPointList,NULL,ModelObj_SubsetList_DecisionPointList,Regime_DecisionPointList,list-method}
\alias{.newOptimalSeq,ModelObj_DecisionPointList,ModelObj_SubsetList_DecisionPointList,ModelObj_SubsetList_DecisionPointList,Regime_DecisionPointList,list-method}
\alias{.newOptimalSeq,ModelObj_DecisionPointList,ModelObj_SubsetList_DecisionPointList,NULL,Regime_DecisionPointList,list-method}
\alias{.newOptimalSeq,ModelObj_DecisionPointList,NULL,ModelObj_SubsetList_DecisionPointList,Regime_DecisionPointList,list-method}

\title{ Value Search Method for Optimal Treatment Regime.}

\description{
Value search methods to estimate the optimal treatment. Method not exported.
}

\usage{
.newOptimalSeq(moPropen, moMain, moCont, regimes, fSet, ...)
\S4method{.newOptimalSeq}{modelObj,NULL,NULL,Regime,NULL}(moPropen, moMain, moCont, regimes, fSet, data, response, txName, 
               suppress, argsList)
\S4method{.newOptimalSeq}{modelObj,NULL,NULL,Regime,function}(moPropen, moMain, moCont, regimes, fSet, data, response, txName, 
               suppress, argsList)
\S4method{.newOptimalSeq}{ModelObj_SubsetList,NULL,NULL,Regime,function}(moPropen, moMain, moCont, regimes, fSet, data, response, txName, 
               suppress, argsList)
\S4method{.newOptimalSeq}{modelObj,modelObj,modelObj,Regime,NULL}(moPropen, moMain, moCont, regimes, fSet, iter, data, response, 
               txName, suppress, argsList)
\S4method{.newOptimalSeq}{modelObj,modelObj,NULL,Regime,NULL}(moPropen, moMain, moCont, regimes, fSet, iter, data, response,  
               txName, suppress, argsList)
\S4method{.newOptimalSeq}{modelObj,NULL,modelObj,Regime,NULL}(moPropen, moMain, moCont, regimes, fSet, iter, data, response,  
               txName, suppress, argsList)
\S4method{.newOptimalSeq}{modelObj,modelObj,modelObj,Regime,function}(moPropen, moMain, moCont, regimes, fSet, iter, data, response,  
               txName, suppress, argsList)
\S4method{.newOptimalSeq}{modelObj,modelObj,NULL,Regime,function}(moPropen, moMain, moCont, regimes, fSet, iter, data, response,  
               txName, suppress, argsList)
\S4method{.newOptimalSeq}{modelObj,NULL,modelObj,Regime,function}(moPropen, moMain, moCont, regimes, fSet, iter, data, response,  
               txName, suppress, argsList)
\S4method{.newOptimalSeq}{ModelObj_SubsetList,modelObj,modelObj,Regime,function}(moPropen, moMain, moCont, regimes, fSet, iter, data, response,  
               txName, suppress, argsList)
\S4method{.newOptimalSeq}{ModelObj_SubsetList,modelObj,NULL,Regime,function}(moPropen, moMain, moCont, regimes, fSet, iter, data, response,  
               txName, suppress, argsList)
\S4method{.newOptimalSeq}{ModelObj_SubsetList,NULL,modelObj,Regime,function}(moPropen, moMain, moCont, regimes, fSet, iter, data, response,  
               txName, suppress, argsList)
\S4method{.newOptimalSeq}{ModelObj_SubsetList,ModelObj_SubsetList,ModelObj_SubsetList,Regime,function}(moPropen, moMain, moCont, regimes, fSet, iter, data, response,  
               txName, suppress, argsList)
\S4method{.newOptimalSeq}{ModelObj_SubsetList,ModelObj_SubsetList,NULL,Regime,function}(moPropen, moMain, moCont, regimes, fSet, iter, data, response,  
               txName, suppress, argsList)
\S4method{.newOptimalSeq}{ModelObj_SubsetList,NULL,ModelObj_SubsetList,Regime,function}(moPropen, moMain, moCont, regimes, fSet, iter, data, response,  
               txName, suppress, argsList)
\S4method{.newOptimalSeq}{modelObj,ModelObj_SubsetList,ModelObj_SubsetList,Regime,function}(moPropen, moMain, moCont, regimes, fSet, iter, data, response,  
               txName, suppress, argsList)
\S4method{.newOptimalSeq}{modelObj,ModelObj_SubsetList,NULL,Regime,function}(moPropen, moMain, moCont, regimes, fSet, iter, data, response,  
               txName, suppress, argsList)
\S4method{.newOptimalSeq}{modelObj,NULL,ModelObj_SubsetList,Regime,function}(moPropen, moMain, moCont, regimes, fSet, iter, data, response,  
               txName, suppress, argsList)
\S4method{.newOptimalSeq}{ModelObj_DecisionPointList,NULL,NULL,Regime_DecisionPointList,list}(moPropen, moMain, moCont, regimes, fSet, data, response, txName, 
               suppress, argsList)
\S4method{.newOptimalSeq}{ModelObj_DecisionPointList,NULL,NULL,Regime_DecisionPointList,NULL}(moPropen, moMain, moCont, regimes, fSet, data, response, txName, 
               suppress, argsList)
\S4method{.newOptimalSeq}{ModelObj_SubsetList_DecisionPointList,NULL,NULL,Regime_DecisionPointList,list}(moPropen, moMain, moCont, regimes, fSet, data, response, 
               txName, suppress, argsList)
\S4method{.newOptimalSeq}{ModelObj_DecisionPointList,ModelObj_DecisionPointList,ModelObj_DecisionPointList,Regime_DecisionPointList,NULL}(moPropen, moMain, moCont, regimes, fSet, iter, data, response,  
               txName, suppress, argsList)
\S4method{.newOptimalSeq}{ModelObj_DecisionPointList,ModelObj_DecisionPointList,NULL,Regime_DecisionPointList,NULL}(moPropen, moMain, moCont, regimes, fSet, iter, data, response,  
               txName, suppress, argsList)
\S4method{.newOptimalSeq}{ModelObj_DecisionPointList,NULL,ModelObj_DecisionPointList,Regime_DecisionPointList,NULL}(moPropen, moMain, moCont, regimes, fSet, iter, data, response,  
               txName, suppress, argsList)
\S4method{.newOptimalSeq}{ModelObj_DecisionPointList,ModelObj_DecisionPointList,ModelObj_DecisionPointList,Regime_DecisionPointList,list}(moPropen, moMain, moCont, regimes, fSet, iter, data, response,  
               txName, suppress, argsList)
\S4method{.newOptimalSeq}{ModelObj_DecisionPointList,ModelObj_DecisionPointList,NULL,Regime_DecisionPointList,list}(moPropen, moMain, moCont, regimes, fSet, iter, data, response,  
               txName, suppress, argsList)
\S4method{.newOptimalSeq}{ModelObj_DecisionPointList,NULL,ModelObj_DecisionPointList,Regime_DecisionPointList,list}(moPropen, moMain, moCont, regimes, fSet, iter, data, response,  
               txName, suppress, argsList)
\S4method{.newOptimalSeq}{ModelObj_SubsetList_DecisionPointList,ModelObj_DecisionPointList,ModelObj_DecisionPointList,Regime_DecisionPointList,list}(moPropen, moMain, moCont, regimes, fSet, iter, data, response,  
               txName, suppress, argsList)
\S4method{.newOptimalSeq}{ModelObj_SubsetList_DecisionPointList,ModelObj_DecisionPointList,NULL,Regime_DecisionPointList,list}(moPropen, moMain, moCont, regimes, fSet, iter, data, response,  
               txName, suppress, argsList)
\S4method{.newOptimalSeq}{ModelObj_SubsetList_DecisionPointList,NULL,ModelObj_DecisionPointList,Regime_DecisionPointList,list}(moPropen, moMain, moCont, regimes, fSet, iter, data, response,  
               txName, suppress, argsList)
\S4method{.newOptimalSeq}{ModelObj_SubsetList_DecisionPointList,ModelObj_SubsetList_DecisionPointList,ModelObj_SubsetList_DecisionPointList,Regime_DecisionPointList,list}(moPropen, moMain, moCont, regimes, fSet, iter, data, response,  
               txName, suppress, argsList)
\S4method{.newOptimalSeq}{ModelObj_SubsetList_DecisionPointList,ModelObj_SubsetList_DecisionPointList,NULL,Regime_DecisionPointList,list}(moPropen, moMain, moCont, regimes, fSet, iter, data, response,  
               txName, suppress, argsList)
\S4method{.newOptimalSeq}{ModelObj_SubsetList_DecisionPointList,NULL,ModelObj_SubsetList_DecisionPointList,Regime_DecisionPointList,list}(moPropen, moMain, moCont, regimes, fSet, iter, data, response,  
               txName, suppress, argsList)
\S4method{.newOptimalSeq}{ModelObj_DecisionPointList,ModelObj_SubsetList_DecisionPointList,ModelObj_SubsetList_DecisionPointList,Regime_DecisionPointList,list}(moPropen, moMain, moCont, regimes, fSet, iter, data, response,  
               txName, suppress, argsList)
\S4method{.newOptimalSeq}{ModelObj_DecisionPointList,ModelObj_SubsetList_DecisionPointList,NULL,Regime_DecisionPointList,list}(moPropen, moMain, moCont, regimes, fSet, iter, data, response,  
               txName, suppress, argsList)
\S4method{.newOptimalSeq}{ModelObj_DecisionPointList,NULL,ModelObj_SubsetList_DecisionPointList,Regime_DecisionPointList,list}(moPropen, moMain, moCont, regimes, fSet, iter, data, response,  
               txName, suppress, argsList)
}

\arguments{

 \item{moPropen}{\code{"modelObj"} for propensity score regression.}
 \item{moMain}{\code{"modelObj"} for main effects of outcome regression.}
 \item{moCont}{\code{"modelObj"} for contrasts of outcome regression.}
 \item{regimes}{\code{"function"} defining class of regimes}
 \item{fSet}{\code{"function"} defining feasible treatment rules}
 \item{...}{Used to pass arguments that are required but
            the class of which does not determine the
            method that is selected.}
 \item{data}{\code{"data.frame"} of covariates and treatment history.}
 \item{response}{Outcome of interest.}
 \item{txName}{\code{"character"} name of column header in \code{data}
        containing treatment variable.}
 \item{iter}{Maximum number of iterations for iterative algorithm.}
 \item{suppress}{T/F indicating if screen prints are generated.}
 \item{argsList}{\code{"list"} of additional arguments to be passed to rgenoud.}
}

\author{
  Shannon T. Holloway <sthollow@ncsu.edu>
}

\examples{

data(bmiData)

y <- -(bmiData$month12BMI - bmiData$baselineBMI) / bmiData$baselineBMI * 100

bmiData$A2 <- as.factor(bmiData$A2)

moPropen <- buildModelObj(model = ~1, 
                          solver.method = 'glm',
                          solver.args = list("family" = "binomial"),
                          predict.args = list("type" = "response"))

moMain <- buildModelObj(model = ~parentBMI+baselineBMI+month4BMI, 
                        solver.method = 'lm')
moCont <- buildModelObj(model = ~parentBMI+baselineBMI+month4BMI, 
                        solver.method = 'lm')

regime <- function(a,data){
            vec <- character(nrow(data))
            vec[data$month4BMI < a] <- "MR"
            vec[data$month4BMI >= a] <- "CD"
            return( vec )
          }

regime <- DynTxRegime:::.newRegime(object = regime)

regList <- list(Domains = matrix(c(20,40),ncol=2), 
                pop.size = 500, 
                starting.values = 30)

\dontrun{
obj <- DynTxRegime:::.newOptimalSeq(moPropen = moPropen,
                                    moMain = NULL, 
                                    moCont = NULL, 
                                    regime = regime,
                                    fSet = NULL,
                                    data = bmiData,
                                    response = y,
                                    txName = 'A2', 
                                    suppress = TRUE,
                                    argsList = regList)

is(obj)

coef(obj)
DTRstep(obj)
estimator(obj)
fitObject(obj)
optTx(obj)
optTx(obj,bmiData)
outcome(obj)
plot(obj)
propen(obj)
show(obj)
summary(obj)
}
}

\keyword{internal}
