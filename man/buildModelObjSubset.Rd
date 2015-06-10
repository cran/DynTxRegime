\name{buildModelObjSubset}
\alias{buildModelObjSubset}

\title{
Create Model Objects for Subsets of Data.
}

\description{
This function extends the buildModelObj() function of package modelObj. Here,
the returned model object includes a specification of the decision point and a
subset of the data to which the model is to be applied.
}
\usage{ 
buildModelObjSubset(..., model, dp = 1L, subset, 
                    solver.method, solver.args = NULL, 
                    predict.method = NULL, predict.args = NULL)}
\arguments{

\item{...           }{Ignored.}

\item{model         }{an object of class \code{"formula"}: 
                      the symbolic description of the model to be fitted.}
 
\item{dp            }{an object of class \code{"integer"}:
                      the decision point for which this model and subset are
                      defined.}
 
\item{subset        }{an object of class \code{"character"}: 
                      a nickname for the subset for which \code{model} is to 
                      be used. This argument will be used by the methods
                      of DynTxRegime to ``link" input arguments.}

\item{solver.method }{an object of class \code{"character"}:
                      the name of the R function to be used to obtain 
                      parameter estimates, e.g.,  
                      \code{lm}, \code{glm}, or \code{rpart}. 
                      The specified function MUST
                      have a corresponding predict method.}
 
\item{solver.args   }{an object of class \code{"list"}:
                      additional arguments to be sent to \code{solver.method}. 
                      This argument must be provided as a named list, 
                      where the name of each element matches a formal argument
                      of solver.method. For example, if a logistic regression 
                      using glm is desired, 
\preformatted{
solver.method = "glm"
solver.args = list("family"=binomial)
}
                      It is assumed that the R method  
                      specified in \code{solver.method} has
                      formal arguments ``formula" and ``data."
                      If \code{solver.method} does not use 
                      ``formula" for the model or ``data" as the  
                      data.frame of covariates, 
                      \code{solver.args} must explicitly indicate
                      the variable names used for these inputs.
                      For example, list(``x"=``formula") if the model
                      is passed to \code{solver.method}
                      through input argument
                      ``x" or list(``df"=``data") if the data.frame of
                      covariates is passed to \code{solver.method} through 
                      input argument ``df." }
 
\item{predict.method }{an object of class \code{"character"}:
                       the name of the R function to be used to obtain 
                       predictions, e.g., \code{predict.lm}, \code{predict}, 
                       or \code{predict.glm}. 
                       If no function is explicitly 
                       given, the generic \code{predict} is assumed.
                       For many regression methods, the generic \code{predict} 
                       method is appropriate. }
 
\item{predict.args  }{an object of class \code{"list"}:
                      additional arguments to be sent to 
                      \code{predict.method}. This argument must be provided 
                      as a named list, where the name of each element 
                      matches a formal argument of \code{predict.method}. 
                      For example, if a logistic regression using \code{glm}  
                      was used to fit the model and predictions on the scale of 
                      the response are desired, 
\preformatted{
predict.method = "predict.glm"
predict.args = list("type"="response").
}

                      It is assumed that 
                      \code{predict.method} has formal arguments
                      ``object" and ``newdata." If \code{predict.method} 
                      does not use these 
                      formal arguments, \code{predict.args} must explicitly 
                      indicate the argument names used for these inputs. 
                      For example, list(``x"=``object") if the object 
                      returned by \code{solver.method} is passed to 
                      \code{predict.method}
                      through input argument ``x"  or list(``ndf"=``newdata") 
                      if the data.frame object is passed to 
                      \code{predict.method} 
                      through input argument ``ndf."}
 
}

\details{
In some clinical trial settings, an analyst may
want to use different models for unique subsets of the data.
\code{buildModelObjSubset} provides a mechanism for users to 
define models for such subsets. Specifically,
models are specified in connection with the decision point and subset to 
which they are to be applied. For DynTxRegime methods \code{qLearn}
and \code{optimalSeq}, a list of said objects can be passed as
input.
}

\value{
 An object of type modelObjSubset, which
  contains a complete description of the conditions under which a model 
  is to be used and the R methods to be used to obtain parameter
  estimates and predictions.
}


\examples{

    #----------------------------------------------------#
    # Consider a 2 decision point trial.                 #
    # At the first decision point, the subset of         #
    # treatment options available to each patient is     #
    # always the set "a1." At the second decision point, #
    # Some patients are eligible to received treatment   #
    # from set "a" and others from set "b." The outcome  #
    # for these subsets will be modeled as ~ x1 + x2 and #
    # ~ x2 + x3, respectively.                           #
    #                                                    #
    # All parameter estimates are to be obtained used lm #
    # and prediction obtained using predict.             #
    #                                                    #
    # The following illustrates how to build these       #
    # model objects.                                     #
    #----------------------------------------------------#

    model <- list()

    model[[1]] <- buildModelObjSubset(dp = 1,
                                      subset = "a1",
                                      model = ~ x1 + x2 + x3,
                                      solver.method = 'lm')

    model[[2]] <- buildModelObjSubset(dp = 2,
                                      subset = "a",
                                      model = ~ ~ x1 + x2,
                                      solver.method = 'lm')

    model[[3]] <- buildModelObjSubset(dp = 2,
                                      subset = "b",
                                      model = ~ x2 + x3,
                                      solver.method = 'lm')


}
