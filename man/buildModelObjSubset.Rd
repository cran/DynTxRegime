\name{buildModelObjSubset}
\alias{buildModelObjSubset}

\title{
Create Model Objects for Subsets of Data.
}

\description{
Extends the buildModelObj() function of package \pkg{modelObj}. Here,
the returned model object includes a specification of the 
decision point and subset of the data to which the model is to 
be applied.
}
\usage{ 
buildModelObjSubset(..., model, 
                    solver.method, solver.args = NULL, 
                    predict.method = NULL, predict.args = NULL,
                    dp = 1L, subset = NA)}
\arguments{

\item{\dots         }{ignored. Included to require named input.}

\item{model         }{An object of class \code{"formula."} 
                      The symbolic description of the model to be fitted.
                      If the regression method specified in \code{solver.method}
                      accepts as input a \code{"formula"} object, \code{model} 
                      is passed to the solver.method. If the regression method
                      instead accepts a matrix of covariates as the
                      model to fit, \code{model} is used to obtain
                      the model matrix that is passed to
                      solver.method. }
 
\item{solver.method }{An object of class \code{"character."}
                      The name of the R function to be used to obtain 
                      parameter estimates, e.g.,  
                      \code{lm}, \code{glm}, or \code{rpart}. 
                      The specified function MUST
                      have a corresponding predict method,
                      which can be the generic predict() function.}
 
\item{solver.args   }{An object of class \code{"list."}
                      Additional arguments to be sent to the
                      function specified in \code{solver.method}.
                      This argument must be provided as a named list, 
                      where the name of each element matches a 
                      formal argument of the function specified in
                      \code{solver.method}. For example, if a logistic 
                      regression using \code{glm} is desired, 
\preformatted{
solver.method = "glm"
solver.args = list("family"=binomial)
}
                      See Details section for further information.}
 
\item{predict.method }{An object of class \code{"character."}
                       The name of the R function to be used to obtain 
                       predictions, e.g., \code{predict.lm}, \code{predict}, 
                       or \code{predict.glm}. 
                       If no function is explicitly 
                       given, the generic \code{predict()} is assumed.
                       For many regression methods, the generic \code{predict()} 
                       method is appropriate. }
 
\item{predict.args  }{An object of class \code{"list."}
                      Additional arguments to be sent to the function
                      specified in 
                      \code{predict.method}. This argument must be provided 
                      as a named list, where the name of each element 
                      matches a formal argument of the function
                      specified in \code{predict.method}. 
                      For example, if a logistic regression using \code{glm}  
                      was used to fit the model and predictions on the scale of 
                      the response are desired, 
\preformatted{
predict.method = "predict.glm"
predict.args = list("type"="response").
}

                      See Details section for further information.}
 
\item{dp            }{An object of class \code{"integer."}
                      The decision point for which this model and subset are
                      defined.}
 
\item{subset        }{An object of class \code{"character."} 
                      A nickname for the subset for which model and
                      methods are to 
                      be used. This argument will be used by the methods
                      of \pkg{DynTxRegime} to "link" input arguments.
                      In the event that a model is to be fit using
                      more than 1 subset, collapse the subset names
                      into a single character string separating each
                      with a comma. For example, if the model is to
                      be fit using patients in both subsets "a" and "b,"
                      the subset nickname should be "a,b" (no space).}

}

\details{
In some settings, an analyst may
want to use different models for unique subsets of the data.
\code{buildModelObjSubset()} provides a mechanism for users to 
define models for such subset. Specifically,
models are specified in connection with the decision point and subset to 
which they are to be applied. 

It is assumed that if the method specified in \code{solver.method}
uses a formula then \code{formula} and \code{data} are the formal arguments for the
\code{"formula"} object and \code{"data.frame,"} respectively. However, 
if solver.method
does not use formal argument \code{formula} and/or \code{data}, the
appropriate names can be provided through \code{solver.args}. 
For example, the input list for \code{solver.args} would
include the element "x"="formula" if the \code{"formula"} object
is passed through input argument \code{x} or it would include element
"df"="data" if the \code{"data.frame"} object is passed through 
input argument \code{df}.

If  the R method specified in \code{solver.method} instead uses a model matrix
to define the model, it is assumed to have formal arguments \code{x} and \code{y}.
If the solver.method does not use \code{x} for the model matrix and/or 
\code{y} as the response, \code{solver.args} must explicitly indicate
the variable names used for these inputs.
For example, the list passed through \code{solver.args} would include
element "X"="x" if the model matrix is passed through input argument
\code{X} or element "Y"="y" if the response is passed through input 
argument \code{Y}.

It is also assumed that the function specified in \code{predict.method}
has formal arguments \code{object} and \code{newdata} through which
the value returned by the solver.method and the \code{"data.frame"} objects
are provided. If the predict.method does not use these 
formal arguments, \code{predict.args} must explicitly indicate the 
argument names used for these inputs. For example, 
the names list passed through \code{predict.args} would include element "x"="object"
if the object returned by the function specified in \code{solver.method} is passed 
through input argument \code{x}  or it would include element "ndf"="newdata"
if the \code{"data.frame"} object is passed through input argument \code{ndf}.
}

\value{
 An object of class \code{"ModelObjSubset,"} which
  contains a complete description of the conditions under which a model 
  is to be used and the R methods to be used to obtain parameter
  estimates and predictions.
}


\examples{

# Consider a 2 decision point trial. At the 1st decision point, the subset of 
# treatment options available to each patient is always set "set1."
# At the 2nd decision point, some patients are eligible to receive
# treatment from set "set2a" and others from set "set2b." The outcome
# for these subsets will be modeled as ~ x1 + x2 and ~ x2 + x3, respectively.
#
# All parameter estimates are to be obtained used lm and predictions obtained using predict.
#
# The following illustrates how to build these model objects.

  model <- list()

  model[[1]] <- buildModelObjSubset(dp = 1, subset = "set1",
                                    model = ~ x1 + x2 + x3, solver.method = 'lm')

  model[[2]] <- buildModelObjSubset(dp = 2, subset = "set2a",
                                    model = ~ ~ x1 + x2, solver.method = 'lm')

  model[[3]] <- buildModelObjSubset(dp = 2, subset = "set2b",
                                    model = ~ x2 + x3, solver.method = 'lm')


}
