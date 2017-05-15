\name{moPropen}
\alias{moPropen}

\title{
Defining the moPropen input variable
}

\description{
Several of the statistical methods implemented in package
\pkg{DynTxRegime} use propensity score modeling. 
This section details how this
input is to be defined.
}

\details{

For input \code{moPropen}, the method specified to obtain predictions 
MUST return the prediction on the scale of the probability,
i.e., predictions must be in the range (0,1). In 
addition, \code{moPropen} differs from standard \code{"modelObj"} 
objects in that an additional element may be required in 
\code{predict.args}. Recall, \code{predict.args} is the list of control 
parameters passed to the prediction method. An additional control 
parameter, \code{propen.missing} can be included. \code{propen.missing} 
takes value "smallest" or "largest". It will be required if the 
prediction method returns predictions for only a subset of the 
treatment data; e.g., predict.glm(). \code{propen.missing} indicates if 
it is the smallest or the largest treatment value that is missing
from the returned predictions.

For example, fitting a binary treatment (A in \{0,1\}) using
\preformatted{
  moPropen <- buildModelObj(model = ~1,
                            solver.method = 'glm',
                            solver.args = list('family'='binomial'),
                            predict.method = 'predict.glm',
                            predict.args = list(type='response'))
}
returns only P(A=1). P(A=0) is "missing," and thus
\preformatted{
  moPropen <- buildModelObj(model = ~1,
                            solver.method = 'glm',
                            solver.args = list('family'='binomial'),
                            predict.method = 'predict.glm',
                            predict.args = list(type='response',
                                                propen.missing = 'smallest'))
}
If the dimension of the value returned by the prediction method is 
less than the number of treatment options and no value is provided 
in \code{propen.missing}, it is assumed that the smallest valued treatment
option is missing. Here, 'smallest' indicates the lowest value 
integer if treatment is an integer, or the 'base' level if treatment 
is a factor.
}


