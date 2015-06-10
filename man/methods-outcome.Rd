\name{outcome}
\alias{outcome}
\alias{outcome,DynTxRegime-method}

\title{ Retrieve Regression Objects for the Outcome Regression Models}

\description{
Retrieve the value objects returned by the regression methods used to
obtain parameter estimates for the outcome regression.
}
\usage{ \S4method{outcome}{DynTxRegime}(object, ...)  }

\arguments{
  \item{object }{an object of class DynTxRegime.}

  \item{\dots }{ignored.}

}

\details{
There is a pattern to the format for the returned values of 
all standard regression tools that have been extended to the
DynTxRegime class. A list is always returned. The length and
structure of the list are determined by the 
statistical method used to estimate the optimal dynamic treatment regime
and by the combination of the input arguments.

For IQ-Learning and optimalClass, the models
are always communicated using objects of class modelObj. The pattern for
the returned object follows:

if( iter == 0 && class(moMain) == "modelObj" && class(moCont) == "modelObj")

names(coef()) = "Combined"

if( iter > 0 && class(moMain) == "modelObj" && class(moCont) == "modelObj")

names(coef()) = "MainEffect", "Contrast"

if( iter == 0 && class(moMain) == "modelObj" && class(moCont) == "NULL")

names(coef()) = "moMain"

if( iter == 0 && class(moMain) == "NULL" && class(moCont) == "modelObj")

names(coef()) = "moCont"



For Q-Learning, models can be defined using modelObj
or a list of objects of class modelObjSubset.
If regression models are defined using objects of class modelObj, 
the returned list will follow the pattern described for IQ-Learning.
If regression models are defined using objects of class modelObjSubset,
a list is returned. The kth element of that list
corresponds to the kth subset model and is named accordingly. 
Each subset model follows the pattern for modelObj described above. 

For optimalSeq, models can be defined using modelObj
lists of objects of class modelObj, or lists of objects of
class modelObjSubset.
If regression models are defined using objects of class modelObj, 
the returned list will follow the pattern described for IQ-Learning.
If regression models are defined using lists, the kth element of the
returned list corresponds to the kth model. 
Each model follows the pattern for modelObj described above. 

}

\value{
Retrieves the value object returned by the regression method
used to estimate the outcome. 
See details for more information.
}

\author{
  Shannon T. Holloway <sthollow@ncsu.edu>
}


