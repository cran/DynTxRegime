\name{fitObject}
\alias{fitObject}
\alias{fitObject,DynTxRegime-method}

\title{ Modeling Function Value Objects}

\description{
Retrieve the value objects returned by the modeling function
used for the regression analysis.
Extends methods defined in \pkg{modelObj}.
}
\usage{ \S4method{fitObject}{DynTxRegime}(object, ...)  }

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

For IQ-Learning, only outcome regression models are used. These models
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



For Q-Learning, only outcome regression models are used. 
If regression models are defined using objects of class modelObj, 
the returned list will follow the pattern described for IQ-Learning.
If regression models are defined using objects of class modelObjSubset,
a list is returned. The kth element of that list
corresponds to the kth subset model and is named accordingly. 
Each subset model follows the pattern for modelObj described above. 

For optimalSeq, a list is returned,
the elements of which are named "regimes," "propen," and "outcome." 
Element "regimes" will contain the parameter estimates for the treatment
regimes. Element "propen" will contain the parameter estimates for each
of the propensity for treatment models. And, "outcome" will contain
the parameter estimates for each of the outcome regression models.

For optimalClass, a list is returned,
the elements of which are named "classif," "propen," and "outcome." 
Element "propen" will contain the parameter estimates for 
the propensity for treatment model. And, "outcome" will contain
the parameter estimates for the outcome regression models.
And, "classif" will contain the value object returned by the classification
method.

}

\value{
       Returns a list of the value objects
returned by the regression method(s). See the details section for more information.
}

\author{
  Shannon T. Holloway <sthollow@ncsu.edu>
}


