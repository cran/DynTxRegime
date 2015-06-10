\name{DynTxRegime-package}
\alias{DynTxRegime-package}
\alias{DynTxRegime}
\docType{package}
\title{
Methods for Estimating Dynamic Treatment Regimes
}

\description{
Implementations of Interactive Q-Learning, Q-Learning,
and value-search methods based on augmented
inverse probability weighted estimators and inverse
probability weighted estimators. 
}
\details{
\tabular{ll}{
Package: \tab DynTxRegime\cr
Type: \tab Package\cr
Version: \tab 2.1\cr
Date: \tab 2015-06-10\cr
License: \tab GPL-2\cr
Depends: \tab methods, modelObj, rgenoud\cr
}
Please see the references below for details of each method implemented.
}
\author{
Marie Davidian, Eric B. Laber, Kristin A. Linn, 
Leonard A. Stefanski, Anastasios A. Tsiatis, Baqun Zhang, Min Zhang, 
and Shannon T. Holloway \cr
Maintainer: Shannon T. Holloway <sthollow@ncsu.edu>
}
\references{
Laber, E. B., Linn, K. A., and Stefanski, L. A. 
(2014). 
Interactive Q-learning. 
Biometrika, in press.

Zhang, B., Tsiatis, A. A., Davidian, M., Zhang, M., and Laber, E. B.
(2012).
Estimating Optimal Treatment Regimes from a Classification Perspective.
Stat, 1, 103--114

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
\keyword{ package }
\seealso{
 \code{\link{iqLearnSS}}, 
 \code{\link{iqLearnFSM}}, 
 \code{\link{iqLearnFSC}}, 
 \code{\link{iqLearnFSV}}, 
 \code{\link{optimalSeq}}, 
 \code{\link{optimalClass}}, 
 \code{\link{qLearn}}
}

