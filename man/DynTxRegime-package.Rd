\name{DynTxRegime-package}
\alias{DynTxRegime-package}
\docType{package}
\title{
Methods for Estimating Optimal Dynamic Treatment Regimes
}
\description{
Implementations of Interactive Q-Learning, Q-Learning,
value-search methods based on augmented
inverse probability weighted estimators and inverse
probability weighted estimators, outcome weighted learning (OWL),
residual weighted learning (RWL), backward outcome weighted learning (BOWL),
and efficient augmentation and relaxation learning (EARL). 
}
\details{
\tabular{ll}{
Package: \tab DynTxRegime\cr
Type: \tab Package\cr
Version: \tab 3.01\cr
Date: \tab 2017-05-21\cr
License: \tab GPL-2\cr
Depends:  \tab methods, modelObj, stats\cr
Suggests:  \tab MASS, rpart, nnet\cr
Imports: \tab  kernlab, rgenoud, dfoptim
}
See the references below for details of each method implemented.
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
Interactive model building for Q-learning. 
Biometrika, 101, 831--847.

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

Zhao, Y-Q., Laber, E. B., Saha, S., and Sands, B. E.
(2016+).
Efficient Augmentation and Relaxation Learning for Treatment
Regimes Using Observational Data.
in press.

Zhou, X., Mayer-Hamblett, N., Kham, U., and Kosorok, M. R.
(2016+).
Residual Weighted Learning for Estimating Individualized Treatment Rules.
Journal of the American Statistical Association, in press.

Zhao, Y-Q., Zeng, D., Rush, A. J., and Kosrok, M. R.
(2012).
Estimating Individualized Treatment Rules Using Outcome Weighted Learning.
Journal of the American Statistical Association, 107, 1106--1118.

Zhao, Y-Q., Zeng, D., Laber, E. B., and Kosorok, M. R.
(2015).
New Statistical Learning Methods for Estimating Optimal Dynamic Treatment Regimes.
Journal of the American Statistical Association, 110, 583--598.

}

\seealso{
 \code{\link{bowl}},
 \code{\link{earl}},
 \code{\link{iqLearnFSC}}, 
 \code{\link{iqLearnFSM}}, 
 \code{\link{iqLearnFSV}}, 
 \code{\link{iqLearnSS}}, 
 \code{\link{optimalClass}}, 
 \code{\link{optimalSeq}}, 
 \code{\link{owl}},
 \code{\link{qLearn}}, and
 \code{\link{rwl}},
}

