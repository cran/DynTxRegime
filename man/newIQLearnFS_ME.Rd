% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/E_class_IQLearnFS_ME.R
\name{.newIQLearnFS_ME}
\alias{.newIQLearnFS_ME}
\alias{.newIQLearnFS_ME,modelObj,modelObj,IQLearnSS-method}
\alias{.newIQLearnFS_ME,modelObj,NULL,IQLearnSS-method}
\alias{.newIQLearnFS_ME,NULL,modelObj,IQLearnSS-method}
\title{Complete First Stage Analysis of Main Effects for Interactive Q-Learning Algorithm}
\usage{
.newIQLearnFS_ME(moMain, moCont, response, ...)

\S4method{.newIQLearnFS_ME}{modelObj,modelObj,IQLearnSS}(moMain, moCont, data, response, txName, iter, suppress)

\S4method{.newIQLearnFS_ME}{modelObj,`NULL`,IQLearnSS}(moMain, moCont, data, response, txName, iter, suppress)

\S4method{.newIQLearnFS_ME}{`NULL`,modelObj,IQLearnSS}(moMain, moCont, data, response, txName, iter, suppress)
}
\description{
Performs regression on the fitted main effects the second stage regression.
}
\keyword{internal}
