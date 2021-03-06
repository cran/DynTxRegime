% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/K_KernelObj.R
\name{.newKernelObj}
\alias{.newKernelObj}
\alias{.newKernelObj,character,formula-method}
\alias{.newKernelObj,list,list-method}
\title{Create a KernelObj}
\usage{
.newKernelObj(kernel, model, ...)

\S4method{.newKernelObj}{character,formula}(kernel, model, data, kparam = NULL, ...)

\S4method{.newKernelObj}{list,list}(kernel, model, data, kparam = NULL, ...)
}
\arguments{
\item{kernel}{A character. Name of kernel}

\item{model}{A formula or list of formula}
}
\description{
Processes input to determine type of kernel, creates it, and stores in 
  @slot kernel.
}
\keyword{internal}
