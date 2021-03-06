% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/B_TxInfoBasic.R
\name{TxInfoBasic-methods}
\alias{TxInfoBasic-methods}
\alias{.compareTx,TxInfoBasic,ANY,ANY-method}
\alias{.convertFromBinary,TxInfoBasic-method}
\alias{.convertToBinary,TxInfoBasic-method}
\alias{.convertTx,TxInfoBasic-method}
\alias{.getLevels,TxInfoBasic,ANY-method}
\alias{.getSuperset,TxInfoBasic-method}
\alias{.getTxName,TxInfoBasic-method}
\alias{.validTx,TxInfoBasic-method}
\title{Methods Available for Objects of Class \code{TxInfoBasic}}
\usage{
\S4method{.compareTx}{TxInfoBasic,ANY,ANY}(object, vec1, vec2)

\S4method{.convertFromBinary}{TxInfoBasic}(txObj, txVec, ...)

\S4method{.convertToBinary}{TxInfoBasic}(txObj, ..., txVec)

\S4method{.convertTx}{TxInfoBasic}(object, txVec)

\S4method{.getLevels}{TxInfoBasic,ANY}(object, txVec)

\S4method{.getSuperset}{TxInfoBasic}(object)

\S4method{.getTxName}{TxInfoBasic}(object)

\S4method{.validTx}{TxInfoBasic}(object, txVec)
}
\description{
Methods Available for Objects of Class \code{TxInfoBasic}

\code{.compareTx(object, vec1, vec2)}
  not allowed

\code{.convertFromBinary(txObj, txVec)}
  converts a -1/1 Tx to user provided tx coding

\code{.convertToBinary(txObj, data)}
  converts user specified tx variable to binary -1/1

\code{.convertTx(object, txVec)}
  not allowed

\code{.getLevels(object, txVec)}
  not allowed

\code{.getSuperset(object)}
  retrieves superset information

\code{.getTxName(object)}
  retrieve tx variable name

\code{.validTx(object, txVec)}
  ensures all elements in txVec are allowed by superset
}
\keyword{internal}
