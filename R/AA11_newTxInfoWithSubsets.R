setGeneric(name = ".newTxInfoWithSubsets", 
           def = function(txName, txVec, ...){
                   standardGeneric(".newTxInfoWithSubsets")
                 })

.newTxInfoFactorWithSubsets <- function(txName, 
                                        txVec, 
                                        fSet, 
                                        subsets, 
                                        ptsSubset) {


  singleton <- logical(length(ptsSubset))
  for( i in 1L:length(subsets) ) {
    if( length(subsets[[i]]) == 1L ) {
      tst <- ptsSubset == names(subsets)[i]
      singleton[tst] <- TRUE
    }
  }

  result <- new("TxInfoFactorWithSubsets",
                "subsets"    = subsets,
                "ptsSubset"  = ptsSubset,
                "subsetRule" = fSet,
                "singleton"  = singleton,
                "txName" = txName,
                "superSet" = levels(txVec))

  return(result)
}

setMethod(f = ".newTxInfoWithSubsets",   
          signature = c(txName = "character",
                        txVec = "factor"), 
          definition = .newTxInfoFactorWithSubsets)

.newTxInfoIntegerWithSubsets <- function(txName, 
                                         txVec, 
                                         fSet, 
                                         subsets, 
                                         ptsSubset) {

  singleton <- logical(length(ptsSubset))
  for( i in 1L:length(subsets) ) {
    if( length(subsets[[i]]) == 1L ) {
      tst <- ptsSubset == names(subsets)[i]
      singleton[tst] <- TRUE
    }
  }

  levs <- unique(txVec)
  levs <- sort(levs[!is.na(levs)])

  result <- new("TxInfoIntegerWithSubsets",
                "subsets"    = subsets,
                "ptsSubset"  = ptsSubset,
                "subsetRule" = fSet,
                "singleton"  = singleton,
                "txName" = txName,
                "superSet" = levs)

  return(result)
}

setMethod(f = ".newTxInfoWithSubsets",   
          signature = c(txName = "character",
                        txVec = "integer"), 
          definition = .newTxInfoIntegerWithSubsets)

