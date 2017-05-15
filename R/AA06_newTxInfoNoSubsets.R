setGeneric(name = ".newTxInfoNoSubsets", 
           def = function(txName, txVec, ...){standardGeneric(".newTxInfoNoSubsets")})

setMethod(f = ".newTxInfoNoSubsets",
          signature = c(txName = "character",
                        txVec = "factor"),
          definition = function(txName, txVec, suppress) {

                         levs <- levels(txVec)

                         if( !suppress ) {
                           cat("All patients have same feasible",
                               "treatment set {", levs, "}.\n")
                         }

                         result <- new("TxInfoFactorNoSubsets",
                                       "txName"   = txName,
                                       "superSet" = levs)

                         return( result )

                       } )

setMethod(f = ".newTxInfoNoSubsets",
          signature = c(txName = "character",
                        txVec = "integer"),
          definition = function(txName, txVec, suppress) {

                         levs <- unique(txVec)
                         levs <- sort(levs[!is.na(levs)])

                         if( !suppress ) {
                           cat("All patients have same feasible",
                               "treatment set {", levs, "}.\n")
                         }

                         result <- new("TxInfoIntegerNoSubsets",
                                       "txName"   = txName,
                                       "superSet" = levs)

                         return( result )

                       } )

