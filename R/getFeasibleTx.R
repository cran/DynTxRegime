#----------------------------------------------------------------------#
# Determine subset of treatments available to each patient             #
#----------------------------------------------------------------------#
# fSet     : a function defining the feasibility rule                  #
#                                                                      #
# superSet : super set of tx options given as vector character/integer #
#            levels                                                    #
#                                                                      #
# data     : data.frame; covariates and treatment histories            #
#                                                                      #
# suppress : T/F indicating if prints to screen should be executed     #
#----------------------------------------------------------------------#
.getFeasibleTx <- function(fSet, 
                           superSet, 
                           data, 
                           suppress, 
                           verify = TRUE, ...) {

  if( is(fSet, "NULL") ) {
    #--------------------------------------------------------------#
    # If fSet is NULL, no treatment subsets were identified by     #
    # user. All patients have superset of treatment as treatment   #
    # option.                                                      #
    #--------------------------------------------------------------#
    if( !suppress ) {
      cat("All patients have same feasible treatment set {",
          superSet, "}.\n")
    }

    return( list("subsets"   = list("SS" = superSet),
                 "ptsSubset" = rep("SS", nrow(data))) )

  } else {
    res <- .feasibleTx(fSet = fSet, 
                       superSet = superSet, 
                       data = data,
                       suppress = suppress,
                       verify = verify)
    return( res )

  }
}

#----------------------------------------------------------------------#
# fSet     : a function defining the feasibility rule                  #
#                                                                      #
# superSet : super set of tx options given as vector character/integer #
#            levels                                                    #
#                                                                      #
# data     : data.frame; covariates and treatment histories            #
#                                                                      #
# suppress : T/F indicating if prints to screen should be executed     #
#----------------------------------------------------------------------#
.feasibleTx <- function(fSet, superSet, data, suppress, verify) {
  #------------------------------------------------------------------#
  # Determine formals indicated in user specified function.          #
  #------------------------------------------------------------------#
  fSetFormalArguments <- names(formals(fSet))

  if( length(fSetFormalArguments) == 0L || 
      is(fSetFormalArguments,"NULL")) {
    UserError("input", 
              "formal arguments of fSet could not be identified")
  }

  #------------------------------------------------------------------#
  # Identify if formals match the column names of the dataset        #
  #------------------------------------------------------------------#
  matchedColnames <- match(fSetFormalArguments, colnames(data))

  unmatched <- which(is.na(matchedColnames))

  if( length(unmatched) == 1L ) {
    #--------------------------------------------------------------#
    # Assume data.frame is the formal                              #
    #--------------------------------------------------------------#
    # General function that passes the covariates of a single      #
    # patient to fSet to obtain the list of feasible txs for the   #
    # individual                                                   #
    #--------------------------------------------------------------#
    func1 <- function(x, rule, verify) {

               msg <- "unable to execute fSet"

               txvec <- tryCatch(rule(x),
                                 condition = function(c){UserError("input", msg)},
                                 silent = TRUE)

               if(verify) txvec <- .verifyTxvec(txvec, superSet)

               return( txvec )
             }

    #--------------------------------------------------------------#
    # For each patient, call fSet to obtain feasible tx set.       #
    #--------------------------------------------------------------#
    subsets <- list()
    testFSet <- func1(x = data[1L,,drop=FALSE], 
                      rule = fSet, verify = FALSE)

    if( is(testFSet, "list") ) {
      if( is(testFSet[[1L]], "list") ) {
        tt <- fSet(data)

        for( i in 1L:length(tt$subsets) ) {
          subsets[[tt$subsets[[i]][[1L]]]] <- tt$subsets[[i]][[2L]]
        }
        ptsSubset <- tt$txOpts
      } else {
        ptsSubset <- character(nrow(data))
        for( i in 1L:nrow(data) ) {
          tt <- func1(x = data[i,,drop=FALSE], 
                      rule = fSet, verify = verify)

          subsets[[ tt[[1L]] ]] <- tt[[2L]]

          ptsSubset[i] <- tt[[1L]]
        }
      }
    }


  } else if( length(unmatched) != 0L ) {

    msg <- paste("fSet formal arguments", 
                 paste(fSetFormalArguments[unmatched], collapse = ", "), 
                 "could not be found in dataset")

    UserError("input", msg)

  } else {
    #--------------------------------------------------------------#
    # General function that passes the covariates of a single      #
    # patient to fSet to obtain the list of feasible txs for the   #
    # individual                                                   #
    #--------------------------------------------------------------#
    func2 <- function(x, fSet, verify){

               txvec <- do.call(what = fSet, args = as.list(x))

               if(verify) txvec <- .verifyTxvec(txvec, superSet)

               return( txvec )
             }

    #--------------------------------------------------------------#
    # For each patient, call fSet to obtain feasible tx set        #
    #--------------------------------------------------------------#
    subsets <- list()
    dm <- data[1L,matchedColnames,drop=FALSE]

    dmSplit <- split(data.matrix(dm),col(dm))
    names(dmSplit) <- fSetFormalArguments
    testFSet <- tryCatch(do.call(fSet, dmSplit), 
                         condition =  function(e){NA},
                         silent = TRUE)

    if( all(is.na(testFSet)) ) {
      UserError("input", "unable to execute fSet")
    } else if( is(testFSet, "list") ) {
      if( is(testFSet[[1L]], "list") ) {
        dm <- data[,matchedColnames,drop=FALSE]

        dmSplit <- split(data.matrix(dm),col(dm))
        names(dmSplit) <- fSetFormalArguments
        testFSet <- tryCatch(do.call(fSet, dmSplit), 
                             condition =  function(e){NA},
                             silent = TRUE)
        for( i in 1L:length(testFSet$subsets) ) {
          subsets[[testFSet$subsets[[i]][[1L]]]] <- testFSet$subsets[[i]][[2L]]
        }
        ptsSubset <- testFSet$txOpts
      } else {
        ptsSubset <- character(nrow(data))
        dm <- data[,matchedColnames,drop=FALSE]
        for( i in 1L:nrow(data) ) {
          tt <- func2(x = dm[i,,drop=FALSE], 
                      fSet = fSet, verify = verify)

          subsets[[ tt[[1L]] ]] <- tt[[2L]]

          ptsSubset[i] <- tt[[1L]]
        }
      }
    }
  }

  if( verify ) {
    ssOpts <- NULL
    for( i in 1L:length(subsets) ) {
      if( !all(subsets[[i]] %in% superSet) ) {
        stop("subset has treatments not in data")
      }
      ssOpts <- c(ssOpts, subsets[[i]])
    }
    if( any(!{superSet %in% ssOpts}) ) {
      stop("data has treatments not in subsets")
    }
  }

  if( !suppress ) {
    cat("\nSubsets of treatment identified as:\n")
    print(subsets)
    counts <- sapply(names(subsets), function(x){sum(x == ptsSubset)})
    names(counts) <- names(subsets)
    cat("Number of patients in data for each subset:\n")
    print(counts)
  }

  return( list("subsets"   = subsets,
               "ptsSubset" = ptsSubset) )

}

#----------------------------------------------------------------------#
# txvec  : object returned by a call to user defined fSet.             #
#                                                                      #
# levelsInData : character vector indicating the superset of           #
#                treatments in data.                                   #
#                                                                      #
#  Returns a list object of the verified and appropriately typed result#
#  of a call to fSet. The first element must be a character object     #
#  indicating the nickname of the subset. The second element will be   #
#  a character/integer representation of the subset.                   #
#----------------------------------------------------------------------#
.verifyTxvec <- function(txvec, levelsInData) {

  #------------------------------------------------------------------#
  # fSet must return a list object. The first element of which is a  #
  # nickname for the subset. The second element is a vector of       #
  # treatment options in that subset.                                #
  #------------------------------------------------------------------#
  if( is.list(txvec) ) {
    #--------------------------------------------------------------#
    # There can only be two elements in the list.                  #
    #--------------------------------------------------------------#
    if( length(txvec) != 2L ) {
      UserError("input", "fSet must return a list of length 2")
    }

    #--------------------------------------------------------------#
    # The first element must be a character description of subset  #
    #--------------------------------------------------------------#
    if( is.character(txvec[[ 1L ]]) ) {
      #----------------------------------------------------------#
      # second must be a vector of treatment options available   #
      #----------------------------------------------------------#
      if( length(txvec[[ 2L ]]) == 0L ) {
        msg <- paste("at least one tx option must be available ",
                     "to every patient --- verify fSet", sep="")
        UserError("input", msg)
      }

      if( class(txvec[[2L]]) == "factor" ) {
        #------------------------------------------------------#
        # If provided as a factor vector, convert to character #
        #------------------------------------------------------#
        txvec[[2L]] <- levels(txvec[[2L]])[txvec[[2L]]]
      } else if( class(txvec[[2L]]) == "numeric" ) {
        #------------------------------------------------------#
        # If provided as a numeric vector, round and convert to#
        # to integer.                                          #
        #------------------------------------------------------#
        txvec[[2L]] <- as.integer(round(txvec[[2L]],0L))
      } else if( class(txvec[[2L]]) != "factor" && 
                 class(txvec[[2L]]) != "integer" &&
                 class(txvec[[2L]]) != "character" ) {
        msg <- "fSet defined treatment must be factor/character or integer"
        UserError("input", msg)
      }

    } else {
      msg <- "first element of list returned by fSet must be a character"
      UserError("input", msg)
    }

  } else {
    UserError("input", "fSet must return a list")
  }

  return(txvec)

}
