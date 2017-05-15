# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                          TxSubset CLASS                          + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

#----------------------------------------------------------------------#
# treatment information for a single decision point when subsets are   #
# identified                                                           #
#----------------------------------------------------------------------#
# slots:
#
#  ptsSubset  : a character vector. Each element corresponds to a 
#               patient as ordered by the data. The vector contains 
#               the name of the subset of which the patient is a member.
#  subsetRule : a function. The original fSet function provided by user.
#  subsets    : a named list of the tx subsets
#  singleton  : logical vector indicating if >1 tx is available
#
#----------------------------------------------------------------------#
.checkValidity_TxSubset <- function(object){

  errors <- character()

  #------------------------------------------------------------------#
  # Ensure that subsets identified for each patient are in the list  #
  # of subsets.                                                      #
  #------------------------------------------------------------------#
  ssNames <- names(object@subsets)
  tst <- object@ptsSubset %in% ssNames
  if( any(!tst) ) {
    msg <- "patient subset not found in the subset list"
    errors <- c(errors, msg)
  }

  #------------------------------------------------------------------#
  # n must be > 0                                                    #
  #------------------------------------------------------------------#
  if( length(object@singleton) != length(object@ptsSubset) ) {
    msg <- "ptsSubset/singleton not appropriately defined"
    errors <- c(errors, msg)
  }

  if( length(errors) == 0L ) {
    return(TRUE)
  } else {
    return(errors)
  }
}

setClass(Class = "TxSubset",
         slots = c(ptsSubset  = "character",
                   subsetRule = "function",
                   subsets    = "list",
                   singleton  = "logical"),
         validity = .checkValidity_TxSubset,
         contains = c("VIRTUAL") )

# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                         TxSubset GENERICS                        + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

setGeneric(name = ".getPtsSubset",  
           def = function(object){standardGeneric(".getPtsSubset")})
           
setGeneric(name = ".getSingleton", 
           def = function(object){standardGeneric(".getSingleton")})

setGeneric(name = ".getSubsetRule", 
           def = function(object){standardGeneric(".getSubsetRule")})

setGeneric(name = ".getSubsets",  
           def = function(object){standardGeneric(".getSubsets")})

# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                         TxSubset METHODS                         + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

#----------------------------------------------------------------------#
# Retrieve the subset of treatment that is available to each patient.  #
#----------------------------------------------------------------------#
# input arguments                                                      #
#   object : an object of class TxSubset                               #
# returns                                                              #
#   A character vector. Returns the value stored in slot ptsSubset.    #
#----------------------------------------------------------------------#
setMethod(f = ".getPtsSubset",
          signature = c(object = "TxSubset"), 
          definition = function(object){ return( object@ptsSubset ) } )
          
#----------------------------------------------------------------------#
# Retrieve flag indicating if patient has only 1 tx available.         #
#----------------------------------------------------------------------#
# input arguments                                                      #
#   object : an object of class TxSubset.                              #
# returns                                                              #
#   A logical vector. Returns the value stored in slot singleton.      #
#----------------------------------------------------------------------#
setMethod(f = ".getSingleton",
          signature = c(object = "TxSubset"), 
          definition = function(object) { return( object@singleton ) } )

#----------------------------------------------------------------------#
# Retrieve the function used to determine the subset of treatment that #
# is available to a patient.                                           #
#----------------------------------------------------------------------#
# input arguments                                                      #
#   object : an object of class TxSubset                               #
# returns                                                              #
#   A function or NULL. Returns the value stored in slot subsetRule.   #
#----------------------------------------------------------------------#
setMethod(f = ".getSubsetRule",
          signature = c(object = "TxSubset"), 
          definition = function(object){ return( object@subsetRule ) } )

#----------------------------------------------------------------------#
# Retrieve a list of the subsets of feasible treatment.                #
#----------------------------------------------------------------------#
# input arguments                                                      #
#   object : an object of class TxSubset                               #
# returns                                                              #
#   A list. Returns the value stored in slot subsets.                  #
#----------------------------------------------------------------------#
setMethod(f = ".getSubsets", 
          signature = c(object = "TxSubset"), 
          definition = function(object){ return( object@subsets ) } )
          
#----------------------------------------------------------------------#
# Given a tx vector, ensure that all elements are in superSet.         #
#----------------------------------------------------------------------#
# input arguments                                                      #
#   object : an object of class TxSubset.                              #
#   txVec  : vector of treatments                                      #
# returns                                                              #
#   NULL                                                               #
#----------------------------------------------------------------------#
setMethod(f = ".validTx",
          signature = c(object = "TxSubset",
                        txVec = "ANY"),
          definition = function(object, txVec) {

                         for( i in 1L:length(object@subsets) ) {
                           inss <- object@ptsSubset %in% names(object@subsets)[i]
                           tst <- txVec[inss] %in% object@subsets[[i]]
                           if( !all(tst) ) {
                             UserError("input",
                                       "tx value not allowed by fset")
                           }
                         }

                         return( NULL )

                       } )
