# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                        TxInfoBasic CLASS                         + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

#----------------------------------------------------------------------#
# treatment name and superset of treatment options                     #
#----------------------------------------------------------------------#
# slots:
#
#   superSet  : a vector of all possible tx options
#   txName    : column header of data.frame that contains tx variable
#
#----------------------------------------------------------------------#
.checkValidity_TxInfoBasic <- function(object) {

  errors <- character()

  #------------------------------------------------------------------#
  # The treatment variable must be identified.                       #
  #------------------------------------------------------------------#
  if( length(object@txName) == 0L ) {
    msg <- "txName must be specified"
    errors <- c(errors, msg)
  }

  if( length(object@superSet) == 0L ) {
    msg <- "superSet must be specified"
    errors <- c(errors, msg)
  }

  if( length(errors) == 0L ) {
    return(TRUE)
  } else {
    return(errors)
  }
}

setClass(Class = "TxInfoBasic",
         slots = c(superSet = "ANY",
                   txName   = "character"),
         contains = c("VIRTUAL"),
         validity = .checkValidity_TxInfoBasic )


# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                       TxInfoBasic GENERICS                       + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

setGeneric(name = ".getSuperSet", 
           def = function(object){standardGeneric(".getSuperSet")})

setGeneric(name = ".getTxName", 
           def = function(object){standardGeneric(".getTxName")})
          
setGeneric(name = ".validTx", 
           def = function(object, txVec){standardGeneric(".validTx")})

# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                       TxInfoFactor METHODS                       + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

#----------------------------------------------------------------------#
# Retrieve the vector of all treatments available                      #
#----------------------------------------------------------------------#
# input arguments                                                      #
#   object : an object of class TxInfoBasic                            #
# returns                                                              #
#   A vector. Returns the value stored in slot superSet.               #
#----------------------------------------------------------------------#
setMethod(f = ".getSuperSet", 
          signature = c(object = "TxInfoBasic"), 
          definition = function(object) { return( object@superSet ) })
          
#----------------------------------------------------------------------#
# Retrieve the treatment variable name used in original data.          #
#----------------------------------------------------------------------#
# input arguments                                                      #
#   object : an object of class TxInfoBasic                            #
# returns                                                              #
#   A string. Returns the value stored in slot txName.                 #
#----------------------------------------------------------------------#
setMethod(f = ".getTxName", 
          signature = c(object = "TxInfoBasic"),
          definition = function(object) { 
                         return( object@txName ) 
                       } )

#----------------------------------------------------------------------#
# Given a tx vector, ensure that all elements are in superSet.         #
#----------------------------------------------------------------------#
# input arguments                                                      #
#  object : an object of class TxInfoBasic                             #
#  txVec  : vector of treatments                                       #
# returns                                                              #
#   Return NULL                                                        #
#----------------------------------------------------------------------#
setMethod(f = ".validTx",
          signature = c(object = "TxInfoBasic",
                        txVec = "ANY"),
          definition = function(object, txVec) {

                         tst <- txVec %in% object@superSet

                         if( !all(tst) ) {
                           UserError("input",
                                     "tx value not allowed by fSet")
                         }

                         return( NULL )

                       } )
