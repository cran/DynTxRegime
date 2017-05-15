# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                        TxInfoFactor CLASS                        + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

#----------------------------------------------------------------------#
# treatment name and superset of treatment options when treatment      #
# is a factor                                                          #
#----------------------------------------------------------------------#
# extends TxInfoNoSubsets directly
# extends TxInfoBasic directly
# extends SingleDecisionPoint directly
#
# slots:
#
#   superSet  : a character vector of all possible tx options
#
#----------------------------------------------------------------------#
setClass(Class = "TxInfoFactor",
         slots = c(superSet = "character"),
         contains = c("TxInfoBasic") )

# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                       TxInfoFactor GENERICS                      + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

setGeneric(name = ".compare",
           def = function(object, vec1, vec2){
                   standardGeneric(".compare")
                 })

setGeneric(name = ".convertTx",
           def = function(object, txVec){standardGeneric(".convertTx")})
          
setGeneric(name = ".getLevels", 
           def = function(object, txVec){standardGeneric(".getLevels")})

# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                       TxInfoFactor METHODS                       + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

#----------------------------------------------------------------------#
# Compare two factor vectors with possibly differing levels            #
#----------------------------------------------------------------------#
# input arguments                                                      #
#  object : an object of class TxInfoFactor                            #
#  vec1   : a factor vector                                            #
#  vec2   : a factor vector                                            #
# returns                                                              #
#  Logical vector indicating which elements of vec1 are equivalent     #
#  to the corresponding element of vec2                                #
#----------------------------------------------------------------------#
setMethod(f = ".compare",
          signature = c(object = "TxInfoFactor",
                        vec1 = "factor",
                        vec2 = "factor"),
          definition = function(object, vec1, vec2) {
                         vec1 <- as.character(vec1)
                         vec2 <- as.character(vec2)
                         ind <- {vec1 == vec2} |
                                {is.na(vec1) & is.na(vec2)}
                         return(ind)
                       } )

#----------------------------------------------------------------------#
# Convert the given vector to the appropriate class.                   #
#----------------------------------------------------------------------#
# input arguments                                                      #
#   object : an object of class TxInfoFactor.                          #
#   txVec  : vector                                                    #
# returns                                                              #
#   txVec cast as a factor vector.                                     #
#----------------------------------------------------------------------#
setMethod(f = ".convertTx",
          signature = c(object = "TxInfoFactor",
                        txVec = "ANY"), 
          definition = function(object, txVec) { 
                         return( as.factor(txVec) ) 
                       } )

#----------------------------------------------------------------------#
# Retrieve tx levels of txVec.                                         #
#----------------------------------------------------------------------#
# input arguments                                                      #
#   object : an object of class TxInfoFactor.                          #
#   txVec  : factor                                                    #
# returns                                                              #
#   A vector giving the levels in txVec                                #
#----------------------------------------------------------------------#
setMethod(f = ".getLevels",
          signature = c(object = "TxInfoFactor",
                        txVec = "factor"), 
          definition = function(object, txVec) { 
                         return( levels(txVec) ) 
                       } )


