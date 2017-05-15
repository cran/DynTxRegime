# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                        TxInfoInteger CLASS                       + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

#----------------------------------------------------------------------#
# treatment name and superset of treatment options when treatment      #
# is an integer                                                        #
#----------------------------------------------------------------------#
# extends TxInfoBasic directly
#
# slots:
#
#   superSet  : an integer vector of all possible tx options
#
#----------------------------------------------------------------------#
setClass(Class = "TxInfoInteger",
         slots = c(superSet = "integer"),
         contains = c("TxInfoBasic") )

# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                      TxInfoInteger METHODS                       + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

#----------------------------------------------------------------------#
# Compare two integer vectors                                          #
#----------------------------------------------------------------------#
# input arguments                                                      #
#  object : an object of class TxInfoInteger                           #
#  vec1   : an integer vector                                          #
#  vec2   : an integer vector                                          #
# returns                                                              #
#  Logical vector indicating which elements of vec1 are equivalent     #
#  to the corresponding element of vec2                                #
#----------------------------------------------------------------------#
setMethod(f = ".compare",
          signature = c(object = "TxInfoInteger",
                        vec1 = "integer",
                        vec2 = "integer"),
          definition = function(object, vec1, vec2) {
                         ind <- {vec1 == vec2} |
                                {is.na(vec1) & is.na(vec2)}
                         return(ind)
                       } )

#----------------------------------------------------------------------#
# Convert the given vector to the appropriate class.                   #
#----------------------------------------------------------------------#
# input arguments                                                      #
#   object : an object of class TxInfoInteger                          #
#   txVec  : vector                                                    #
# returns                                                              #
#   txVec cast as an integer vector                                    #
#----------------------------------------------------------------------#
setMethod(f = ".convertTx",
          signature = c(object = "TxInfoInteger",
                        txVec = "numeric"), 
          definition = function(object, txVec){ 
                         return( as.integer(round(txVec,0L)) ) 
                       } )

#----------------------------------------------------------------------#
# Retrieve tx levels of txVec.                                         #
#----------------------------------------------------------------------#
# input arguments                                                      #
#   object : an object of class TxInfoInteger                          #
#   txVec  : integer                                                   #
# returns                                                              #
#   A vector giving the levels in txVec                                #
#----------------------------------------------------------------------#
setMethod(f = ".getLevels",
          signature = c(object = "TxInfoInteger",
                        txVec = "integer"), 
          definition = function(object, txVec){ 
                         res <- unique(txVec)
                         res <- sort(res[!is.na(res)])
                         return( res ) 
                       } )


