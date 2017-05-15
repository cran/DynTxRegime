# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                            List CLASS                            + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

#----------------------------------------------------------------------#
# VIRTUAL class to mimic an object of class list.                      #
#----------------------------------------------------------------------#
# slots:
#
#  loo : an object of class "list" whose elements can be of any class
#
#----------------------------------------------------------------------#
setClass(Class = "List",
         slots = c(loo = "list"),
         contains = "VIRTUAL")

# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                           List METHODS                           + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

#----------------------------------------------------------------------#
# Retrieve the ith element of the list                                 #
#----------------------------------------------------------------------#
# required arguments                                                   #
#   x : object of class List                                           #
#   i : integer indicating the element of x to be returned             #
# returns                                                              #
#   ith element of object x                                            #
#----------------------------------------------------------------------#
setMethod(f = "[[",
          signature = c(x = "List"),
          definition = function(x,i){ return( x@loo[[i]] ) } )

#----------------------------------------------------------------------#
# Set the ith element of the list                                      #
#----------------------------------------------------------------------#
# required arguments                                                   #
#   x     : object of class List                                       #
#   i     : integer indicating the element of x to be set.             #
#   value : object to be stored in ith element of x                    #
# returns                                                              #
#   an object of class List with the ith element set to value          #
#----------------------------------------------------------------------#
setMethod(f = "[[<-",
          signature = c(x = "List"),
          definition = function(x,i,value){ 
                         x@loo[[i]] <- value
                         names(x@loo)[i] <- names(value)
                         return( x ) 
                       } )

#----------------------------------------------------------------------#
# Retrieve the length of the list                                      #
#----------------------------------------------------------------------#
# required arguments                                                   #
#   x : object of class List                                           #
# returns                                                              #
#   an integer giving the length of object x                           #
#----------------------------------------------------------------------#
setMethod(f = "length",
          signature = c(x = "List"),
          definition = function(x){ return( length(x@loo) ) } )

#----------------------------------------------------------------------#
# Retrieve the names of the list elements                              #
#----------------------------------------------------------------------#
# required arguments                                                   #
#   x : object of class List                                           #
# returns                                                              #
#   a character vector of element names                                #
#----------------------------------------------------------------------#
setMethod(f = "names",
          signature = c(x = "List"),
          definition = function(x){ return( names(x@loo) ) } )

