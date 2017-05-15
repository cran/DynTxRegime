# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                         Class CVInfo1Par                         + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# pars  : vector of possible parameter values                          #
# opt   : optimal parameter values                                     #
# value : matrix of values at each parameter                           #
setClass(Class = "CVInfo",
         slots = c(value = "matrix",
                   pars  = "numeric",
                   opt   = "numeric"))

# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                       CVInfo1Par GENERICS                        + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

setGeneric(name = ".getValue", 
           def = function(object){standardGeneric(".getValue")})

# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                          CVInfo METHODS                          + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

#----------------------------------------------------------------------#
# Retrieve the parameters at which values were calculated              #
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class CVInfo1Par                               #
#   returns                                                            #
# a vector of parameter values                                         #
#----------------------------------------------------------------------#
setMethod(f = ".getPars",   
          signature = c(object = "CVInfo"), 
          definition = function(object) {
                         return(object@pars)
                       })

#----------------------------------------------------------------------#
# Retrieve the value matrix                                            #
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class CVInfo1Par                               #
#   returns                                                            #
# the matrix of values                                                 #
#----------------------------------------------------------------------#
setMethod(f = ".getValue",   
          signature = c(object = "CVInfo"), 
          definition = function(object) {
                         return(object@value)
                       })

setMethod(f = "cvInfo",   
          signature = c(object = "CVInfo"), 
          definition = function(object) {
                         return(object@value)
                       })

#----------------------------------------------------------------------#
# Results of cross-validation of one parameter                         #
#----------------------------------------------------------------------#
setClass(Class = "CVInfo1Par",
         contains = c("CVInfo"))


# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                         Class CVInfo2Par                         + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

#----------------------------------------------------------------------#
# Results of cross-validation of two parameters                        #
#----------------------------------------------------------------------#
# par2  : vector of possible parameter values                          #
# opt2  : optimal parameter values                                     #
#----------------------------------------------------------------------#
setClass(Class = "CVInfo2Par",
         slots = c(pars2 = "numeric",
                   opt2  = "numeric"),
         contains = c("CVInfo"))

# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                        CVInfo2Par METHODS                        + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

#----------------------------------------------------------------------#
# Retrieve the parameters at which values were calculated              #
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class CVInfo2Par                               #
#   returns                                                            #
# a list of 2 elements. First element is a vector of parameter values  #
# for lambda, Second element is a vector of parameter values for kparam#
#----------------------------------------------------------------------#
setMethod(f = ".getPars",   
          signature = c(object = "CVInfo2Par"), 
          definition = function(object) {
                         result <- .getPars(object = as(object, "CVInfo"))
                         result <- list("lambda" = result,
                                        "kparam" = object@pars2)
                         return(result)
                       })


setClassUnion("CVInfoOrNULL",
              members = c("CVInfo", "NULL"))
