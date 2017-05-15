#----------------------------------------------------------------------#
# Verify that kernel inputs are appropriate and sets kparam to default #
# if not provided.                                                     #
#----------------------------------------------------------------------#
# kernel  : character. One of {linear, poly, radial}                   #
# kparam  : numeric or NULL                                            #
#           if kernel = linear, kparam -> 1.0                          #
#           if kernel = poly, kparam != NULL && length(kparam) = 1     #
#           if kernel = radial, kparam != NULL                         #
# cvFolds : logical. TRUE if cross-validation to be used               #
#----------------------------------------------------------------------#
# Returns the value for the kernel parameter.                          #
#----------------------------------------------------------------------#
.checkKernel <- function(kernel, kparam, cvFolds){

  if( kernel == "linear" ) {

    #----------------------------------------------------------------#
    # If kernel is linear, kparam is ignored. Set to 1.0 as a default#
    #----------------------------------------------------------------#
    kparam = 1.0

  } else if( kernel == "poly" ) {

    #----------------------------------------------------------------#
    # If kernel is polynomial, a single kparam must be given.        #
    # kparam is the order of the polynomial.                         #
    # (X^TX + 1)^kparam                                              #
    #----------------------------------------------------------------#
    if( is(kparam, "NULL") ) {
      UserError("input",
                "kparam must be set if kernel= 'poly'")
    }
    if( length(kparam) > 1L ) {
      UserError("input",
                "for poly, only one kernel parameter can be specified")
    } 

  } else if( kernel == "radial" ) {

    #----------------------------------------------------------------#
    # If kernel is radial, at least on kparam must be given.         #
    # exp( - ||X-X'||^2 / (2*kparam*kparam)                          #
    # If more than one value is provided, cross validation must have #
    # been specified.                                                #
    #----------------------------------------------------------------#
    if( is(kparam, "NULL") ) {
      UserError("input",
                "kparam must be set if kernel= 'radial'")
    }
    if( {length(kparam) > 1L} && !cvFolds ) {
      warning("only first kparam value considered")
      kparam <- kparam[1L]
    }

  } else {

    UserError("input",
              "kernel must be one of {'linear', 'poly', 'radial'}")
  }

  return(kparam)
}

#----------------------------------------------------------------------#
# Verify that treatment is provided appropriately                      #
#----------------------------------------------------------------------#
# txName : character. Column header of data containing treatment       #
# data   : data.frame of covariates and treatment histories            #
#----------------------------------------------------------------------#
# Returns data with treatment variable in correct class                #
#----------------------------------------------------------------------#
.checkTxData <- function(txName, data){

  #------------------------------------------------------------------#
  # txName must be an object of class character                      #
  #------------------------------------------------------------------#
  if( !is(txName, "character") ) {
    UserError("input",
              "'txName' must be a character.")
  }

  #------------------------------------------------------------------#
  # txName can only be a single name                                 #
  #------------------------------------------------------------------#
  if( length(txName) != 1L ) {
    UserError("input",
              "'txName' must be of length 1 for this method.")
  }

  #------------------------------------------------------------------#
  # Test to see if treatment is in data set provided.                #
  #------------------------------------------------------------------#
  txVec <- try(data[,txName], silent = TRUE)

  if( is(txVec,"try-error") ) {
    UserError("input",
              paste(txName, " not found in data.", sep="") )
  }

  #------------------------------------------------------------------#
  # If treatment is not a factor or an integer, attempt to coerce    #
  # to integer.                                                      #
  #------------------------------------------------------------------#
  if( !is(txVec,"factor") && !is(txVec, "integer") ) {
    if( is(txVec, "character") ) {
      data[,txName] <- factor(txVec)
    } else {
      if( !isTRUE(all.equal(txVec, round(txVec,0L))) ) {
        UserError("input",
                  "Treatment variable must be a factor or an integer.")
      }
      data[,txName] <- as.integer(round(data[,txName],0L))
   }
  }

  return(data)
}

#----------------------------------------------------------------------#
# Verify that model object is of class modelObj or modelObjSubset      #
# This verification is for single-decision-point methods               #
#----------------------------------------------------------------------#
# object : model object or list of model objects                       #
# nm     : character description of model object purpose               #
#----------------------------------------------------------------------#
# Returns object converted to appropriate internal class               #
#----------------------------------------------------------------------#
.checkModelObjOrModelObjSubset <- function(object, nm){

  if( is(object, "NULL") ) return(object)

  #--------------------------------------------------------------#
  # If object is a list is provided, must contain more than one  #
  # object of class ModelObjSubset                               #
  #--------------------------------------------------------------#
  if( is(object, "list") ) {

    if( length(object) == 0L ) {

      UserError("input",
                paste(nm, "can be a single modelObj",
                      "or a list of ModelObjSubset objects. Received",
                      "an empty list."))

    } else if( length(object) == 1L ) {

      if( is(object[[1L]], "ModelObjSubset") ) {

        object <- .newModelObjSubset(object)

      } else if( is(object[[1L]], "modelObj") ) {

        object <- object[[1L]]

      } else {

        UserError("input", 
                  paste(nm, "can be a single modelObj",
                        "or a list of ModelObjSubset. Received",
                        class(object[[1L]])))

      }

    } else {

      #----------------------------------------------------------#
      # Convert list into recognized internal class.             #
      #----------------------------------------------------------#
      object <- .newModelObjSubset(object)

    }

  } else if( is(object, "ModelObjSubset") ) {
      res <- list()
      res[[1L]] <- object

      object <- .newModelObjSubset(res)

  } else if( !is(object, "modelObj") ) {

    #--------------------------------------------------------------#
    # object must be a modelObj or list of ModelObjSubset.         #
    #--------------------------------------------------------------#
    UserError("input", 
              paste("For modeling the superset of treatment options, ",
                    nm, "must be of class modelObj.\n", 
                    "Received an object of class ", 
                    paste(is(object),collapse=","), ".",
                    sep=""))

  }

  return(object)

}

#----------------------------------------------------------------------#
# Verify that model object is of class modelObj or modelObjSubset      #
# This verification is for single/multi-decision-point methods         #
#----------------------------------------------------------------------#
# object : model object or list of model objects                       #
# nm     : character description of model object purpose               #
#----------------------------------------------------------------------#
# Returns object converted to appropriate internal class               #
#----------------------------------------------------------------------#
.checkModelObjOrModelObjSubsetOrList <- function(object, nm){

  if( is(object, "list") ) {

    if( length(object) == 0L ) {

      if(nm != "moPropen"){
      UserError("input",
                paste(nm, "can be NULL, a single modelObj,",
                      "a list of modelObj, or",
                      "a list of ModelObjSubset"))
      } else {
      UserError("input",
                paste(nm, "can be a single modelObj,",
                      "a list of modelObj, or",
                      "a list of ModelObjSubset"))

      }

    } else if( length(object) == 1L ) {

      if( is(object[[1L]], "ModelObjSubset") ) {

        object <- .newModelObjSubset(object)

      } else if( is(object[[1L]], "modelObj") ) {

        object <- object[[1L]]

      } else {

      if(nm != "moPropen"){
      UserError("input",
                paste(nm, "can be NULL, a single modelObj,",
                      "a list of modelObj, or",
                      "a list of ModelObjSubset"))
      } else {
      UserError("input",
                paste(nm, "can be a single modelObj,",
                      "a list of modelObj, or",
                      "a list of ModelObjSubset"))

      }

      }

    } else {

      objectClass <- is(object[[1L]])

      if( !any(objectClass %in% c("modelObj", "ModelObjSubset")) ) {
        UserError("input",
                  paste(nm, "elements must be of class",
                        "modelObj or ModelObjSubset"))
      }

      tst <- sapply(object, class) %in% objectClass
      if( any(!tst) ) {
        UserError("input",
                  paste("all elements of", nm, "must be of the",
                        "same class"))
      }
    }

    #--------------------------------------------------------------#
    # Convert list into recognized internal class.                 #
    #--------------------------------------------------------------#
    if( is(object, "list") ) {
      if( is(object[[1L]], "ModelObjSubset") ) {
        object <- .newModelObjSubset(object)
      } else {
        object <- new("ModelObj_DecisionPointList",
                      loo = object)
      }
    }

  } else if( !is(object, "modelObj") && !is(object, "NULL") ) {

      if(nm != "moPropen"){
      UserError("input",
                paste(nm, "can be NULL, a single modelObj,",
                      "a list of modelObj, or",
                      "a list of ModelObjSubset"))
      } else {
      UserError("input",
                paste(nm, "can be a single modelObj,",
                      "a list of modelObj, or",
                      "a list of ModelObjSubset"))

      }
  }

  return(object)
}

#----------------------------------------------------------------------#
# Verify that treatment is binary. Return vector cast as +/-1          #
#----------------------------------------------------------------------#
# txName : character. Column header of data containing treatment       #
# data   : data.frame of covariates and treatment histories            #
#----------------------------------------------------------------------#
# Returns vector of treatment cast as +/- 1                            #
#----------------------------------------------------------------------#
.checkBinaryTx <- function(txName, data) {

  txVec <- numeric(nrow(data))

  #------------------------------------------------------------------#
  # identify the levels of treatment                                 #
  #------------------------------------------------------------------#
  if( is(data[,txName], "factor") ) {
    levs <- levels(data[,txName])
  } else {
    levs <- unique(data[,txName])
    levs <- levs[!is.na(levs)]
    levs <- sort(levs)
  }

  #------------------------------------------------------------------#
  # If more than 2 options throw error                               #
  #------------------------------------------------------------------#
  if( length(levs) > 2L ) {
    UserError("input",
              "Only binary tx options can be used in this method.")
  }

  #------------------------------------------------------------------#
  # Create treatment vector cast as +/- 1 where -1 = base level      #
  #------------------------------------------------------------------#
  txVec[data[,txName] == levs[1L]] <- -1.0
  txVec[data[,txName] == levs[2L]] <-  1.0


  return(txVec)
}
