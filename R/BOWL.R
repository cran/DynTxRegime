#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#
# BOWL : Main function call for one step of the Backward Outcome       #
#        Weighted Learning method.                                     #
#                                                                      #
#    Yingqi Zhao, Donglin Zeng, Eric B. Laber, Michael R. Kosorok      #
#    (2015)                                                            #
#    New statistical learning methods for estimating optimal dynamic   #
#    treatment regimes.                                                #
#    Journal of the American Statistical Association,                  #
#    110:510, 583--598.                                                #
#                                                                      #
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#
#                                                                      #
# ...     : ignored. Used to require named input.                      #
#                                                                      #
# moPropen: an object of class modelObj or                             #
#           a list of objects of class modelObjSubset.                 #
#                                                                      #
#           defines the model and R methods to be used to obtain       #
#           parameter estimates and predictions for the propensity     #
#           for treatment.                                             #
#           See ?modelObj and/or ?modelObjSubset for details.          #
#                                                                      #
#           If the prediction method specified in moPropen returns     #
#           predictions for only a subset of the categorical tx data,  #
#           it is assumed that the base level defined by levels(tx)[1] #
#           is the missing category.                                   #
#                                                                      #
#           Note that it is assumed that the columns of the predictions#
#           are ordered in accordance with the vector returned by      #
#           levels().                                                  #
#                                                                      #
# data    : a data frame of the covariates and tx histories            #
#           tx variables must be integer or factor                     #
#                                                                      #
# reward  : a numeric vector.                                          #
#           The reward.                                                #
#                                                                      #
# txName  : a character object.                                        #
#           The column header of data that corresponds to the tx       #
#           covariate.                                                 #
#                                                                      #
# regime  : a formula object, or                                       # 
#           a list of formula objects                                  #
#                                                                      #
#           If fSet is NULL, regime is a formula object specifying     #
#           the decision function.                                     #
#                                                                      #
#           If fSet is not NULL, regime is a list of formula objects   #
#           specifying the decision function to be fit for each        #
#           subset. Each element of the list corresponds to a specific #
#           subset defined by fSet. The name of each element of the    #
#           list must match a nickname specified in fSet. The value    #
#           of each element is the formula representation of the model.#
#                                                                      #
# BOWLObj : An object of class BOWL or NULL.                           #
#           If object of class BOWL, the object returned by the        #
#           previous step of the BOWL algorithm. If NULL, it is        #
#           assumed that the analysis is for the final decision point. #
#                                                                      #
# lambdas : A numeric object or a numeric vector object giving the     #
#           penalty tuning parameter. If a vector, lambdas is          #
#           the finite set of values to be considered in the           #
#           cross-validation algorithm.                                #
#                                                                      #
# cvFolds : If cross-validation is to be used to select the tuning     #
#           parameters, the number of folds.                           #
#                                                                      #
# kernel  : a character object.                                        #
#           must be one of {linear, poly, radial}                      #
#                                                                      #
# kparam  : a numeric object of NULL.                                  #
#           If kernel = linear, kparam is ignored.                     #
#           If kernel = poly, kparam is the degree of the polynomial   #
#           If kernel = radial, kparam is the inverse bandwidth of the #
#           kernel. If a vector of bandwidth parameters is given,      #
#           cross-validation will be used to select the parameter      #
#                                                                      #
# fSet    : A function or NULL.                                        #
#           This argument allows the user to specify the subset of tx  #
#           options available to a patient or the subset of patients   #
#           that will be modeled uniquely.                             #
#           The functions should accept as input either                #
#             1) explicit covariate names as given in column headers   #
#                of data, or                                           #
#             2) a vector of covariates (i.e. a row of a data.frame)   #
#           and must return a list. The first element of the list is a #
#           character giving a nickname to the subset. The second      #
#           element is a vector of the tx options available to the     #
#           subset.                                                    #
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#
bowl <- function(...,
                 moPropen,  
                 data, 
                 reward,  
                 txName,
                 regime,
                 BOWLObj = NULL,
                 lambdas = 2.0,
                 cvFolds = 0L,
                 kernel = "linear",
                 kparam = NULL,
                 fSet = NULL,
                 verbose = TRUE) { 

  #------------------------------------------------------------------#
  # Verify that moPropen is appropriate type                         #
  #------------------------------------------------------------------#
  moPropen <- .checkModelObjOrModelObjSubset(object = moPropen, 
                                             nm = 'moPropen')

  #------------------------------------------------------------------#
  # data must be provided as a data.frame object.                    #
  #------------------------------------------------------------------#
  if( !is(data, "data.frame") ) {
    UserError("input", "'data' must be a data.frame")
  }

  #------------------------------------------------------------------#
  # Verify that a single reward vector is provided.                  #
  #------------------------------------------------------------------#
  if( is(ncol(reward), "NULL") ) reward <- matrix(reward, ncol=1L)
  if( ncol(reward) > 1L ) {
      UserError("input",
                paste("only the reward for the decision point under",
                      "analysis should be provided"))
  }
  reward <- drop(reward)

  #------------------------------------------------------------------#
  # Verify treatment is appropriately coded.                         #
  #------------------------------------------------------------------#
  data <- .checkTxData(txName, data)

  #------------------------------------------------------------------#
  # Treatment vector coded as -1.0/1.0                               #
  #------------------------------------------------------------------#
  if( is(data[,txName], "factor") ) {
    levs <- levels(data[,txName])
  } else {
    levs <- unique(data[,txName])
    levs <- sort(levs[!is.na(levs)])
  }

  txVec <- NULL

  if( length(levs) > 2L ) {
    if( is(fSet, "NULL") ) {
      UserError("input",
                paste("only binary treatment options can be used",
                      "in this method"))
    } else {
      txSet <- .getFeasibleTx(fSet = fSet, 
                              superSet = levs, 
                              data = data, 
                              suppress = TRUE)

      for( i in 1L:length(txSet$subsets) ) {
        if( length(txSet$subsets[[i]]) > 2L ) {
          UserError("input",
                    paste("only binary treatment options can be used",
                          "in this method"))
        }

        inSet <- txSet$ptsSubset == names(txSet$subsets)[i]

        txVec[inSet & {data[,txName] == levs[1L]}] <- -1.0
        txVec[inSet & {data[,txName] == levs[2L]}] <-  1.0
        
      }

    }
  }  else {
    txVec[data[,txName] == levs[1L]] <- -1.0
    txVec[data[,txName] == levs[2L]] <-  1.0
  }

  if( !isTRUE(all.equal(txVec, data[,txName])) ) {
    cat("Treatment variable converted to {-1,1}\n")
    data[,txName] <- as.integer(round(txVec,0))
  }

  #------------------------------------------------------------------#
  # regime must be formula.                                          #
  #------------------------------------------------------------------#
  if( !is(regime, "formula") ) {
    UserError("input", "'regime' must be a formula")
  }

  if( missing(BOWLObj) ) BOWLObj <- NULL

  if( !is(BOWLObj, "BOWL") && !is(BOWLObj, "NULL") ) {
    UserError("input",
              paste("BOWLObj must be a BOWL object from a previous",
                    "call or NULL if analysis is for the final",
                    "decision point"))
  }

  #------------------------------------------------------------------#
  # lambdas must be numeric.                                         #
  #------------------------------------------------------------------#
  if( !is(lambdas, "numeric") ) {
      UserError("input", "'lambdas' must be a numeric object")
  }

  #------------------------------------------------------------------#
  # Verify kernel information                                        #
  #------------------------------------------------------------------#
  kernel <- tolower(kernel)
  if( missing(kparam) ) kparam <- NULL

  kparam <- .checkKernel(kernel, kparam, cvFolds)

  #------------------------------------------------------------------#
  # cvFolds must be an integer.                                      #
  #------------------------------------------------------------------#
  if( !is(cvFolds, "integer") ) cvFolds <- as.integer(round(cvFolds,0L))

  #------------------------------------------------------------------#
  # If not using cross-validation to estimate lambda, verify that    #
  # only one lambda value is given. If more than 1, ignore all but   #
  # the first element.                                               #
  #------------------------------------------------------------------#
  if( cvFolds < 1L ) {
    cvFolds <- 0L
    if( length(lambdas) > 1L ) {
      warning("only first lambda value considered")
      lambdas <- lambdas[1L]
    }
    if( length(kparam) > 1L ) {
      warning("only first kparam value considered")
      kparam <- kparam[1L]
    }
  }

  #------------------------------------------------------------------#
  # Verify request for CV is appropriate.                            #
  #------------------------------------------------------------------#
  if( {cvFolds > 0L} && 
      {length(lambdas) == 1L} && 
      {length(kparam) == 1L} ) {
    cvFolds <- 0L
    warning(paste("cross-validation not performed;",
                  "only one pair of tuning parameters provided"))
  }

  #------------------------------------------------------------------#
  # fSet must be provided when modeling subsets.                     #
  #------------------------------------------------------------------#
  if( is(moPropen, "ModelObj_SubsetList") ) {
    if( is(fSet, "NULL") ) {
      UserError("input", 
                paste("when using objects of class modelObjSubset, ",
                      "'fSet' must be provided", sep=""))
    }
  }

  #------------------------------------------------------------------#
  # fSet must be NULL or a function.                                 #
  #------------------------------------------------------------------#
  if( !is(fSet, "function") && !is(fSet,"NULL") ) {
    UserError("input", "'fSet' must be NULL or a function")
  }

  if( !is(verbose, "logical") ) {
    UserError("input", "verbose must be a logical")
  }

  obj <- .newBOWL(moPropen = moPropen,
                  data = data,
                  reward = reward,
                  txName = txName,
                  regime = regime,
                  BOWLObj = BOWLObj,
                  cvFolds = cvFolds,
                  lambdas = lambdas,
                  kernel = kernel,
                  kparam = kparam,
                  fSet = fSet,
                  suppress = !verbose)

  obj@call <- match.call()

  return( obj )

}
