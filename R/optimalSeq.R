#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#
# Main function call for robust estimation of optimal dynamic          #
#  tx regimes for sequential tx decisions as described in              #
#                                                                      #
#  Baqun Zhang, Anastasios A. Tsiatis, Eric B. Laber & Marie Davidian, #
#  "A Robust Method for Estimating Optimal Treatment Regimes",         #
#  Biometrics, 68, 1010-1018.                                          #
#                                                                      #
#    and                                                               #
#                                                                      #
#  Baqun Zhang, Anastasios A. Tsiatis, Eric B. Laber & Marie Davidian, #
#  "Robust estimation of optimal treatment regimes for sequential      #
#  treatment decisions", Biometrika (2013) pp.1-14.                    #
#                                                                      #
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#
# moPropen: an object of class modelObj, a list of objects of class    #
#           modelObj, or a list of object of class modelObjSubset,     #
#           which define the models and R methods to be used to obtain #
#           parameter estimates and predictions for the propensity for #
#           treatment.                                                 #
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
# moMain  : an object of class modelObj, a list of objects of class    #
#           modelObj, or a list of object of class modelObjSubset,     #
#           which define the models and R methods to be used to obtain #
#           parameter estimates and predictions for the main effects   #
#           component of the outcome regression.                       #
#           See ?modelObj and/or ?modelObjSubset for details.          #
#           NULL is an acceptable input if IPWE is desired or there is #
#           no main effects component of the outcome regression model. #
#                                                                      #
# moCont  : an object of class modelObj, a list of objects of class    #
#           modelObj, or a list of object of class modelObjSubset,     #
#           which define the models and R methods to be used to obtain #
#           parameter estimates and predictions for the contrasts      #
#           component of the outcome regression.                       #
#           See ?modelObj and/or ?modelObjSubset for details.          #
#           NULL is an acceptable input if IPWE is desired or there is #
#           no contrast component of the outcome regression model.     #
#                                                                      #
# data    : a data frame of the covariates and tx histories            #
#           tx variables will be recast as integers if provided as     #
#           numeric or non-factor                                      #
#                                                                      #
# response: response vector                                            #
#                                                                      #
# txName  : a vector of characters.                                    #
#           The column headers of \emph{data} that correspond to the tx#
#           covariate for each decision point.                         #
#           The ordering should be sequential, i.e., the 1st element   #
#           gives column name for the 1st decision point tx, the       #
#           2nd gives column name for the 2nd decision point tx,       #
#           etc.                                                       #
#                                                                      #
# regimes : a function or a list of functions.                         #
#           For each decision point, a function defining the tx        #
#           rule. For example, if the tx rule is : I(eta_1 < x1),      #
#           regimes is defined as                                      #
#             regimes <- function(a,data){as.numeric(a < data$x1)}     #
#           THE LAST ARGUMENT IS ALWAYS TAKEN TO BE THE DATA.FRAME     #
#                                                                      #
# fSet    : A function or a list of functions.                         #
#           This argument allows the user to specify the subset of tx  #
#           options available to a patient or the subset of patients   #
#           that will be modeled uniquely.                             #
#           The functions should accept as input either                #
#             1) explicit covariate names as given in column headers   #
#                of data                                               #
#             2) a vector of covariates (i.e. a row of a data.frame)   #
#           and must return a list. The first element of the list is a #
#           character giving a nickname to the subset. The second      #
#           element is a vector of the tx options available to the     #
#           subset.                                                    #
#                                                                      #
# refit   : no longer used                                             #
#                                                                      #
# iter    : an integer                                                 #
#                                                                      #
#           >=1 if moMain and moCont are to be fitted iteratively      #
#           The value is the maximum number of iterations.             #
#           Note the iterative algorithms is as follows:               #
#           Y = Ymain + Ycont                                          #
#            (1) hat(Ycont) = 0                                        #
#            (2) Ymain = Y - hat(Ycont)                                #
#            (3) fit Ymain ~ moMain                                    #
#            (4) set Ycont = Y - hat(Ymain)                            #
#            (5) fit Ycont ~ moCont                                    #
#            (6) Repeat steps (2) - (5) until convergence or           #
#            a maximum of iter iterations.                             #
#                                                                      #
#           <=0 moMain and moCont will be combined and fit as a single #
#           object.                                                    #
#                                                                      #
#           Note that if iter <= 0, all non-model components of the    #
#           moMain and moCont must be identical                        #
#                                                                      #
# verbose:  a logical                                                  #
#           If FALSE, screen prints are suppressed.                    #
#                                                                      #
# ...     : Additional arguments required by rgenoud. At a minimum this#
#           should include Domains, pop.size and starting.values.      #
#           See ?rgenoud for more information.                         #
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#
optimalSeq <- function(...,
                       moPropen,
                       moMain,
                       moCont,
                       data,
                       response,
                       txName,
                       regimes,
                       fSet = NULL,
                       refit = FALSE,
                       iter = 0L,
                       verbose = TRUE) {

  if( !requireNamespace("rgenoud", quietly=TRUE) ) {
    stop(paste("optimalSeq requires the rgenoud package,",
               "which is available on CRAN."))
  }

  #------------------------------------------------------------------#
  # Ensure that moPropen is modelObj, list of modelObj or list of    #
  # modelObjSusbet.                                                  #
  #------------------------------------------------------------------#
  if( missing(moPropen) ) moPropen <- NULL
  if( is(moPropen, "NULL") ) {
    UserError("input", "moPropen must be provided")
  }
  moPropen <- .checkModelObjOrModelObjSubsetOrList(moPropen, 'moPropen')

  #------------------------------------------------------------------#
  # Ensure that moMain is modelObj, list of modelObj or list of      #
  # modelObjSusbet.                                                  #
  #------------------------------------------------------------------#
  if( missing(moMain) ) moMain <- NULL
  moMain <- .checkModelObjOrModelObjSubsetOrList(moMain, 'moMain')

  #------------------------------------------------------------------#
  # Ensure that moCont is modelObj, list of modelObj or list of      #
  # modelObjSusbet.                                                  #
  #------------------------------------------------------------------#
  if( missing(moCont) ) moCont <- NULL
  moCont <- .checkModelObjOrModelObjSubsetOrList(moCont, 'moCont')

  if( missing(regimes) ) {
    UserError("input", "'regimes' must be provided")
  }

  if( !is(regimes,"function") ) {
    if( is(regimes,"list") ) {
      if( !is(regimes[[1L]], "function") ) {
        UserError("input", "'regimes' must be a function or a list of functions")
      }
    } else {
      UserError("input", "'regimes' must be a function or a list of functions")
    }
  }

  #------------------------------------------------------------------#
  # data must be provided as a data.frame object.                    #
  #------------------------------------------------------------------#
  if( !is(data, "data.frame") ) {
    UserError("input", "'data' must be a data.frame")
  }

  #------------------------------------------------------------------#
  # response must be vector or matrix                                #
  #------------------------------------------------------------------#
  if( is(response, "data.frame") ) response <- data.matrix(response)
  if( is(response, "matrix") ) {
    if( ncol(response) > 1L ) stop("only a single response variable allowed")
    response <- drop(response)
  }

  nDP <- length(txName)

  if( is(moPropen, "ModelObj_DecisionPointList") || 
      is(moPropen, "ModelObj_SubsetList_DecisionPointList") ) {
    if( length(moPropen) != nDP ) {
      UserError("input",
                "incorrect number of propensity models provided")
    }
  } else if( is(moPropen, "modelObj") || 
             is(moPropen, "ModelObj_SubsetList") ) {
    if( nDP != 1L ) {
      UserError("input",
                "incorrect number of propensity models provided")
    }
  }

  if( is(moMain, "ModelObj_DecisionPointList") || 
      is(moMain, "ModelObj_SubsetList_DecisionPointList") ) {
    if( length(moMain) != nDP ) {
      UserError("input",
                "incorrect number of main effect models provided")
    }
  } else if( is(moMain, "modelObj") || 
             is(moMain, "ModelObj_SubsetList") ) {
    if( nDP != 1L ) {
      UserError("input",
                "incorrect number of main effect models provided")
    }
  }

  if( is(moCont, "ModelObj_DecisionPointList") || 
      is(moCont, "ModelObj_SubsetList_DecisionPointList") ) {
    if( length(moCont) != nDP ) {
      UserError("input",
                "incorrect number of contrast models provided")
    }
  } else if( is(moCont, "modelObj") || 
             is(moCont, "ModelObj_SubsetList") ) {
    if( nDP != 1L ) {
      UserError("input",
                "incorrect number of contrast models provided")
    }
  }

  if( !is(moMain, "NULL") && !is(moCont, "NULL") ) {
    tst <- all.equal(is(moMain), is(moCont))
    if( !is(tst, "logical") || !tst ) {
      UserError("input",
                "moMain and moCont must be the same class")
    }
  }

  #------------------------------------------------------------------#
  # txName must be an object of class character                      #
  #------------------------------------------------------------------#
  if( length(txName) != nDP ) {
    UserError("input",
              "incorrect number of treatment variables provided")
  }

  #------------------------------------------------------------------#
  # Verify treatment is appropriately coded.                         #
  #------------------------------------------------------------------#
  for( i in 1L:length(txName) ) {
    data <- .checkTxData(txName[i], data)
  }

  #------------------------------------------------------------------#
  # Convert regimes input into local class Regime or RegimeList      #
  #------------------------------------------------------------------#
  regimes <- .newRegime(regimes)

  if( is(regimes, "Regime") ) {
    if( nDP != 1L ) {
      UserError("input",
                "incorrect number of regimes provided")
    }
  } else {
    if( length(regimes) != nDP ) {
      UserError("input",
                "incorrect number of regimes provided")
    }
  }

  if( missing(fSet) ) fSet <- NULL

  if( nDP == 1L ) {
    if( !is(fSet, "function") && !is(fSet,"NULL") ) {
      UserError("input",
                "fSet must be NULL, a function, or a list of functions")
    }
  } else {
    if( !is(fSet, "list") && !is(fSet,"NULL") ) {
      UserError("input",
                "fSet must be NULL or a list of functions")
    }
    if( is(fSet, "list") ) {
      if( length(fSet) != nDP ) {
        UserError("input",
                  "incorrect number of fSet provided")
      }
    }
  }

  if( is(moPropen, "ModelObj_SubsetList_DecisionPointList") ||
      is(moPropen, "ModelObj_SubsetList") ||
      is(moMain, "ModelObj_SubsetList_DecisionPointList") ||
      is(moMain, "ModelObj_SubsetList") || 
      is(moCont, "ModelObj_SubsetList_DecisionPointList") ||
      is(moCont, "ModelObj_SubsetList") ) {
    if( is(fSet, "NULL") ) {
      UserError("input",
                "fSet must be provided when subset modeling requested")
    }
  } else {
    if( {is(moPropen, "ModelObj_DecisionPointList") ||
         is(moPropen, "modelObj")} &&
        {is(moMain, "ModelObj_DecisionPointList") ||
         is(moMain, "modelObj") || is(moMain, "NULL")} && 
        {is(moCont, "ModelObj_DecisionPointList") ||
         is(moCont, "modelObj") || is(moCont, "NULL")} ) {
      if( !is(fSet, "NULL") ) {
        UserError("input",
                  "fSet only provided if subset modeling requested")
      }
    }
  }

  if( !is(iter, "integer") ) iter <- as.integer(round(iter,0L))
  if( iter < 0 ) iter <- 0L

  if( !is(verbose, "logical") ) {
    UserError("input",
              "'verbose' must be a TRUE/FALSE")
  }

  argsList <- list(...)

  tst <- c("Domains", "pop.size", "starting.values") %in% names(argsList)

  if( !all(tst) ) {
    UserError("input",
              "Domains, pop.size, and starting.values for rgenoud are required")
  }

  result <- .newOptimalSeq(moPropen = moPropen,
                           moMain = moMain,
                           moCont = moCont,
                           data = data,
                           response = response,
                           txName = txName,
                           regimes = regimes,
                           fSet = fSet,
                           iter = iter,
                           suppress = !verbose,
                           argsList = argsList)
    
  result@call <- match.call()

  return(result)
}
