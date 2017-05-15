#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#
#  public function to perform a step of the Q-Learning algorithm       #
#  If an object of class QLearn is passed, it is assumed to be the     #
#  preceding step of the Q-Learning algorithm and models are fit       #
#  using the Ytilde variable of the QLearn object. If a vector         #
#  is passed, it is assumed that this is the first step in the         #
#  Q-Learning algorithm and models are fit using the response.         #
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#
#                                                                      #
# ...    : ignored                                                     #
#                                                                      #
# moMain  : an object of class modelObj or a list of objects of class  #
#           modelObjSubset, which define the models and R methods to   #
#           be used to obtain parameter estimates and predictions      #
#           for the main effects component of the outcome regression.  #
#           See ?modelObj and/or ?modelObjSubset for details.          #
#           NULL is an acceptable value if moCont is defined.          #
#                                                                      #
# moCont  : an object of class modelObj or a list of objects of class  #
#           modelObjSubset, which define the models and R methods to   #
#           be used to obtain parameter estimates and predictions      #
#           for the contrasts component of the outcome regression.     #
#           See ?modelObj and/or ?modelObjSubset for details.          #
#           NULL is an acceptable value if moMain is defined.          #
#                                                                      #
# data    : data frame of covariates and treatment histories           #
#                                                                      #
# response: response vector or object of class qLearn from a previous  #
#           Q-Learning step.                                           #
#                                                                      #
# txName  : character string giving column header of treatment variable#
#           in data                                                    #
#                                                                      #
# fSet    : A function.                                                #
#           This argument allows the user to specify the subset of tx  #
#           options available to a patient.                            #
#           The functions should accept as input either                #
#           1) explicit covariate names as given in column names of    #
#              data                                                    #
#           2) a vector of covariates (i.e. a row of a data.frame)     #
#           and must return a vector of tx options available to the    #
#           patient                                                    #
#           Note this function is used for an INDIVIDUAL patient,      #
#           matrix results are not appropriate                         #
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
#           Either categorical or integer data can be provided for the #
#           tx. If categorical, the fitted contrast and main effects   #
#           are defined relative to the base category {defined as      #
#           levels()[1]}. The values may not be those returned by      #
#           predict(object) if iterate fits are used. If integer, the  #
#           fitted contrast and main effects are defined               #
#           relative to no tx (tx = 0).                                #
#                                                                      #
#           Note that if iter <= 0, all non-model components of the    #
#           moMain and moCont must be identical                        #
#                                                                      #
# verbose:  a logical                                                  #
#           If TRUE, screen prints are generated.                      #
#                                                                      #
# Function returns an object of class QLearn                           #
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#
qLearn <- function(...,
                   moMain,
                   moCont, 
                   data, 
                   response, 
                   txName, 
                   fSet, 
                   iter = 0L,
                   verbose = TRUE) {

  if( missing(moMain) ) moMain <- NULL
  moMain <- .checkModelObjOrModelObjSubset(moMain, 'moMain')

  if( missing(moCont) ) moCont <- NULL
  moCont <- .checkModelObjOrModelObjSubset(moCont, 'moCont')

  if( !is(moCont, "NULL") && !is(moMain,"NULL") ) {
    if( is(moCont, "ModelObj_SubsetList") && 
       !is(moMain, "ModelObj_SubsetList") ) {
      UserError("input",
                "moMain and moCont must both be modelObjSubset")
    }

    if( is(moCont, "modelObj") && 
       !is(moMain, "modelObj") ) {
      UserError("input",
                "moMain and moCont must both be modelObj")
    }
  } else if( is(moCont, "NULL") && is(moMain, "NULL") ) {
    UserError("input",
              "must provide moMain and/or moCont")
  }

  #------------------------------------------------------------------#
  # data must be provided as a data.frame object.                    #
  #------------------------------------------------------------------#
  if( !is(data, "data.frame") ) {
    UserError("input", "'data' must be a data.frame")
  }

  #------------------------------------------------------------------#
  # response must be QLearn or vector                                #
  #------------------------------------------------------------------#
  if( !is(response, "QLearn") ) {

    if( is(response, "data.frame") ) response <- data.matrix(response)

    if( is(response, "matrix") ) {
      if( ncol(response) != 1L ) {
        UserError("input", 
                  paste("'response' must be a vector of responses or ",
                        "an object returned by a prior call to qLearn()",
                        sep=""))
      }
      response <- drop(response)
    }

  }

  if( !is(response, "QLearn")  && !is(response, "vector") ){
    UserError("input", 
              paste("'response' must be a vector of responses or ",
                    "an object returned by a prior call to qLearn()",
                    sep=""))
  }

  #------------------------------------------------------------------#
  # Verify treatment is appropriately coded.                         #
  #------------------------------------------------------------------#
  data <- .checkTxData(txName, data)

  if( !is(iter, "integer") ) iter <- as.integer(round(iter,0L))
  if( iter < 0 ) iter <- 0L

  if( missing(fSet) ) fSet <- NULL

  #------------------------------------------------------------------#
  # fSet must be provided when modeling subsets.                     #
  #------------------------------------------------------------------#
  if( is(moMain, "ModelObj_SubsetList") ||
      is(moCont, "ModelObj_SubsetList") ) {
    if( is(fSet, "NULL") ) {
      UserError("input", 
                paste("when using objects of class ModelObjSubset, ",
                      "'fSet' must be provided", sep=""))
    }
  }

  #------------------------------------------------------------------#
  # fSet must be NULL or a function.                                 #
  #------------------------------------------------------------------#
  if( !is(fSet, "function") && !is(fSet,"NULL") ) {
    UserError("input",
              "'fSet' must be NULL or a function")
  }

  if( !is(verbose, "logical") ) {
    UserError("input",
              "'verbose' must be a TRUE/FALSE")
  }

  result <- .newQLearn(moMain = moMain,
                       moCont = moCont,
                       data = data,
                       response = response,
                       txName = txName,
                       fSet = fSet,
                       iter = iter,
                       suppress = !verbose)

  result@call <- match.call()

  return(result)
}

