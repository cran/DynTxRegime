#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#
#                                                                      #
# iqLearnFSV - obtain the variance of the contrast mean                #
#              This is the variance step of the iqLearn method.        #
#                                                                      #
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#
# For homoskedastic :                                                  #
#                                                                      #
# object  : object of class iqLearnFS_C                                #
#                                                                      #
# For heteroskedastic :                                                #
#                                                                      #
# object  : object of class iqLearnFS_C                                #
#                                                                      #
# moMain  : an object of class modelObj that defines the models and R  #
#           methods to be used to obtain parameter estimates and       #
#           predictions for main effects component of outcome          #
#           regression.                                                #
#           See ?modelObj for details.                                 #
#           NULL is an acceptable value if moCont is defined.          #
#                                                                      #
# moCont  : an object of class modelObj that defines the models and R  #
#           methods to be used to obtain parameter estimates and       #
#           predictions for contrast component of outcome regression   #
#           See ?modelObj for details.                                 #
#           NULL is an acceptable value if moMain is defined.          #
#                                                                      #
# data    : data.frame of covariates and treatment histories           #
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
# verbose: a logical                                                   #
#           If FALSE, screen prints are suppressed.                    #
#                                                                      #
#======================================================================#
#=                                                                    =#
#= Returns an object of class iqLearnFS_VHom or iqLearnFS_VHet        =#
#=                                                                    =#
#======================================================================#
iqLearnFSV <- function(...,
                       object, 
                       moMain, 
                       moCont, 
                       data = NULL, 
                       iter = 0L,
                       verbose = TRUE){
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
  #++++++                     Verify Input                     ++++++#
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
  if( !is(object,'IQLearnFS_C') ){
    msg <- "'object' must be an object returned by a call to iqLearnFSC()"
    UserError("input", msg)
  }

  if( missing(moMain) ) moMain <- NULL
  if( missing(moCont) ) moCont <- NULL

  if( is.null(moMain) && is.null(moCont) ){
    UserError("input", "must provide moMain and/or moCont")
  }

  #------------------------------------------------------------------#
  # moMain must be either an object of class modelObj or NULL        #
  #------------------------------------------------------------------#
  if( !is(moMain, 'modelObj') && !is(moMain,'NULL') ) {
    UserError("input",
              "the class of 'moMain' must be one of {modelObj, NULL}")
  }

  #------------------------------------------------------------------#
  # moCont must be either an object of class modelObj or NULL        #
  #------------------------------------------------------------------#
  if( !is(moCont, 'modelObj') && !is(moCont,'NULL') ) {
    UserError("input",
              "the class of 'moCont' must be one of {modelObj, NULL}")
  }

  #------------------------------------------------------------------#
  # At least one of {moMain, moCont} must be an object of class      #
  # modelObj If either is NULL, iterative algorithm is not           #
  # appropriate.                                                     #
  #------------------------------------------------------------------#
  if( is(moMain, "NULL") || is(moCont, "NULL") ) {
    iter = 0L
  }

  #------------------------------------------------------------------#
  # data must be an object of class data.frame                       #
  #------------------------------------------------------------------#
  if( !is(data, "data.frame") ) {
    UserError("input", "'data' must be a data.frame")
  }

  #------------------------------------------------------------------#
  # iter must be an integer                                          #
  #------------------------------------------------------------------#
  if( !is(iter, "integer") ) iter <- as.integer(round(iter,0L))
  if( iter < 0 ) iter <- 0L

  if( !is(verbose, "logical") ) {
    UserError("input",
              "'verbose' must be a TRUE/FALSE")
  }

  result <- .newIQLearnFS_VHet(object = object, 
                               moMain = moMain,  
                               moCont = moCont,  
                               data = data,  
                               iter = iter,
                               suppress = !verbose)

  result@call <- match.call()

  return(result)

}
