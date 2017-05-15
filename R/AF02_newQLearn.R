setGeneric(name = ".newQLearn", 
           def = function(moMain, moCont, fSet, response, ...){
                   standardGeneric(".newQLearn")
                 })

#----------------------------------------------------------------------#
# Q-Learning Method for Single Decision Point final dp                 #
#----------------------------------------------------------------------#
#   params                                                             #
# moMain   : an object of class modelObj, ModelObj_SubsetList, or NULL #
# moCont   : an object of class modelObj, ModelObj_SubsetList, or NULL #
#            note that at least 1 of moMain and moCont must be defined #
# fSet     : a function or NULL                                        #
# response : response vector or QLearn object                          #
# data     : data.frame of covariates                                  #
# txName   : string; column header of treatment variable in data       #
# iter     : maximum number of iterations if iterative algorithm used  #
# suppress : TRUE/FALSE indicating of screen prints are generated      #
#   returns                                                            #
# an object of class QLearn.                                           #
#----------------------------------------------------------------------#
.firstQLearn <- function(moMain, 
                         moCont, 
                         fSet, 
                         response, 
                         data,
                         txName, 
                         iter, 
                         suppress){

  if( !suppress ) cat("First step of the Q-Learning Algorithm.\n")

  step <- 1L

  result <- .qLearn(moMain = moMain, 
                    moCont = moCont,  
                    data = data,  
                    response = response, 
                    txName = txName,  
                    fSet = fSet,  
                    iter = iter,  
                    step = step,
                    suppress = suppress)

  return(result)

}

setMethod(f = ".newQLearn",    
          signature = c(moMain   = "modelObj",
                        moCont   = "modelObj",
                        fSet     = "function",
                        response = "vector"), 
          definition = .firstQLearn)

setMethod(f = ".newQLearn",    
          signature = c(moMain   = "modelObj",
                        moCont   = "NULL",
                        fSet     = "function",
                        response = "vector"), 
          definition = .firstQLearn)

setMethod(f = ".newQLearn",    
          signature = c(moMain   = "NULL",
                        moCont   = "modelObj",
                        fSet     = "function",
                        response = "vector"), 
          definition = .firstQLearn)

setMethod(f = ".newQLearn",    
          signature = c(moMain   = "modelObj",
                        moCont   = "modelObj",
                        fSet     = "NULL",
                        response = "vector"), 
          definition = .firstQLearn)

setMethod(f = ".newQLearn",    
          signature = c(moMain   = "modelObj",
                        moCont   = "NULL",
                        fSet     = "NULL",
                        response = "vector"), 
          definition = .firstQLearn)

setMethod(f = ".newQLearn",    
          signature = c(moMain   = "NULL",
                        moCont   = "modelObj",
                        fSet     = "NULL",
                        response = "vector"), 
          definition = .firstQLearn)

setMethod(f = ".newQLearn",    
          signature = c(moMain   = "ModelObj_SubsetList",
                        moCont   = "ModelObj_SubsetList",
                        fSet     = "function",
                        response = "vector"), 
          definition = .firstQLearn)

setMethod(f = ".newQLearn",    
          signature = c(moMain   = "ModelObj_SubsetList",
                        moCont   = "NULL",
                        fSet     = "function",
                        response = "vector"), 
          definition = .firstQLearn)


setMethod(f = ".newQLearn",    
          signature = c(moMain   = "NULL",
                        moCont   = "ModelObj_SubsetList",
                        fSet     = "function",
                        response = "vector"), 
          definition = .firstQLearn)


# Q-Learning Method for Single Decision Point non-final dp
.nextQLearn <- function(moMain, 
                        moCont, 
                        fSet, 
                        response, 
                        data,
                        txName, 
                        iter, 
                        suppress){

  step <- response@step + 1L

  if( !suppress ) cat("Step", step, "of the Q-Learning Algorithm.\n")

  #------------------------------------------------------------------#
  # Response is value function of previous                           #
  #------------------------------------------------------------------#
  response <- response@decisionFunc
  response <- apply(X = response,
                    MARGIN = 1L,
                    FUN = max, na.rm = TRUE)

  result <- .qLearn(moMain = moMain, 
                    moCont = moCont,  
                    data = data,  
                    response = response, 
                    txName = txName,  
                    fSet = fSet,  
                    iter = iter,  
                    step = step,
                    suppress = suppress)

  return(result)

}

setMethod(f = ".newQLearn",    
          signature = c(moMain = "modelObj",
                        moCont = "modelObj",
                        fSet   = "function",
                        response = "QLearn"), 
          definition = .nextQLearn)

setMethod(f = ".newQLearn",    
          signature = c(moMain = "modelObj",
                        moCont = "NULL",
                        fSet   = "function",
                        response = "QLearn"), 
          definition = .nextQLearn)

setMethod(f = ".newQLearn",    
          signature = c(moMain = "NULL",
                        moCont = "modelObj",
                        fSet = "function",
                        response = "QLearn"), 
          definition = .nextQLearn)

setMethod(f = ".newQLearn",    
          signature = c(moMain = "modelObj",
                        moCont = "modelObj",
                        fSet   = "NULL",
                        response = "QLearn"), 
          definition = .nextQLearn)

setMethod(f = ".newQLearn",    
          signature = c(moMain = "modelObj",
                        moCont = "NULL",
                        fSet   = "NULL",
                        response = "QLearn"), 
          definition = .nextQLearn)

setMethod(f = ".newQLearn",    
          signature = c(moMain = "NULL",
                        moCont = "modelObj",
                        fSet = "NULL",
                        response = "QLearn"), 
          definition = .nextQLearn)

setMethod(f = ".newQLearn",    
          signature = c(moMain = "ModelObj_SubsetList",
                        moCont = "ModelObj_SubsetList",
                        fSet = "function",
                        response = "QLearn"), 
          definition = .nextQLearn)

setMethod(f = ".newQLearn",    
          signature = c(moMain = "ModelObj_SubsetList",
                        moCont = "NULL",
                        fSet = "function",
                        response = "QLearn"), 
          definition = .nextQLearn)

setMethod(f = ".newQLearn",    
          signature = c(moMain = "NULL",
                        moCont = "ModelObj_SubsetList",
                        fSet = "function",
                        response = "QLearn"), 
          definition = .nextQLearn)

#----------------------------------------------------------------------#
# Q-Learning Method for Single Decision Point                          #
#----------------------------------------------------------------------#
#   params                                                             #
# moMain   : an object of class modelObj, ModelObj_SubsetList, or NULL #
# moCont   : an object of class modelObj, ModelObj_SubsetList, or NULL #
#            note that at least 1 of moMain and moCont must be defined #
# data     : data.frame of covariates                                  #
# response : response vector                                           #
# txName   : string; column header of treatment variable in data       #
# fSet     : a function or NULL                                        #
# iter     : maximum number of iterations if iterative algorithm used  #
# step     : integer step of the Q-learning algorithm                  #
# suppress : TRUE/FALSE indicating of screen prints are generated      #
#   returns                                                            #
# an object of class QLearn.                                           #
#----------------------------------------------------------------------#
.qLearn <- function(moMain, moCont, data, response, 
                    txName, fSet, iter, step, suppress){

  #------------------------------------------------------------------#
  # Generate tx information for calculation.                         #
  #------------------------------------------------------------------#
  txInfo <- .newTxInfo(fSet = fSet, 
                       txName = txName,   
                       data = data, 
                       suppress = suppress)

  #------------------------------------------------------------------#
  # Outcome regression analysis.                                     #
  #------------------------------------------------------------------#
  est <- .newOutcomeRegression(moMain = moMain, 
                               moCont = moCont,
                               data = data, 
                               response = response, 
                               iter = iter, 
                               txInfo = txInfo,
                               suppress = suppress)

  #------------------------------------------------------------------#
  # Retrieve Q-Functions at each treatment                           #
  #------------------------------------------------------------------#
  qf <- .predictAllTreatments(object = est, data = data, response = response)

  #------------------------------------------------------------------#
  # Determine optimal treatment in terms of original tx variable     #
  #------------------------------------------------------------------#
  optTx <- apply(X = qf$vals, MARGIN = 1L, FUN = which.max)
  optTx <- qf$ss[optTx]

  #------------------------------------------------------------------#
  # Estimate value                                                   #
  #------------------------------------------------------------------#
  val <- mean(apply(X = qf$vals, MARGIN = 1L, FUN = max, na.rm = TRUE))

  if( !suppress ) cat("Estimated value:", val, "\n")

  result <- new("QLearn", 
                "step"           = step,
                "decisionFunc"   = qf$vals,
                "outcome"        = est,
                "optimalTx"      = optTx,
                "estimatedValue" = val,
                "call"           = NULL)

  return(result)

}
