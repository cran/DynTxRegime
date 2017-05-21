# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                    OutcomeRegression GENERICS                    + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

setGeneric(name = ".newOutcomeRegression",
           def = function(moMain, moCont, txInfo, ...){
                     standardGeneric(".newOutcomeRegression")
                 })

# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                     OutcomeRegression METHODS                    + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

#----------------------------------------------------------------------#
# Choose combined (Simple) or iterate fitting algorithm and perform    #
# outcome regression.                                                  #
#----------------------------------------------------------------------#
#   params                                                             #
# moMain  : an object of class modelObj or NULL,                       #
# moCont  : an object of class modelObj or NULL,                       #
#           Note that at least one of moMain and moCont must be defined#
# txInfo  : TxInfoNoSubsets object                                     #
# data    : data.frame of covariates and treatment histories           #
# response: response vector                                            #
# iter    : an integer                                                 #
# suppress : T/F indicating if prints to screen are to be executed     #
#----------------------------------------------------------------------#
setMethod(f = ".newOutcomeRegression",
          signature = c(moMain = "modelObj",
                        moCont = "modelObj",
                        txInfo = "TxInfoBasic"),
          definition = function(moMain,
                                moCont,
                                txInfo,
                                data,
                                response,
                                iter,
                                suppress){

                         #-----------------------------------------#
                         # If iter <=0, methods specified in model #
                         # object must be the same.                #
                         #-----------------------------------------#
                         if( iter <= 0L ){
                           once <- isTRUE(all.equal(solver(moMain),
                                                    solver(moCont)))

                           if(!once){
                             warning(paste("Solver method for main",
                                           "effects and contrasts",
                                           "differ.\n",
                                           "Solutions will be obtained",
                                           "using iterative method."))
                             iter <- 100L
                           }
                         }

                         if( iter <= 0L ) {
                           fit <- .newTypedSimpleFit(moMain = moMain,
                                                     moCont = moCont,
                                                     txInfo = txInfo,
                                                     data = data,
                                                     response = response,
                                                     suppress = suppress)
                         } else {
                           fit <- .newIterateFit(moMain = moMain,
                                                 moCont = moCont,
                                                 response = response,
                                                 txInfo = txInfo,
                                                 data = data,
                                                 max.iter = iter,
                                                 suppress = suppress)
                         }

                         return(fit)
                       } )

setMethod(f = ".newOutcomeRegression",
          signature = c(moMain = "modelObj",
                        moCont = "NULL",
                        txInfo = "TxInfoBasic"),
          definition = function(moMain,
                                moCont,
                                txInfo,
                                data,
                                response,
                                iter,
                                suppress){

                         fit <- .newTypedSimpleFit(moMain = moMain,
                                                   moCont = moCont,
                                                   txInfo = txInfo,
                                                   data = data,
                                                   response = response,
                                                   suppress = suppress)

                         return(fit)
                       } )

setMethod(f = ".newOutcomeRegression",
          signature = c(moMain = "NULL",
                        moCont = "modelObj",
                        txInfo = "TxInfoBasic"),
          definition = function(moMain,
                                moCont,
                                txInfo,
                                data,
                                response,
                                iter,
                                suppress){

                         fit <- .newTypedSimpleFit(moMain = moMain,
                                                   moCont = moCont,
                                                   txInfo = txInfo,
                                                   data = data,
                                                   response = response,
                                                   suppress = suppress)

                         return(fit)
                       } )

#----------------------------------------------------------------------#
# Obtain parameter estimates for each subset of data                   #
#----------------------------------------------------------------------#
#   params                                                             #
# moMain   : an object of class ModelObj_SubsetList or NULL            #
# moCont   : an object of class ModelObj_SubsetList or NULL            #
#            note that at least 1 of moMain and moCont must be defined #
# txInfo   : object of class TxInfo                                    #
# data     : data.frame of covariates and treatment histories          #
# response : response vector                                           #
# iter     : the number of iterations to use if iterative algorithm    #
#   returns                                                            #
# an object of class SubsetListFit containing all key results of all   #
# regressions.                                                         #
#----------------------------------------------------------------------#
.subsetMEC <- function(moMain,
                       moCont,
                       txInfo,
                       data,
                       response,
                       iter,
                       suppress){

  res <- list()

  nmsMoMain <- names(moMain@loo)
  nmsMoCont <- names(moCont@loo)

  uniqueNames <- unique(c(nmsMoMain, nmsMoCont))

  for( i in 1L:length(uniqueNames) ) {

    #--------------------------------------------------------------#
    # Identify which contrast model partners this main effects     #
    #--------------------------------------------------------------#
    j <- which(nmsMoCont == uniqueNames[i])
    k <- which(nmsMoMain == uniqueNames[i])

    if( length(j) != 0L && length(k) != 0L ) {
      res[[ uniqueNames[i] ]] <- .newSubsetFit(moMain = moMain[[k]],
                                               moCont = moCont[[j]],
                                               data = data,
                                               response = response,
                                               txInfo = txInfo,
                                               iter = iter,
                                               modelSubset = uniqueNames[i],
                                               suppress = suppress)
    } else if( length(j) != 0L ) {

      res[[ uniqueNames[i] ]] <- .newSubsetFit(moMain = NULL,
                                               moCont = moCont[[j]],
                                               data = data,
                                               response = response,
                                               txInfo = txInfo,
                                               iter = iter,
                                               modelSubset = uniqueNames[i],
                                               suppress = suppress)
    } else if( length(k) != 0L ) {
      res[[ uniqueNames[i] ]] <- .newSubsetFit(moMain = moMain[[k]],
                                               moCont = NULL,
                                               data = data,
                                               response = response,
                                               txInfo = txInfo,
                                               iter = iter,
                                               modelSubset = uniqueNames[i],
                                               suppress = suppress)
    } else {
      stop("This should never happen")
    }


  }

  return( new("SubsetListFit",
              "txInfo" = txInfo,
              "loo" = res) )

}

setMethod(f = ".newOutcomeRegression",
          signature = c(moMain = "ModelObj_SubsetList",
                        moCont = "ModelObj_SubsetList",
                        txInfo = "TxInfoWithSubsets"),
          definition = .subsetMEC )


.subsetME <- function(moMain,
                      moCont,
                      txInfo,
                      data,
                      response,
                      iter,
                      suppress){

  res <- list()

  nms <- names(moMain@loo)

  for( i in 1L:length(moMain) ) {

    res[[ nms[i] ]] <- .newSubsetFit(moMain = moMain[[i]],
                                     moCont = NULL,
                                     data = data,
                                     response = response,
                                     txInfo = txInfo,
                                     iter = iter,
                                     modelSubset = nms[i],
                                     suppress = suppress)
  }

  return( new("SubsetListFit",
              "txInfo" = txInfo,
              "loo" = res) )

}

setMethod(f = ".newOutcomeRegression",
          signature = c(moMain = "ModelObj_SubsetList",
                        moCont = "NULL",
                        txInfo = "TxInfoWithSubsets"),
          definition = .subsetME)

.subsetC <- function(moMain,
                     moCont,
                     txInfo,
                     data,
                     response,
                     iter,
                     suppress){

  res <- list()

  nms <- names(moCont@loo)

  for( i in 1L:length(moCont) ) {

    res[[ nms[i] ]] <- .newSubsetFit(moMain = NULL,
                                     moCont = moCont[[i]],
                                     data = data,
                                     response = response,
                                     txInfo = txInfo,
                                     iter = iter,
                                     modelSubset = nms[i],
                                     suppress = suppress)
  }

  return( new("SubsetListFit",
              "txInfo" = txInfo,
              "loo" = res) )
}

setMethod(f = ".newOutcomeRegression",
          signature = c(moMain = "NULL",
                        moCont = "ModelObj_SubsetList",
                        txInfo = "TxInfoWithSubsets"),
          definition = .subsetC )


.newSubsetFit <- function(moMain,
                          moCont,
                          data,
                          response,
                          txInfo,
                          iter,
                          modelSubset,
                          suppress){

  #------------------------------------------------------------------#
  # Identify all subsets to be included in fitting this model        #
  #------------------------------------------------------------------#
  modelSubset <- unlist(strsplit(modelSubset,","))

  #------------------------------------------------------------------#
  # Retrieve subsets identified by fSet                              #
  #------------------------------------------------------------------#
  fSetSubsets <- .getSubsets(txInfo)

  #------------------------------------------------------------------#
  # Retrieve vector indicating treatment group available to each pt  #
  #------------------------------------------------------------------#
  fSetPtsSubset <- .getPtsSubset(txInfo)

  #------------------------------------------------------------------#
  # Retrieve treatment name                                          #
  #------------------------------------------------------------------#
  txName <- .getTxName(txInfo)

  #------------------------------------------------------------------#
  # Match model subsets to list of subsets defined by fSet           #
  #------------------------------------------------------------------#
  isubset <- which( names(fSetSubsets) %in% modelSubset )

  if( length(isubset) == 0L ) {
    UserError("input",
              paste("unable to match subset",
                    paste(modelSubset,collapse=", "),
                    "to a subset defined by fSet"))
  }

  #------------------------------------------------------------------#
  # Use only patients whose treatment opts match this group          #
  #------------------------------------------------------------------#
  use4fit <- fSetPtsSubset %in% modelSubset

  if( !any(use4fit) ) {
    UserError("input",
              paste("no observations match", modelSubset))
   }

  if( !suppress ) {
    cat("Fitting models for ", paste(modelSubset, collapse=" ,"),
        "using", sum(use4fit), "patient records.\n")
  }

  txTemp <- txInfo
  txTemp@ptsSubset <- txInfo@ptsSubset[use4fit]
  txTemp@singleton <- txInfo@singleton[use4fit]

  #------------------------------------------------------------------#
  # Fit model                                                        #
  #------------------------------------------------------------------#
  fitO <- .newOutcomeRegression(moMain = moMain,
                                moCont = moCont,
                                txInfo = txTemp,
                                data = data[use4fit,,drop=FALSE],
                                response = response[use4fit],
                                iter = iter,
                                suppress = suppress)

  return( fitO )
}

#----------------------------------------------------------------------#
# Q-learning algorithm for multiple dp                                 #
#----------------------------------------------------------------------#
#  params                                                              #
#  moMain   : ModelObj_DecisionPointList for main effects of outcome   #
#  moCont   : ModelObj_DecisionPointList for contrasts of outcome      #
#  txInfo   : TxInfoList                                               #
#  data     : data.frame of covariates and treatment history           #
#  response : outcome of interest                                      #
#  iter     : max iterations in iterative algorithm                    #
#  suppress : T/F indicating if screen prints are suppressed           #
#----------------------------------------------------------------------#
.qLearnDP1 <- function(moMain,
                       moCont,
                       txInfo,
                       data,
                       response,
                       iter,
                       suppress) {
  #------------------------------------------------------------------#
  # The number decision points.                                      #
  #------------------------------------------------------------------#
  nDP <- length(txInfo)

  QfitObj <- list()
  fitObj <- list()

  #------------------------------------------------------------------#
  # Obtain outcome regression for final dp                           #
  #------------------------------------------------------------------#
  QfitObj[[nDP]] <- .newOutcomeRegression(moMain = moMain[[nDP]],
                                          moCont = moCont[[nDP]],
                                          response = response,
                                          txInfo = txInfo[[nDP]],
                                          data = data,
                                          iter = iter,
                                          suppress = suppress)

  vals <- .predictAllTreatments(object = QfitObj[[nDP]],
                                data = data, response = response)

  takeResp <- apply(X = vals$vals, MARGIN = 1L, FUN = function(x){all(is.na(x))})
  hold <- response
  hold[!takeResp] <- apply(X = vals$vals[!takeResp,,drop=FALSE],
                           MARGIN = 1L,
                           FUN = max,
                           na.rm=TRUE)
  response <- hold
  #------------------------------------------------------------------#
  # Iterate backwards to first decision point                        #
  #------------------------------------------------------------------#
  j <- nDP - 1L
  while( j > 0L ) {
    #--------------------------------------------------------------#
    # Obtain outcome regression                                    #
    #--------------------------------------------------------------#
    QfitObj[[j]] <- .newOutcomeRegression(moMain = moMain[[j]],
                                          moCont = moCont[[j]],
                                          response = response,
                                          txInfo = txInfo[[j]],
                                          data = data,
                                          iter = iter,
                                          suppress = suppress)

    vals <- .predictAllTreatments(object = QfitObj[[j]],
                                  data = data, response = response)

    hold <- response
    hold[!takeResp] <- apply(X = vals$vals[!takeResp,,drop=FALSE],
                             MARGIN = 1L,
                             FUN = max,
                             na.rm=TRUE)
    response <- hold

    j <- j -1L
  }

  return(QfitObj)
}

#----------------------------------------------------------------------#
# Q-learning algorithm for multiple dp                                 #
#----------------------------------------------------------------------#
#  params                                                              #
#  moMain   : ModelObj_SubsetList_DecisionPointList for main effects   #
#  moCont   : ModelObj_SubsetList_DecisionPointList for contrasts      #
#  txInfo   : TxInfoList                                               #
#  data     : data.frame of covariates and treatment history           #
#  response : outcome of interest                                      #
#  iter     : max iterations in iterative algorithm                    #
#  suppress : T/F indicating if screen prints are suppressed           #
#----------------------------------------------------------------------#
.qLearnDP <- function(moMain,
                      moCont,
                      txInfo,
                      data,
                      response,
                      iter,
                      suppress, ...) {

  QfitObj <- .qLearnDP1(moMain = moMain,
                        moCont = moCont,
                        txInfo = txInfo,
                        data = data,
                        response = response,
                        iter = iter,
                        suppress = suppress)

  result <- new("OutcomeRegression_DecisionPointList",
                "loo" = QfitObj)

  return(result)

  return(result)

}
setMethod(f = ".newOutcomeRegression",
          signature = c(moMain = "ModelObj_DecisionPointList",
                        moCont = "ModelObj_DecisionPointList",
                        txInfo   = "TxInfoList"),
          definition = .qLearnDP )

setMethod(f = ".newOutcomeRegression",
          signature = c(moMain = "ModelObj_DecisionPointList",
                        moCont = NULL,
                        txInfo   = "TxInfoList"),
          definition = .qLearnDP )

setMethod(f = ".newOutcomeRegression",
          signature = c(moMain = "NULL",
                        moCont = "ModelObj_DecisionPointList",
                        txInfo   = "TxInfoList"),
          definition = .qLearnDP )

#----------------------------------------------------------------------#
# Q-learning algorithm for multiple dp                                 #
#----------------------------------------------------------------------#
#  params                                                              #
#  moMain   : ModelObj_SubsetList_DecisionPointList for main effects   #
#  moCont   : ModelObj_SubsetList_DecisionPointList for contrasts      #
#  txInfo   : TxInfoList                                               #
#  data     : data.frame of covariates and treatment history           #
#  response : outcome of interest                                      #
#  iter     : max iterations in iterative algorithm                    #
#  suppress : T/F indicating if screen prints are suppressed           #
#----------------------------------------------------------------------#
.qLearnSSDP <- function(moMain,
                        moCont,
                        txInfo,
                        data,
                        response,
                        iter,
                        suppress, ...) {


  QfitObj <- .qLearnDP1(moMain = moMain,
                        moCont = moCont,
                        txInfo = txInfo,
                        data = data,
                        response = response,
                        iter = iter,
                        suppress = suppress)

  result <- new("SubsetListFit_DecisionPointList",
                "loo" = QfitObj)

  return(result)

}

setMethod(f = ".newOutcomeRegression",
          signature = c(moMain = "ModelObj_SubsetList_DecisionPointList",
                        moCont = "ModelObj_SubsetList_DecisionPointList",
                        txInfo   = "TxInfoList"),
          definition = .qLearnSSDP )

setMethod(f = ".newOutcomeRegression",
          signature = c(moMain = "ModelObj_SubsetList_DecisionPointList",
                        moCont = NULL,
                        txInfo   = "TxInfoList"),
          definition = .qLearnSSDP )

setMethod(f = ".newOutcomeRegression",
          signature = c(moMain = "NULL",
                        moCont = "ModelObj_SubsetList_DecisionPointList",
                        txInfo   = "TxInfoList"),
          definition = .qLearnSSDP )
