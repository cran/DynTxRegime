setGeneric(name = ".newOptimalSeq", 
           def = function(moPropen, moMain, moCont, regimes, fSet, ...){
                   standardGeneric(".newOptimalSeq")
                 })

#----------------------------------------------------------------------#
# IPWE missing data routine                                            #
#----------------------------------------------------------------------#
#  params                                                              #
#  moPropen : modelObj for propensity                                  #
#  moMain   : missing                                                  #
#  moCont   : missing                                                  #
#  data     : data.frame of covariates and treatment history           #
#  response : outcome of interest                                      #
#  txName   : character name of treatment in data                      #
#  regimes  : regimes to be fit                                        #
#  fSet     : subsetting function                                      #
#  suppress : T/F indicating if screen prints are suppressed           #
#  argList  : arguments to be set in rgenoud                           #
#----------------------------------------------------------------------#
.optimalSeqIPWE_SDP <- function(moPropen, 
                                moMain,
                                moCont,
                                data, 
                                response, 
                                txName, 
                                regimes, 
                                fSet, 
                                suppress, 
                                argsList, ...) {

  if( !suppress ) {
    cat("IPWE. Single Decision Point - Missing Data Perspective.\n")
  }

  #------------------------------------------------------------------#
  # Process treatment and feasibility input                          #
  #------------------------------------------------------------------#
  txInfo <- .newTxInfo(fSet = fSet, 
                       txName = txName, 
                       data = data,
                       suppress = suppress)

  #------------------------------------------------------------------#
  # Fit propensity models                                            #
  #------------------------------------------------------------------#
  propen <- .newPropensityRegression(moPropen = moPropen, 
                                     txInfo = txInfo, 
                                     data = data,
                                     suppress = suppress)

  #------------------------------------------------------------------#
  # create argument list for rgenoud.                                #
  #------------------------------------------------------------------#
  argList <- argsList
  argList[['regimes']] <- regimes
  argList[['txInfo']] <- txInfo
  argList[['l.data']] <- quote(data)
  argList[['propen']] <- propen
  argList[['response']] <- response

  argList[['fn']] <- .seqIPWE_SDP
  argList[['nvars']] <- .getNumPars(regimes)
  argList[['print.level']] <- 0L
  argList[['max']] <- TRUE
  argList[['gradient.check']] <- FALSE
  argList[['BFGS']] <- FALSE
  argList[['P9']] <- 0L
  argList[['optim.method']] <- "Nelder-Mead"

  #------------------------------------------------------------------#
  # Initiate genetic algorithm                                       #
  #------------------------------------------------------------------#
  gaEst <- do.call(rgenoud::genoud, argList)

  if( !suppress ) {
    cat("Genetic Algorithm\n")
    print(gaEst)
  }

  regimes <- .setPars(regimes, gaEst$par)

  optTx <- .predictOptimalTx(regimes, data)
  optTx <- .convertTx(txInfo, optTx)

  result <- new("OptimalSeqIPWE_SDP",
                "genetic"        = gaEst,
                "propen"         = propen,
                "regime"         = regimes,
                "estimatedValue" = gaEst$value,
                "optimalTx"      = optTx,
                "call"           = NULL)

  return( result )

}
setMethod(f = ".newOptimalSeq",    
          signature = c(moPropen = "modelObj",
                        moMain   = "NULL",
                        moCont   = "NULL",
                        regimes  = "Regime",
                        fSet     = "NULL"), 
          definition = .optimalSeqIPWE_SDP)

setMethod(f = ".newOptimalSeq",    
          signature = c(moPropen = "modelObj",
                        moMain   = "NULL",
                        moCont   = "NULL",
                        regimes  = "Regime",
                        fSet     = "function"), 
          definition = .optimalSeqIPWE_SDP)

setMethod(f = ".newOptimalSeq",    
          signature = c(moPropen = "ModelObj_SubsetList",
                        moMain   = "NULL",
                        moCont   = "NULL",
                        regimes  = "Regime",
                        fSet     = "function"), 
          definition = .optimalSeqIPWE_SDP)
#----------------------------------------------------------------------#
# objective function to be minimized                                   #
#----------------------------------------------------------------------#
#  params                                                              #
#   eta      : estimated regime parameters                             #
#   regimes  : definition of class of regimes                          #
#   l.data   : data.frame of covariates and treatment history          #
#   propen   : propensity for treatment predictions                    #
#   response : outcome of interest                                     #
#----------------------------------------------------------------------#
.seqIPWE_SDP <- function(eta, 
                         regimes, 
                         txInfo, 
                         l.data, 
                         propen, 
                         response){

  #------------------------------------------------------------------#
  # Store parameter estimates in Regimes object                      #
  #------------------------------------------------------------------#
  regimes <- .setPars(regimes, eta)

  nSamples <- nrow(l.data)

  #------------------------------------------------------------------#
  # Predict optimal treatment at current regime parameter estimates  #
  #------------------------------------------------------------------#
  optTx <- .predictOptimalTx(regimes, l.data)

  #------------------------------------------------------------------#
  # Convert to appropriate class factor or integer                   #
  #------------------------------------------------------------------#
  optTx <- .convertTx(txInfo, optTx)

  #------------------------------------------------------------------#
  # Retrieve treatment variable column header                        #
  #------------------------------------------------------------------#
  txName <- .getTxName(txInfo)

  #------------------------------------------------------------------#
  # Verify that value returned by regime is allowed by fSet          #
  #------------------------------------------------------------------#
  .validTx(txInfo, optTx)

  #------------------------------------------------------------------#
  # ind = 1 if patient tx in accordance with regime I(A = g)         #
  #     = 0 if patient tx not in accordance with regime I(A != g)    #
  #------------------------------------------------------------------#
  ind <- .compare(txInfo, l.data[,txName], optTx)
  #------------------------------------------------------------------#
  # Calculate propensity for treatment                               #
  #------------------------------------------------------------------#
  mm <- predict(object = propen, newdata = l.data)

  #------------------------------------------------------------------#
  # Retrieve probability being assigned optimal treatment            #
  #------------------------------------------------------------------#
  func1 <- function(i, tx){ 
             if( is.na(tx[i]) ) {
               return( 0.0 ) 
             } else {
               return( mm[i,as.character(tx[i])] )
             }
           }

  pc <- sapply(X = 1L:nSamples, FUN = func1, tx = optTx)

  #------------------------------------------------------------------#
  # For those that have only 1 treatment, probability is 1           #
  #------------------------------------------------------------------#
  pc[is.na(pc)] <- 1.0

  #------------------------------------------------------------------#
  #     (     C_eta    )                                             #
  # mean|   -------- Y |                                             #
  #     (     pi_c     )                                             #
  #------------------------------------------------------------------#
  mn <- sum( ind / pc * as.vector(response), na.rm = TRUE) / nSamples

  return(mn)
}


#----------------------------------------------------------------------#
# AIPWE missing data routine                                           #
#----------------------------------------------------------------------#
#  params                                                              #
#  moPropen : modelObj for propensity                                  #
#  moMain   : modelObj for main effects of outcome                     #
#  moCont   : modelObj for contrasts of outcome                        #
#  data     : data.frame of covariates and treatment history           #
#  response : outcome of interest                                      #
#  txName   : character name of treatment in data                      #
#  regimes  : regimes to be fit                                        #
#  fSet     : subsetting function                                      #
#  suppress : T/F indicating if screen prints are suppressed           #
#  argList  : arguments to be set in rgenoud                           #
#----------------------------------------------------------------------#
.optimalSeqAIPWE_SDP <- function(moPropen, 
                                 moMain, 
                                 moCont, 
                                 data,
                                 response, 
                                 txName, 
                                 regimes, 
                                 fSet, 
                                 iter, 
                                 suppress, 
                                 argsList, ...){

  if( !suppress ) {
    cat("AIPWE. Single Decision Point - Missing Data Perspective.\n")
  }

  #------------------------------------------------------------------#
  # Process treatment and feasibility input                          #
  #------------------------------------------------------------------#
  txInfo <- .newTxInfo(fSet = fSet, 
                       txName = txName, 
                       data = data,
                       suppress = suppress)

  #------------------------------------------------------------------#
  # Fit propensity models                                            #
  #------------------------------------------------------------------#
  propen <- .newPropensityRegression(moPropen = moPropen, 
                                     txInfo = txInfo, 
                                     data = data,
                                     suppress = suppress)

  #------------------------------------------------------------------#
  # Fit outcome regression models                                    #
  #------------------------------------------------------------------#
  outcome <- .newOutcomeRegression(moMain = moMain, 
                                   moCont = moCont,  
                                   data = data,  
                                   response = response,  
                                   txInfo = txInfo,  
                                   iter = iter,
                                   suppress = suppress)

  #------------------------------------------------------------------#
  # create argument list for rgenoud.                                #
  #------------------------------------------------------------------#
  argList <- argsList
  argList[['regimes']] <- regimes
  argList[['txInfo']] <- txInfo
  argList[['l.data']] <- quote(data)
  argList[['propen']] <- propen

  argList[['outcome']] <- outcome
  argList[['response']] <- response

  argList[['fn']] <- .aipwe_SDP
  argList[['nvars']] <- .getNumPars(regimes)
  argList[['print.level']] <- 0L
  argList[['max']] <- TRUE
  argList[['gradient.check']] <- FALSE
  argList[['BFGS']] <- FALSE
  argList[['P9']] <- 0L
  argList[['optim.method']] <- "Nelder-Mead"

  #------------------------------------------------------------------#
  # Initiate genetic algorithm                                       #
  #------------------------------------------------------------------#
  gaEst <- do.call(rgenoud::genoud, argList)

  if( !suppress ) {
    cat("Genetic Algorithm\n")
    print(gaEst)
  }

  regimes <- .setPars(regimes, gaEst$par)

  optTx <- .predictOptimalTx(regimes, data)
  optTx <- .convertTx(txInfo, optTx)

  result <- new("OptimalSeqAIPWE_SDP",
                "genetic"        = gaEst,
                "propen"         = propen,
                "outcome"        = outcome,
                "regime"         = regimes,
                "estimatedValue" = gaEst$value,
                "optimalTx"      = optTx,
                "call"           = NULL)

  return( result )

}

setMethod(f = ".newOptimalSeq", 
          signature = c(moPropen = "modelObj",
                        moMain   = "modelObj",
                        moCont   = "modelObj",
                        regimes  = "Regime",
                        fSet     = "NULL"), 
          definition = .optimalSeqAIPWE_SDP)

setMethod(f = ".newOptimalSeq", 
          signature = c(moPropen = "modelObj",
                        moMain   = "modelObj",
                        moCont   = "NULL",
                        regimes  = "Regime",
                        fSet     = "NULL"), 
          definition = .optimalSeqAIPWE_SDP)

setMethod(f = ".newOptimalSeq",    
          signature = c(moPropen = "modelObj",
                        moMain   = "NULL",
                        moCont   = "modelObj",
                        regimes  = "Regime",
                        fSet     = "NULL"), 
          definition = .optimalSeqAIPWE_SDP)

setMethod(f = ".newOptimalSeq", 
          signature = c(moPropen = "modelObj",
                        moMain   = "modelObj",
                        moCont   = "modelObj",
                        regimes  = "Regime",
                        fSet     = "function"), 
          definition = .optimalSeqAIPWE_SDP)

setMethod(f = ".newOptimalSeq", 
          signature = c(moPropen = "modelObj",
                        moMain   = "modelObj",
                        moCont   = "NULL",
                        regimes  = "Regime",
                        fSet     = "function"), 
          definition = .optimalSeqAIPWE_SDP)

setMethod(f = ".newOptimalSeq",    
          signature = c(moPropen = "modelObj",
                        moMain   = "NULL",
                        moCont   = "modelObj",
                        regimes  = "Regime",
                        fSet     = "function"), 
          definition = .optimalSeqAIPWE_SDP)

setMethod(f = ".newOptimalSeq", 
          signature = c(moPropen = "ModelObj_SubsetList",
                        moMain   = "modelObj",
                        moCont   = "modelObj",
                        regimes  = "Regime",
                        fSet     = "function"), 
          definition = .optimalSeqAIPWE_SDP)

setMethod(f = ".newOptimalSeq", 
          signature = c(moPropen = "ModelObj_SubsetList",
                        moMain   = "modelObj",
                        moCont   = "NULL",
                        regimes  = "Regime",
                        fSet     = "function"), 
          definition = .optimalSeqAIPWE_SDP)

setMethod(f = ".newOptimalSeq",    
          signature = c(moPropen = "ModelObj_SubsetList",
                        moMain   = "NULL",
                        moCont   = "modelObj",
                        regimes  = "Regime",
                        fSet     = "function"), 
          definition = .optimalSeqAIPWE_SDP)

setMethod(f = ".newOptimalSeq", 
          signature = c(moPropen = "ModelObj_SubsetList",
                        moMain   = "ModelObj_SubsetList",
                        moCont   = "ModelObj_SubsetList",
                        regimes  = "Regime",
                        fSet     = "function"), 
          definition = .optimalSeqAIPWE_SDP)

setMethod(f = ".newOptimalSeq", 
          signature = c(moPropen = "ModelObj_SubsetList",
                        moMain   = "ModelObj_SubsetList",
                        moCont   = "NULL",
                        regimes  = "Regime",
                        fSet     = "function"), 
          definition = .optimalSeqAIPWE_SDP)

setMethod(f = ".newOptimalSeq",    
          signature = c(moPropen = "ModelObj_SubsetList",
                        moMain   = "NULL",
                        moCont   = "ModelObj_SubsetList",
                        regimes  = "Regime",
                        fSet     = "function"), 
          definition = .optimalSeqAIPWE_SDP)

setMethod(f = ".newOptimalSeq", 
          signature = c(moPropen = "modelObj",
                        moMain   = "ModelObj_SubsetList",
                        moCont   = "ModelObj_SubsetList",
                        regimes  = "Regime",
                        fSet     = "function"), 
          definition = .optimalSeqAIPWE_SDP)

setMethod(f = ".newOptimalSeq", 
          signature = c(moPropen = "modelObj",
                        moMain   = "ModelObj_SubsetList",
                        moCont   = "NULL",
                        regimes  = "Regime",
                        fSet     = "function"), 
          definition = .optimalSeqAIPWE_SDP)

setMethod(f = ".newOptimalSeq",    
          signature = c(moPropen = "modelObj",
                        moMain   = "NULL",
                        moCont   = "ModelObj_SubsetList",
                        regimes  = "Regime",
                        fSet     = "function"), 
          definition = .optimalSeqAIPWE_SDP)

#----------------------------------------------------------------------#
# Objective function for AIPWE                                         #
# called only by rgenoud method for sequential DTR method              #
#----------------------------------------------------------------------#
# eta     : array of current parameters estimates for tx regime.       #
#                                                                      #
# regimes : an object of class Regime                                  #
#           a function defining the tx rule.                           #
#           For example, if the tx rule is : I(eta_1 < x1),            #
#           regimes is defined as                                      #
#             regimes <- function(a,data){as.numeric(a < data$x1)}     #
#           THE LAST ARGUMENT IS ALWAYS TAKEN TO BE THE DATA.FRAME     #
#                                                                      #
# txInfo  : an object of class TxInfo or TxInfoList                    #
#                                                                      #
# l.data  : data.frame of covariates and tx histories                  #
#                                                                      #
# outcome : an object of class QLearnEst or QLearnEstList.             #
#                                                                      #
# propen  : an object of class PropenFit or PropenFitList              #
#                                                                      #
# response: Response vector                                            #
#                                                                      #
# base    : An integer indicating the base tx or NULL (ordinal tx)     #
#                                                                      #
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#
.aipwe_SDP <- function(eta, regimes, txInfo, l.data,
                       outcome, propen, response){

  #------------------------------------------------------------------#
  # Store parameter estimates in Regimes object                      #
  #------------------------------------------------------------------#
  regimes <- .setPars(regimes, eta)

  nSamples <- nrow(l.data)

  #------------------------------------------------------------------#
  # For the regime parameter obtain optimal                          #
  #------------------------------------------------------------------#
  optTx <- .predictOptimalTx(regimes, l.data)

  #------------------------------------------------------------------#
  # Convert to appropriate class                                     #
  #------------------------------------------------------------------#
  optTx <- .convertTx(txInfo, optTx)

  #------------------------------------------------------------------#
  # Retrieve treatment variable name                                 #
  #------------------------------------------------------------------#
  txName <- .getTxName(txInfo)

  #------------------------------------------------------------------#
  # Verify that value returned by regime is allowed by fSet          #
  #------------------------------------------------------------------#
  tst <- try(.validTx(txInfo, optTx), silent=FALSE)
  if( is(tst, "try-error") ) stop("verify input 'regimes'")

  #------------------------------------------------------------------#
  # ind[,i] = 1 if patient tx in accordance with regime              #
  #         = 0 if patient tx not in accordance with regime          #
  #------------------------------------------------------------------#
  ind <- .compare(txInfo, l.data[,txName], optTx)

  #------------------------------------------------------------------#
  # Calculate propensity for treatment for subset of patients        #
  #------------------------------------------------------------------#
  mm <- predict(object = propen, newdata = l.data)

  #------------------------------------------------------------------#
  # Retrieve probability of treatment being assigned treatment       #
  #------------------------------------------------------------------#
  func1 <- function(i, tx){ 
             if( is.na(tx[i]) ) {
               return( 0.0 ) 
             } else {
               return( mm[i,as.character(tx[i])] )
             }
           }

  pc <- sapply(X = 1L:nSamples, FUN = func1, tx = optTx)

  #------------------------------------------------------------------#
  # For those that have only 1 treatment, probability is 1           #
  #------------------------------------------------------------------#
  pc[is.na(pc)] <- 1.0

  #------------------------------------------------------------------#
  # doubly robust estimator.                                         #
  #------------------------------------------------------------------#
  l.data[,txName] <- optTx

  #------------------------------------------------------------------#
  # Calculate value function using optimal tx set by regime          #
  #------------------------------------------------------------------#
  valueFunc <- predict(object = outcome, newdata = l.data)

  #------------------------------------------------------------------#
  #          C_eta - pi_c                                            #
  # DR = -  ---------------  mu_i                                    #
  #             pi_c                                                 #
  #------------------------------------------------------------------#
  DR <- - {ind - pc} / pc * valueFunc

  #------------------------------------------------------------------#
  #     (   C_eta        )                                           #
  # mean|   ------Y + DR |                                           #
  #     (   pi_c         )                                           #
  #------------------------------------------------------------------#
  mn <- sum(DR + ind / pc * response, na.rm = TRUE) / nSamples

  return(mn)
}

#----------------------------------------------------------------------#
# Sequential optimal treatment regime algorithm                        #
#----------------------------------------------------------------------#
# moPropen : ModelObj_DecisionPointList or                             #
#            ModelObj_SubsetList_DecisionPointList                     #
# moMain   : NULL                                                      #
# moCont   : NULL                                                      #
# data     : data.frame                                                #
# response : vector                                                    #
# txName   : character vector                                          #
# regimes  : Regime_DecisionPointList                                  #
# fSet     : list of functions or NULL                                 #
# suppress : T/F                                                       #
# argsList : list of arguments to be passed to genoud                  #
#----------------------------------------------------------------------#
.optimalSeqIPWE_MDP <- function(moPropen, 
                                moMain,
                                moCont,
                                data, 
                                response, 
                                txName, 
                                regimes, 
                                fSet, 
                                suppress, 
                                argsList, ...) {

  #------------------------------------------------------------------#
  # Number of decision points                                        #
  #------------------------------------------------------------------#
  nDP <- length(txName)

  if( !suppress ) {
    cat("IPWE.", nDP, "Decision Points - Coarsened Data Perspective.\n")
  }

  #------------------------------------------------------------------#
  # Process treatment and feasibility input                          #
  #------------------------------------------------------------------#
  txInfo <- .newTxInfo(fSet = fSet, 
                       txName = as.list(txName), 
                       data = data,
                       suppress = suppress)

  #------------------------------------------------------------------#
  # Fit propensity models                                            #
  #------------------------------------------------------------------#
  propen <- .newPropensityRegression(moPropen = moPropen, 
                                     txInfo = txInfo, 
                                     data = data,
                                     suppress = suppress)

  #------------------------------------------------------------------#
  # create argument list for call to genoud.                         #
  #------------------------------------------------------------------#
  if( !suppress ) cat("Initiating genetic algorithm\n.")
  argsList[['regimes']] <- regimes
  argsList[['txInfo']] <- txInfo
  argsList[['l.data']] <- quote(data)
  argsList[['propen']] <- propen
  argsList[['response']] <- response


  argsList[['fn']] <- .seqIPWE_MDP
  argsList[['nvars']] <- .getNumPars(regimes)
  if( suppress ) argsList[['print.level']] <- 0L
  argsList[['max']] <- TRUE
  argsList[['gradient.check']] <- FALSE
  argsList[['BFGS']] <- FALSE
  argsList[['P9']] <- 0L
  argsList[['optim.method']] <- "Nelder-Mead"

  #------------------------------------------------------------------#
  # Initiate genetic algorithm                                       #
  #------------------------------------------------------------------#
  gaEst <- do.call(rgenoud::genoud, argsList)

  if( !suppress ) {
    cat("Genetic Algorithm\n")
    print(gaEst)
  }

  #------------------------------------------------------------------#
  # Store estimated parameters in regime objects                     #
  #------------------------------------------------------------------#
  regimes <- .setPars(regimes, gaEst$par)

  #------------------------------------------------------------------#
  # Estimate optimal treatment at each stage.                        #
  #------------------------------------------------------------------#
  optTx <- NULL
  for( i in 1L:nDP ) {
    ot <- .predictOptimalTx(regimes, data, dp=i)
    optTx <- cbind(optTx, .convertTx(txInfo[[i]], ot))
    data[,.getTxName(txInfo[[i]])] <- optTx
  }

  nms <- paste("dp=",1L:nDP,sep="")
  colnames(optTx) <- nms

  result <- new("OptimalSeqIPWE_MDP", 
                "genetic"        = gaEst,
                "propen"         = propen,
                "regime"         = regimes,
                "estimatedValue" = gaEst$value,
                "optimalTx"      = optTx)

  return(result)

}

setMethod(f = ".newOptimalSeq",    
          signature = c(moPropen = "ModelObj_DecisionPointList",
                        moMain   = "NULL",
                        moCont   = "NULL",
                        regimes  = "Regime_DecisionPointList",
                        fSet     = "list"), 
          definition = .optimalSeqIPWE_MDP)

setMethod(f = ".newOptimalSeq",    
          signature = c(moPropen = "ModelObj_DecisionPointList",
                        moMain   = "NULL",
                        moCont   = "NULL",
                        regimes  = "Regime_DecisionPointList",
                        fSet     = "NULL"), 
          definition = .optimalSeqIPWE_MDP)

setMethod(f = ".newOptimalSeq",    
          signature = c(moPropen = "ModelObj_SubsetList_DecisionPointList",
                        moMain   = "NULL",
                        moCont   = "NULL",
                        regimes  = "Regime_DecisionPointList",
                        fSet     = "list"), 
          definition = .optimalSeqIPWE_MDP)

#----------------------------------------------------------------------#
# Function to be minimized                                             #
#----------------------------------------------------------------------#
# eta      : current regime parameter estimates                        #
# regimes  : Regime_DecisionPointList                                  #
# txInfo   : TxInfo object                                             #
# l.data   : data.frame of treatment history and covariates            #
# propen   : Propensity_DecisionPointList object                       #
# response : vector                                                    #
#----------------------------------------------------------------------#
.seqIPWE_MDP <- function(eta, regimes, txInfo, l.data,
                         propen, response){

  #------------------------------------------------------------------#
  # set parameter estimates in regime objects.                       #
  #------------------------------------------------------------------#
  regimes <- .setPars(regimes, eta)

  #------------------------------------------------------------------#
  # Number of patients in training data.                             #
  #------------------------------------------------------------------#
  nSamples <- nrow(l.data)

  #------------------------------------------------------------------#
  # Number of decision points.                                       #
  #------------------------------------------------------------------#
  nDP <- length(regimes@loo)

  #------------------------------------------------------------------#
  # ind indicates if patient tx is in accordance with regime at dp i.#
  #------------------------------------------------------------------#
  ind <- matrix(data = 0L, nrow = nSamples, ncol = nDP)

  #------------------------------------------------------------------#
  # lambda : Probability that the tx does not follow regime.         #
  #          Pr(A_k != g_i)                                          #
  #------------------------------------------------------------------#
  lambda <- matrix(data = 0.0, nrow = nSamples, ncol = nDP)

  #------------------------------------------------------------------#
  # Obtain probabilities, indicators, and fits                       #
  #------------------------------------------------------------------#
#  for( i in nDP:1L ) {
  for( i in 1L:nDP ){
    #--------------------------------------------------------------#
    # predict optimal treatment using current parameter estimates  #
    #--------------------------------------------------------------#
    optTx <- .predictOptimalTx(regimes@loo[[i]], l.data)
    optTx <- .convertTx(txInfo[[i]], optTx)

    #--------------------------------------------------------------#
    # Verify that value returned by regime is allowed by fSet      #
    #--------------------------------------------------------------#
#    .validTx(txInfo[[i]], optTx)

    #--------------------------------------------------------------#
    # Retrieve treatment variable column header                    #
    #--------------------------------------------------------------#
    txName <- .getTxName(txInfo[[i]])

    #--------------------------------------------------------------#
    # ind[,i] = 1 if patient tx in accordance with regime at dp i. #
    #           I(A_i = g_i)                                       #
    #         = 0 if pt tx not in accordance with regime @ dp i    #
    #           I(A_i != g_i)                                      #
    #--------------------------------------------------------------#
    ind[,i] <- .compare(txInfo[[i]], l.data[,txName], optTx)

    #--------------------------------------------------------------#
    # Change ith tx for all patients current regime                #
    #--------------------------------------------------------------#
    l.data[,txName] <- optTx

  }

  for( i in 1L:nDP ) {
    #--------------------------------------------------------------#
    # Calculate propensity for treatment                           #
    #--------------------------------------------------------------#
    mm <- predict(object = propen@loo[[i]], newdata = l.data)

    #--------------------------------------------------------------#
    # Retrieve treatment variable column header.                   #
    #--------------------------------------------------------------#
    txName <- .getTxName(txInfo[[i]])

    #--------------------------------------------------------------#
    # Retrieve probability of patient being assigned opt tx        #
    #--------------------------------------------------------------#
    func1 <- function(i, tx, superSet){ 
               if( is.na(tx[i]) ) {
                 return( 0.0 ) 
               } else {
                 tst <- tx[i] == superSet
                 return( mm[i,tst] )
               }
             }
    superSet <- .getSuperSet(txInfo[[i]])

    probOfG <- sapply(X = 1L:nSamples, 
                      FUN = func1, 
                      tx = l.data[,txName],
                      superSet = superSet)

    #--------------------------------------------------------------#
    # lambda is the probability of not being assigned opt Tx       #
    #--------------------------------------------------------------#
    lambda[,i] <- 1.0 - probOfG

  }

  #------------------------------------------------------------------#
  # AC = 1 if all txs given to a patient follow the regime.          #
  #        I(C_{eta} = infinity)                                     #
  #    = 0 otherwise. I(C_{eta} <= K)                                #
  #------------------------------------------------------------------#
  AC <- apply(X = ind, MARGIN = 1L, FUN = prod)

  #------------------------------------------------------------------#
  # pc = probability that coarsening occurs at a later dp.           #
  #   Pr(C_{et} > i) = prod_{k=1}^{i} (Pr(A_k=g_k))                  #
  #                  = prod_{k=1}^{i} (1-Pr(A_k!=g_k))               #
  #------------------------------------------------------------------#
  pc <- apply(X = {1.0-lambda}, MARGIN = 1L, FUN = prod)

  #------------------------------------------------------------------#
  #     (   I(C_{eta} = infinity)        )                           #
  # mean|   --------------------- Y +    |                           #
  #     (      Pr(C_{eta} > K)           )                           #
  #------------------------------------------------------------------#
  mn <- sum( AC / pc * response, na.rm = TRUE) / nSamples

  return(mn)
}


#----------------------------------------------------------------------#
# Sequential optimal treatment regime algorithm                        #
#----------------------------------------------------------------------#
# moPropen : ModelObj_DecisionPointList or                             #
#            ModelObj_SubsetList_DecisionPointList                     #
# moMain   : ModelObj_DecisionPointList or                             #
#            ModelObj_SubsetList_DecisionPointList                     #
# moCont   : ModelObj_DecisionPointList or                             #
#            ModelObj_SubsetList_DecisionPointList                     #
# data     : data.frame                                                #
# response : vector                                                    #
# txName   : character vector                                          #
# regimes  : Regime_DecisionPointList                                  #
# fSet     : list of functions or NULL                                 #
# iter     : integer                                                   #
# suppress : T/F                                                       #
# argsList : list of arguments to be passed to genoud                  #
#----------------------------------------------------------------------#
.OptimalSeqAIPWE_MDP <- function(moPropen, 
                                 moMain, 
                                 moCont, 
                                 data,
                                 response, 
                                 txName, 
                                 regimes, 
                                 fSet, 
                                 iter, 
                                 suppress, 
                                 argsList, ...){

  #------------------------------------------------------------------#
  # Number of decision points                                        #
  #------------------------------------------------------------------#
  nDP <- length(txName)

  if( !suppress ) {
    cat("AIPWE.", nDP, "Decision Points - Coarsened Data Perspective.\n")
  }

  #------------------------------------------------------------------#
  # Process treatment and feasibility input                          #
  #------------------------------------------------------------------#
  txInfo <- .newTxInfo(fSet = fSet, 
                       txName = as.list(txName), 
                       data = data,
                       suppress = suppress)

  #------------------------------------------------------------------#
  # Fit propensity models                                            #
  #------------------------------------------------------------------#
  propen <- .newPropensityRegression(moPropen = moPropen, 
                                     txInfo = txInfo, 
                                     data = data,
                                     suppress = suppress)

  #------------------------------------------------------------------#
  # Fit outcome models                                               #
  #------------------------------------------------------------------#
  outcome <- .newOutcomeRegression(moMain = moMain, 
                                   moCont = moCont,  
                                   data = data,  
                                   response = response,  
                                   txInfo = txInfo,  
                                   iter = iter,
                                   suppress = suppress)

  #------------------------------------------------------------------#
  # create argument list for call to genoud.                         #
  #------------------------------------------------------------------#
  if( !suppress ) cat("Initiating genetic algorithm\n.")
  argsList[['regimes']] <- regimes
  argsList[['txInfo']] <- txInfo
  argsList[['l.data']] <- quote(data)
  argsList[['propen']] <- propen

  argsList[['outcome']] <- outcome
  argsList[['response']] <- response

  argsList[['fn']] <- .seqAIPWE_MPD
  argsList[['nvars']] <- .getNumPars(regimes)
  if( suppress ) argsList[['print.level']] <- 0L
  argsList[['max']] <- TRUE
  argsList[['gradient.check']] <- FALSE
  argsList[['BFGS']] <- FALSE
  argsList[['P9']] <- 0L
  argsList[['optim.method']] <- "Nelder-Mead"

  #------------------------------------------------------------------#
  # Initiate genetic algorithm                                       #
  #------------------------------------------------------------------#
  gaEst <- do.call(rgenoud::genoud, argsList)

  if( !suppress ) {
    cat("Genetic Algorithm\n")
    print(gaEst)
  }

  #------------------------------------------------------------------#
  # Store estimated parameters in regime objects                     #
  #------------------------------------------------------------------#
  regimes <- .setPars(regimes, gaEst$par)

  #------------------------------------------------------------------#
  # Estimate optimal treatment at each stage.                        #
  #------------------------------------------------------------------#
  optTx <- NULL
  for( i in 1L:nDP ) {
    ot <- .predictOptimalTx(regimes, data, dp=i)
    optTx <- cbind(optTx, .convertTx(txInfo[[i]], ot))
    data[,.getTxName(txInfo[[i]])] <- optTx
  }

  nms <- paste("dp=",1L:nDP,sep="")
  colnames(optTx) <- nms

  result <- new("OptimalSeqAIPWE_MDP", 
                "genetic"        = gaEst,
                "propen"         = propen,
                "outcome"        = outcome,
                "regime"        = regimes,
                "estimatedValue" = gaEst$value,
                "optimalTx"      = optTx)

}

setMethod(f = ".newOptimalSeq",    
          signature = c(moPropen = "ModelObj_DecisionPointList",
                        moMain   = "ModelObj_DecisionPointList",
                        moCont   = "ModelObj_DecisionPointList",
                        regimes  = "Regime_DecisionPointList",
                        fSet     = "list"), 
          definition = .OptimalSeqAIPWE_MDP)

setMethod(f = ".newOptimalSeq",    
          signature = c(moPropen = "ModelObj_DecisionPointList",
                        moMain   = "ModelObj_DecisionPointList",
                        moCont   = "ModelObj_DecisionPointList",
                        regimes  = "Regime_DecisionPointList",
                        fSet     = "NULL"), 
          definition = .OptimalSeqAIPWE_MDP)

setMethod(f = ".newOptimalSeq",    
          signature = c(moPropen = "ModelObj_DecisionPointList",
                        moMain   = "ModelObj_DecisionPointList",
                        moCont   = "NULL",
                        regimes  = "Regime_DecisionPointList",
                        fSet     = "list"), 
          definition = .OptimalSeqAIPWE_MDP)

setMethod(f = ".newOptimalSeq",    
          signature = c(moPropen = "ModelObj_DecisionPointList",
                        moMain   = "ModelObj_DecisionPointList",
                        moCont   = "NULL",
                        regimes  = "Regime_DecisionPointList",
                        fSet     = "NULL"), 
          definition = .OptimalSeqAIPWE_MDP)

setMethod(f = ".newOptimalSeq",    
          signature = c(moPropen = "ModelObj_DecisionPointList",
                        moMain   = "NULL",
                        moCont   = "ModelObj_DecisionPointList",
                        regimes  = "Regime_DecisionPointList",
                        fSet     = "list"), 
          definition = .OptimalSeqAIPWE_MDP)

setMethod(f = ".newOptimalSeq",    
          signature = c(moPropen = "ModelObj_DecisionPointList",
                        moMain   = "NULL",
                        moCont   = "ModelObj_DecisionPointList",
                        regimes  = "Regime_DecisionPointList",
                        fSet     = "NULL"), 
          definition = .OptimalSeqAIPWE_MDP)

setMethod(f = ".newOptimalSeq",    
          signature = c(moPropen = "ModelObj_SubsetList_DecisionPointList",
                        moMain   = "ModelObj_DecisionPointList",
                        moCont   = "ModelObj_DecisionPointList",
                        regimes  = "Regime_DecisionPointList",
                        fSet     = "list"), 
          definition = .OptimalSeqAIPWE_MDP)

setMethod(f = ".newOptimalSeq",    
          signature = c(moPropen = "ModelObj_SubsetList_DecisionPointList",
                        moMain   = "ModelObj_DecisionPointList",
                        moCont   = "NULL",
                        regimes  = "Regime_DecisionPointList",
                        fSet     = "list"), 
          definition = .OptimalSeqAIPWE_MDP)

setMethod(f = ".newOptimalSeq",    
          signature = c(moPropen = "ModelObj_SubsetList_DecisionPointList",
                        moMain   = "NULL",
                        moCont   = "ModelObj_DecisionPointList",
                        regimes  = "Regime_DecisionPointList",
                        fSet     = "list"), 
          definition = .OptimalSeqAIPWE_MDP)

setMethod(f = ".newOptimalSeq",    
          signature = c(moPropen = "ModelObj_SubsetList_DecisionPointList",
                        moMain   = "ModelObj_SubsetList_DecisionPointList",
                        moCont   = "ModelObj_SubsetList_DecisionPointList",
                        regimes  = "Regime_DecisionPointList",
                        fSet     = "list"), 
          definition = .OptimalSeqAIPWE_MDP)

setMethod(f = ".newOptimalSeq",    
          signature = c(moPropen = "ModelObj_SubsetList_DecisionPointList",
                        moMain   = "ModelObj_SubsetList_DecisionPointList",
                        moCont   = "NULL",
                        regimes  = "Regime_DecisionPointList",
                        fSet     = "list"), 
          definition = .OptimalSeqAIPWE_MDP)

setMethod(f = ".newOptimalSeq",    
          signature = c(moPropen = "ModelObj_SubsetList_DecisionPointList",
                        moMain   = "NULL",
                        moCont   = "ModelObj_SubsetList_DecisionPointList",
                        regimes  = "Regime_DecisionPointList",
                        fSet     = "list"), 
          definition = .OptimalSeqAIPWE_MDP)

setMethod(f = ".newOptimalSeq",    
          signature = c(moPropen = "ModelObj_DecisionPointList",
                        moMain   = "ModelObj_SubsetList_DecisionPointList",
                        moCont   = "ModelObj_SubsetList_DecisionPointList",
                        regimes  = "Regime_DecisionPointList",
                        fSet     = "list"), 
          definition = .OptimalSeqAIPWE_MDP)

setMethod(f = ".newOptimalSeq",    
          signature = c(moPropen = "ModelObj_DecisionPointList",
                        moMain   = "ModelObj_SubsetList_DecisionPointList",
                        moCont   = "NULL",
                        regimes  = "Regime_DecisionPointList",
                        fSet     = "list"), 
          definition = .OptimalSeqAIPWE_MDP)

setMethod(f = ".newOptimalSeq",    
          signature = c(moPropen = "ModelObj_DecisionPointList",
                        moMain   = "NULL",
                        moCont   = "ModelObj_SubsetList_DecisionPointList",
                        regimes  = "Regime_DecisionPointList",
                        fSet     = "list"), 
          definition = .OptimalSeqAIPWE_MDP)

#----------------------------------------------------------------------#
# Function to be minimized                                             #
#----------------------------------------------------------------------#
# eta      : current regime parameter estimates                        #
# regimes  : Regime_DecisionPointList                                  #
# txInfo   : TxInfo object                                             #
# l.data   : data.frame of treatment history and covariates            #
# outcome  : OutcomeRegressionAllTypes_DecisionPointList               #
# propen   : Propensity_DecisionPointList object                       #
# response : vector                                                    #
#----------------------------------------------------------------------#
.seqAIPWE_MPD_WithSubsets <- function(eta, 
                                      regimes, 
                                      txInfo, 
                                      l.data,
                                      outcome, 
                                      propen, 
                                      response) {

  #------------------------------------------------------------------#
  # set parameter estimates in regime objects.                       #
  #------------------------------------------------------------------#
  regimes <- .setPars(regimes, eta)

  #------------------------------------------------------------------#
  # Number of patients in training data.                             #
  #------------------------------------------------------------------#
  nSamples <- nrow(l.data)

  #------------------------------------------------------------------#
  # Number of decision points.                                       #
  #------------------------------------------------------------------#
  nDP <- length(regimes)

  #------------------------------------------------------------------#
  # ind : 0/1 patient tx is not/is in accordance with regime at dp i.#
  #------------------------------------------------------------------#
  ind <- matrix(data = 0L, nrow = nSamples, ncol = nDP)

  #------------------------------------------------------------------#
  # lambda : Probability that the tx does not follow regime.         #
  #          Pr(A_k != g_i)                                          #
  #------------------------------------------------------------------#
  lambda <- matrix(data = 0.0, nrow = nSamples, ncol = nDP)

  #------------------------------------------------------------------#
  # valueFunc : outcome regression at each dp                        #
  #------------------------------------------------------------------#
  valueFunc <- matrix(data = 0.0, nrow = nSamples, ncol = nDP)

  #------------------------------------------------------------------#
  # Obtain probabilities, indicators & fits                          #
  #------------------------------------------------------------------#
#  for( i in nDP:1L ){
  for( i in 1L:nDP ){
    #--------------------------------------------------------------#
    # Predict optimal treatment based on current parameter ests    #
    #--------------------------------------------------------------#
    optTx <- .predictOptimalTx(regimes@loo[[i]], l.data)
    optTx <- .convertTx(txInfo[[i]], optTx)

    #--------------------------------------------------------------#
    # Verify that value returned by regime is allowed by fSet      #
    #--------------------------------------------------------------#
#    .validTx(txInfo[[i]], optTx)

    #--------------------------------------------------------------#
    # Retrieve treatment variable column header                    #
    #--------------------------------------------------------------#
    txName <- .getTxName(txInfo[[i]])

    #--------------------------------------------------------------#
    # ind[,i] = 1 if patient tx in accordance with regime at dp i  #
    #           I(A_i = g_i)                                       #
    #         = 0 if patient tx not in accordance with regime at   #
    #             dp i.  I(A_i != g_i)                             #
    #--------------------------------------------------------------#
    ind[,i] <- .compare(txInfo[[i]], l.data[,txName], optTx)

    #--------------------------------------------------------------#
    # Change ith tx for all patients current regime                #
    #--------------------------------------------------------------#
    l.data[,txName] <- optTx

  }

  for( i in 1L:nDP ){
    #--------------------------------------------------------------#
    # Calculate propensity for treatment                           #
    #--------------------------------------------------------------#
    mm <- predict(object = propen[[i]], newdata = l.data)

    #--------------------------------------------------------------#
    # Retrieve treatment variable column header.                   #
    #--------------------------------------------------------------#
    txName <- .getTxName(txInfo[[i]])

    #--------------------------------------------------------------#
    # Retrieve probability of treatment being assigned treatment   #
    #--------------------------------------------------------------#
    func1 <- function(i, tx, superSet){ 
               if( is.na(tx[i]) ) {
                 return( 0.0 ) 
               } else {
                 tst <- tx[i] == superSet
                 return( mm[i,tst] )
               }
             }

    superSet <- .getSuperSet(txInfo[[i]])

    probOfG <- sapply(X = 1L:nSamples, 
                      FUN = func1, 
                      tx = l.data[,txName],
                      superSet = superSet)

    #--------------------------------------------------------------#
    # For those that have only 1 treatment, probability is 1.      #
    #--------------------------------------------------------------#
    probOfG[.getSingleton(txInfo[[i]])] <- 1.0

    lambda[,i] <- 1.0 - probOfG
  }

  #------------------------------------------------------------------#
  # doubly robust estimator.                                         #
  #------------------------------------------------------------------#
  vtemp <- response

  for(i in nDP:1L) {
    use4fit <- !.getSingleton(txInfo[[i]])

    #--------------------------------------------------------------#
    # For patients with 1 tx, set to next step value               #
    #--------------------------------------------------------------#
    valueFunc[,i] <- vtemp

    #--------------------------------------------------------------#
    # Calculate value function using optimal tx set by regime      #
    #--------------------------------------------------------------#
    valueFunc[use4fit,i] <- predict(outcome[[i]], newdata=l.data[use4fit,])

    vtemp <- valueFunc[,i]
  }

  #------------------------------------------------------------------#
  # cumInd = 1 if patient followed tx regime up to the ith dp.       #
  #            I(C_{eta} >= i)                                       #
  #        = 0 if patient did not follow tx regime up to the ith dp. #
  #            I(C_{eta} < i)                                        #
  #------------------------------------------------------------------#
  cumInd <- cbind(1L, 
                  t(apply(X = ind, MARGIN = 1L, FUN=cumprod)))[,-{nDP+1L},drop=FALSE]

  #------------------------------------------------------------------#
  # AC = 1 if all txs given to a patient follow the regime.          #
  #        I(C_{eta} = infinity)                                     #
  #    = 0 otherwise. I(C_{eta} <= K)                                #
  #------------------------------------------------------------------#
  AC <- apply(X = ind, MARGIN = 1L, FUN = prod)

  #------------------------------------------------------------------#
  # C = 1 if patient treated in accordance with regime up to dp      #
  #       i, but did not follow tx regime at dp i I(C_{eta} = i)     #
  #   = 0 otherwise. I(C_{eta} != i)                                 #
  #------------------------------------------------------------------#
  C <- cumInd * (1-ind)

  #------------------------------------------------------------------#
  # pc = probability that coarsening occurs at a later dp.           #
  #   Pr(C_{et} > i) = prod_{k=1}^{i} (Pr(A_k=g_k))                  #
  #                  = prod_{k=1}^{i} (1-Pr(A_k!=g_k))               #
  #------------------------------------------------------------------#
  pc <- t(apply(X={1.0-lambda}, MARGIN = 1L, cumprod))

  #------------------------------------------------------------------#
  #          I(C_{eta} = i) - Pr(A_i != g_i)*I(C_{eta} >= i)         #
  # DR = sum ----------------------------------------------- mu_i    #
  #       i               Pr(C_{eta} > i)                            #
  #------------------------------------------------------------------#
  DR <- rowSums((C - lambda*cumInd) / pc * valueFunc)

  #------------------------------------------------------------------#
  #     (   I(C_{eta} = infinity)        )                           #
  # mean|   --------------------- Y + DR |                           #
  #     (      Pr(C_{eta} > K)           )                           #
  #------------------------------------------------------------------#
  mn <- sum(DR + AC / pc[,nDP] * response, na.rm=TRUE) / nSamples

  return(mn)

}

#----------------------------------------------------------------------#
# Function to be minimized if no subsets                               #
#----------------------------------------------------------------------#
# eta      : current regime parameter estimates                        #
# regimes  : Regime_DecisionPointList                                  #
# txInfo   : TxInfoNoSubsets object                                    #
# l.data   : data.frame of treatment history and covariates            #
# outcome  : OutcomeRegressionAllTypes_DecisionPointList               #
# propen   : Propensity_DecisionPointList object                       #
# response : vector                                                    #
#----------------------------------------------------------------------#
.seqAIPWE_MPD_NoSubsets <- function(eta, 
                                    regimes, 
                                    txInfo, 
                                    l.data,
                                    outcome, 
                                    propen, 
                                    response) {

  #------------------------------------------------------------------#
  # set parameter estimates in regime objects.                       #
  #------------------------------------------------------------------#
  regimes <- .setPars(regimes, eta)

  #------------------------------------------------------------------#
  # Number of patients in training data.                             #
  #------------------------------------------------------------------#
  nSamples <- nrow(l.data)

  #------------------------------------------------------------------#
  # Number of decision points.                                       #
  #------------------------------------------------------------------#
  nDP <- length(regimes)

  #------------------------------------------------------------------#
  # ind : 0/1 patient tx is not/is in accordance with regime at dp i.#
  #------------------------------------------------------------------#
  ind <- matrix(data = 0L, nrow = nSamples, ncol = nDP)

  #------------------------------------------------------------------#
  # lambda : Probability that the tx does not follow regime.         #
  #          Pr(A_k != g_i)                                          #
  #------------------------------------------------------------------#
  lambda <- matrix(data = 0.0, nrow = nSamples, ncol = nDP)

  #------------------------------------------------------------------#
  # valueFunc : outcome regression at each dp                        #
  #------------------------------------------------------------------#
  valueFunc <- matrix(data = 0.0, nrow = nSamples, ncol = nDP)

  #------------------------------------------------------------------#
  # Obtain probabilities, indicators & fits                          #
  #------------------------------------------------------------------#
#  for( i in nDP:1L ){
  for( i in 1L:nDP ){

    #--------------------------------------------------------------#
    # Predict optimal treatment based on current parameter ests    #
    #--------------------------------------------------------------#
    optTx <- .predictOptimalTx(regimes[[i]], l.data)
    optTx <- .convertTx(txInfo[[i]], optTx)

    #--------------------------------------------------------------#
    # Verify that value returned by regime is allowed by fSet      #
    #--------------------------------------------------------------#
    .validTx(txInfo[[i]], optTx)

    #--------------------------------------------------------------#
    # Retrieve treatment variable column header                    #
    #--------------------------------------------------------------#
    txName <- .getTxName(txInfo[[i]])

    #--------------------------------------------------------------#
    # ind[,i] = 1 if patient tx in accordance with regime at dp i  #
    #           I(A_i = g_i)                                       #
    #         = 0 if patient tx not in accordance with regime at   #
    #             dp i.  I(A_i != g_i)                             #
    #--------------------------------------------------------------#
    ind[,i] <- .compare(txInfo[[i]], l.data[,txName], optTx)

    #--------------------------------------------------------------#
    # Change ith tx for all patients current regime                #
    #--------------------------------------------------------------#
    l.data[,txName] <- optTx

  }

  for( i in 1L:nDP ){

    #--------------------------------------------------------------#
    # Calculate propensity for treatment                           #
    #--------------------------------------------------------------#
    mm <- predict(object = propen[[i]], newdata = l.data)

    #--------------------------------------------------------------#
    # Retrieve treatment variable column header.                   #
    #--------------------------------------------------------------#
    txName <- .getTxName(txInfo[[i]])

    #--------------------------------------------------------------#
    # Retrieve probability of treatment being assigned treatment   #
    #--------------------------------------------------------------#
    func1 <- function(i, tx, superSet){ 
               if( is.na(tx[i]) ) {
                 return( 0.0 ) 
               } else {
                 tst <- tx[i] == superSet
                 return( mm[i,tst] )
               }
             }

    superSet <- .getSuperSet(txInfo[[i]])

    probOfG <- sapply(X = 1L:nSamples, 
                      FUN = func1, 
                      tx = l.data[,txName],
                      superSet = superSet)

    lambda[,i] <- 1.0 - probOfG

  }

  #------------------------------------------------------------------#
  # doubly robust estimator.                                         #
  #------------------------------------------------------------------#
  vtemp <- response

  for(i in nDP:1L) {

    #--------------------------------------------------------------#
    # Calculate value function using optimal tx set by regime      #
    #--------------------------------------------------------------#
    valueFunc[,i] <- predict(outcome[[i]], newdata=l.data)

  }

  #------------------------------------------------------------------#
  # cumInd = 1 if patient followed tx regime up to the ith dp.       #
  #            I(C_{eta} >= i)                                       #
  #        = 0 if patient did not follow tx regime up to the ith dp. #
  #            I(C_{eta} < i)                                        #
  #------------------------------------------------------------------#
  cumInd <- cbind(1L, 
                  t(apply(X = ind, MARGIN = 1L, FUN=cumprod)))[,-{nDP+1L},drop=FALSE]

  #------------------------------------------------------------------#
  # AC = 1 if all txs given to a patient follow the regime.          #
  #        I(C_{eta} = infinity)                                     #
  #    = 0 otherwise. I(C_{eta} <= K)                                #
  #------------------------------------------------------------------#
  AC <- apply(X = ind, MARGIN = 1L, FUN = prod)

  #------------------------------------------------------------------#
  # C = 1 if patient treated in accordance with regime up to dp      #
  #       i, but did not follow tx regime at dp i I(C_{eta} = i)     #
  #   = 0 otherwise. I(C_{eta} != i)                                 #
  #------------------------------------------------------------------#
  C <- cumInd * (1-ind)

  #------------------------------------------------------------------#
  # pc = probability that coarsening occurs at a later dp.           #
  #   Pr(C_{et} > i) = prod_{k=1}^{i} (Pr(A_k=g_k))                  #
  #                  = prod_{k=1}^{i} (1-Pr(A_k!=g_k))               #
  #------------------------------------------------------------------#
  pc <- t(apply(X={1.0-lambda}, MARGIN = 1L, cumprod))

  #------------------------------------------------------------------#
  #          I(C_{eta} = i) - Pr(A_i != g_i)*I(C_{eta} >= i)         #
  # DR = sum ----------------------------------------------- mu_i    #
  #       i               Pr(C_{eta} > i)                            #
  #------------------------------------------------------------------#
  DR <- rowSums((C - lambda*cumInd) / pc * valueFunc)

  #------------------------------------------------------------------#
  #     (   I(C_{eta} = infinity)        )                           #
  # mean|   --------------------- Y + DR |                           #
  #     (      Pr(C_{eta} > K)           )                           #
  #------------------------------------------------------------------#
  mn <- sum(DR + AC / pc[,nDP] * response, na.rm=TRUE) / nSamples

  return(mn)

}

.seqAIPWE_MPD <- function(eta, 
                          regimes, 
                          txInfo, 
                          l.data,
                          outcome, 
                          propen, 
                          response) {

  if( is(txInfo[[1L]], "TxInfoWithSubsets") ) {
    return(.seqAIPWE_MPD_WithSubsets(eta = eta,
                                     regimes = regimes,
                                     txInfo = txInfo,
                                     l.data = l.data,
                                     outcome = outcome,
                                     propen = propen,
                                     response = response))
  } else {
    return(.seqAIPWE_MPD_NoSubsets(eta = eta,
                                   regimes = regimes,
                                   txInfo = txInfo,
                                   l.data = l.data,
                                   outcome = outcome,
                                   propen = propen,
                                   response = response))
  }
}


