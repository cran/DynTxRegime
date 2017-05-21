setGeneric(name = ".newBOWL",
           def = function(moPropen, regime, BOWLObj, fSet, ...){
                   standardGeneric(".newBOWL")
                 } )

#----------------------------------------------------------------------#
# Create a new BOWL object                                             #
#----------------------------------------------------------------------#
#   params                                                             #
# moPropen : modelObj for propensity modeling                          #
# regime   : formula or list of formulae giving covariates for kernel  #
# BOWLObj  : optional BOWL object from previous step                   #
# fSet     : optional function defining subsets for modeling           #
# data     : data.frame of covariates                                  #
# reward   : reward                                                    #
# txName   : treatment variable column header in data                  #
# cvFolds  : number of cross-validation folds                          #
# lambdas  : tuning parameter(s)                                       #
# kernel   : character description of kernel function                  #
# kparam   : numeric parameter for kernel function                     #
# suppress : T/F indicating if prints to screen are to be executed.    #
#   returns                                                            #
# an BOWL object                                                       #
#----------------------------------------------------------------------#
.BOWLFirst <- function(moPropen,
                       regime,
                       BOWLObj,
                       fSet,
                       data,
                       reward,
                       txName,
                       cvFolds,
                       lambdas,
                       kernel,
                       kparam,
                       suppress, ...) {

  #------------------------------------------------------------------#
  # Initialize indicator of compliance with regime to TRUE           #
  #------------------------------------------------------------------#
  ind <- !logical(nrow(data))

  #------------------------------------------------------------------#
  # Shift reward to be all positive                                  #
  #------------------------------------------------------------------#
  shift <- min(reward)
  holdReward <- reward
  if( shift <= 0.0 ) {
    shift <- shift - 0.001
    reward <- reward - shift
    shift <- -shift
    if( !suppress ) {
      cat("Reward shifted by", shift, "to make positive.\n")
    }
  } else {
    shift <- 0.0
  }

  #------------------------------------------------------------------#
  # Initialize sum of reward to final stage reward.                  #
  #------------------------------------------------------------------#
  sumR <- reward

  #------------------------------------------------------------------#
  # Initialize product of propensity weights to 1.0                  #
  #------------------------------------------------------------------#
  prodPr <- numeric(nrow(data)) + 1.0

  #------------------------------------------------------------------#
  # Initialize step count to 1                                       #
  #------------------------------------------------------------------#
  nStep <- 1L

  if( !suppress ) {
    cat("Step 1 of BOWL Algorithm\n")
  }

  result <- .BOWLAlgorithm(moPropen = moPropen,
                           data = data,
                           txName = txName,
                           regime = regime,
                           ind = ind,
                           sumR = sumR,
                           prodPr = prodPr,
                           nStep = nStep,
                           cvFolds = cvFolds,
                           lambdas = lambdas,
                           kernel = kernel,
                           kparam = kparam,
                           fSet = fSet,
                           suppress = suppress)

  result@shift <- shift
  result@sumR <- holdReward

  return( result )

}

setMethod(f = ".newBOWL",
          signature = c(moPropen = "ModelObj_SubsetList",
                        regime   = "formula",
                        BOWLObj  = "NULL",
                        fSet     = "function"),
          definition = .BOWLFirst)

setMethod(f = ".newBOWL",
          signature = c(moPropen = "modelObj",
                        regime   = "formula",
                        BOWLObj  = "NULL",
                        fSet     = "NULL"),
          definition = .BOWLFirst)

setMethod(f = ".newBOWL",
          signature = c(moPropen = "modelObj",
                        regime   = "formula",
                        BOWLObj  = "NULL",
                        fSet     = "function"),
          definition = .BOWLFirst)

setMethod(f = ".newBOWL",
          signature = c(moPropen = "ModelObj_SubsetList",
                        regime   = "list",
                        BOWLObj  = "NULL",
                        fSet     = "function"),
          definition = .BOWLFirst)

setMethod(f = ".newBOWL",
          signature = c(moPropen = "modelObj",
                        regime   = "list",
                        BOWLObj  = "NULL",
                        fSet     = "function"),
          definition = .BOWLFirst)



#----------------------------------------------------------------------#
# Create a new BOWL object                                             #
#----------------------------------------------------------------------#
#   params                                                             #
# moPropen : modelObj for propensity modeling                          #
# regime   : formula ogiving covariates for kernel                     #
# BOWLObj  : optional BOWL object from previous step                   #
# fSet     : optional function defining subsets for modeling           #
# data     : data.frame of covariates                                  #
# reward   : reward                                                    #
# txName   : treatment variable column header in data                  #
# cvFolds  : number of cross-validation folds                          #
# lambdas  : tuning parameter(s)                                       #
# kernel   : character description of kernel function                  #
# kparam   : numeric parameter for kernel function                     #
# suppress : T/F indicating if prints to screen are to be executed.    #
#   returns                                                            #
# an BOWL object                                                       #
#----------------------------------------------------------------------#
.BOWLNext <- function(moPropen,
                      regime,
                      BOWLObj,
                      fSet,
                      data,
                      reward,
                      txName,
                      cvFolds,
                      lambdas,
                      kernel,
                      kparam,
                      suppress) {

  #------------------------------------------------------------------#
  # Retrieve current compliance indicator from BOWL object           #
  #------------------------------------------------------------------#
  ind <- BOWLObj@ind

  #------------------------------------------------------------------#
  # Add this stage reward to the sum store in BOWL object            #
  #------------------------------------------------------------------#
  if( {length(BOWLObj@sumR) != length(reward)} ||
      {length(reward) != nrow(data)} ) {
    stop("length of reward to not match previous steps")
  }
  sumR <- BOWLObj@sumR + reward

  #------------------------------------------------------------------#
  # Shift reward to be all positive                                  #
  #------------------------------------------------------------------#
  shift <- min(sumR)
  holdReward <- sumR
  if( shift <= 0.0 ) {
    shift <- shift - 0.001
    sumR <- sumR - shift
    shift <- -shift
    if( !suppress ) {
      cat("Total reward shifted by", shift, "to make positive.\n")
    }
  } else {
    shift <- 0.0
  }

  #------------------------------------------------------------------#
  # Retrieve the product of propensity weights from BOWL object      #
  #------------------------------------------------------------------#
  prodPr <- BOWLObj@prodPr

  #------------------------------------------------------------------#
  # increment step count                                             #
  #------------------------------------------------------------------#
  nStep <- BOWLObj@step + 1L

  if( !suppress ) {
    cat("Step", nStep, "of BOWL Algorithm\n")
  }

  result <- .BOWLAlgorithm(moPropen = moPropen,
                           data = data,
                           txName = txName,
                           regime = regime,
                           ind = ind,
                           sumR = sumR,
                           prodPr = prodPr,
                           nStep = nStep,
                           cvFolds = cvFolds,
                           lambdas = lambdas,
                           kernel = kernel,
                           kparam = kparam,
                           fSet = fSet,
                           suppress = suppress)

  result@shift <- shift
  result@sumR <- holdReward

  return( result )

}

setMethod(f = ".newBOWL",
          signature = c(moPropen = "ModelObj_SubsetList",
                        regime   = "formula",
                        BOWLObj  = "BOWL",
                        fSet     = "function"),
          definition = .BOWLNext)

setMethod(f = ".newBOWL",
          signature = c(moPropen = "modelObj",
                        regime   = "formula",
                        BOWLObj  = "BOWL",
                        fSet     = "NULL"),
          definition = .BOWLNext)

setMethod(f = ".newBOWL",
          signature = c(moPropen = "modelObj",
                        regime   = "formula",
                        BOWLObj  = "BOWL",
                        fSet     = "function"),
          definition = .BOWLNext)

setMethod(f = ".newBOWL",
          signature = c(moPropen = "ModelObj_SubsetList",
                        regime   = "list",
                        BOWLObj  = "BOWL",
                        fSet     = "function"),
          definition = .BOWLNext)

setMethod(f = ".newBOWL",
          signature = c(moPropen = "modelObj",
                        regime   = "list",
                        BOWLObj  = "BOWL",
                        fSet     = "function"),
          definition = .BOWLNext)

.BOWLAlgorithm <- function(moPropen,
                           data,
                           txName,
                           regime,
                           ind,
                           sumR,
                           prodPr,
                           nStep,
                           cvFolds,
                           lambdas,
                           kernel,
                           kparam,
                           fSet,
                           suppress) {

  #------------------------------------------------------------------#
  # Number of records in dataset.                                    #
  #------------------------------------------------------------------#
  ns <- nrow(data)

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
  # Predict probability for treatments                               #
  #------------------------------------------------------------------#
  prMatrix <- predict(propen, data)

  #------------------------------------------------------------------#
  # Propensity vector for weighting                                  #
  #------------------------------------------------------------------#
  tst <- data[,txName] %in% .getSuperSet(txInfo)
  prWgt <- prMatrix[tst]

  #------------------------------------------------------------------#
  # Include in product of weights.                                   #
  #------------------------------------------------------------------#
  prodPr <- prodPr * prWgt

  txVec <- numeric(ns) + 1.0
  txVec[data[,txName] == .getSuperSet(txInfo)[1L]] <- -1.0

  #------------------------------------------------------------------#
  # Perform weighted learning                                        #
  #------------------------------------------------------------------#
  result <- .newBOWLOptimization(regime = regime,
                                 txInfo = txInfo,
                                 ind = ind,
                                 prWgt = prodPr,
                                 response = sumR,
                                 txVec = txVec,
                                 data = data,
                                 kernel = kernel,
                                 kparam = kparam,
                                 lambdas = lambdas,
                                 cvFolds = cvFolds,
                                 suppress = suppress)

  #------------------------------------------------------------------#
  # Retrieve optimal treatment                                       #
  #------------------------------------------------------------------#
  optVec <- .predictOptimalTx(result, txInfo = txInfo)$optimalTx

  #------------------------------------------------------------------#
  # Identify patients that received opt tx                           #
  #------------------------------------------------------------------#
  ind2 <- {{txVec < -0.5} & {optVec < -0.5}} |
          {{txVec >  0.5} & {optVec >  0.5}}
  ind2[is.na(ind2)] <- FALSE

  #------------------------------------------------------------------#
  # Update Indicator to reflect if patient followed the optimal      #
  # treatment regime thru this decision point                        #
  #------------------------------------------------------------------#
  ind <- ind & ind2

  value <- .valueFuncOWL(subset = 1L:ns,
                         optTx = optVec,
                         txVec = txVec,
                         prWgt = prodPr,
                         response = sumR*ind)

  optVec <- ( optVec + 1 ) / 2
  optVec <- .getSuperSet(txInfo)[optVec]


  if( !suppress ) {
    cat("Estimated value:", value, "\n")
  }

  bowlObj <- new("BOWL",
                 "shift"          = 0.0,
                 "txInfo"         = txInfo,
                 "propen"         = propen,
                 "step"           = nStep,
                 "ind"            = ind,
                 "sumR"           = sumR,
                 "prodPr"         = prodPr,
                 "optim"          = result,
                 "estimatedValue" = value,
                 "optimalTx"      = optVec,
                 "call"           = NULL)

  return(bowlObj)
}
