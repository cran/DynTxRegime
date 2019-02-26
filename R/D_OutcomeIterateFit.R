# October 25, 2018

.validity_OutcomeIterateFit <- function(object) {

  # fitObjC must be OutcomeSimpleFit, OutcomeSimpleFit_fSet, or
  # OutcomeSimpleFit_SubsetList
  if (!is(object = object@fitObjC, class2 = "OutcomeSimpleFit") &&
      !is(object = object@fitObjC, class2 = "OutcomeSimpleFit_fSet") &&
      !is(object = object@fitObjC, class2 = "OutcomeSimpleFit_SubsetList") &&
      !is.na(x = object@fitObjC)) {
    return( "incorrect object for @fitObjC" )
  }

  # fitObjM must be OutcomeSimpleFit, OutcomeSimpleFit_fSet, or
  # OutcomeSimpleFit_SubsetList
  if (!is(object = object@fitObjM, class2 = "OutcomeSimpleFit") &&
      !is(object = object@fitObjM, class2 = "OutcomeSimpleFit_fSet") &&
      !is(object = object@fitObjM, class2 = "OutcomeSimpleFit_SubsetList") &&
      !is.na(x = object@fitObjM)) {
    return( "incorrect object for @fitObjM" )
  }

  return( TRUE )
}

#' Class \code{OutcomeIterateFit}
#'
#' Class \code{OutcomeIterateFit} is a an outcome regression step completed
#'   using the iterative algorithm.
#'
#' @name OutcomeIterateFit-class
#'
#' @slot fitObjC Contrast Result
#' @slot fitObjM Main Effects Result
#'
#' @include D_newModel.R D_OutcomeSimpleFit.R D_OutcomeSimpleFit_fSet.R
#'  D_OutcomeSimpleFit_SubsetList.R
#'
#' @keywords internal
setClass(Class = "OutcomeIterateFit", 
         slots = c(fitObjC = "ANY",
                   fitObjM = "ANY"),
         prototype = list(fitObjC = NA, fitObjM = NA),
         validity = .validity_OutcomeIterateFit)

##########
## METHODS
##########
#' Methods Available for Objects of Class \code{OutcomeIterateFit}
#'
#' Methods call equivalently named methods defined for \code{OutcomeSimpleFit},
#'   \code{OutcomeSimpleFit_fSet}, or \code{OutcomeSimpleFit_SubsetList}.
#' Exact method dispatched depends on classes of @fitObjC and @fitObjM.
#' When a value object is returned, it is a list. 
#'
#' @name OutcomeIterateFit-methods
#'
#' @keywords internal
NULL

.iterateFit <- function(moMain, 
                        moCont, 
                        response, 
                        txObj, 
                        data, 
                        iter, 
                        suppress) {

  # set tolerance for equivalent results.
  tol <- 1.5e-8

  # initialize contrast component of response to zero
  fittedCont <- numeric(length = nrow(x = data))
  response <- drop(x = response)

  contrast.old <- numeric(length = nrow(x = data)) + 1.0/0.0
  main.old <- numeric(length = nrow(x = data)) + 1.0/0.0

  # iterate until the number of iterations reaches iter or difference
  # between iterations reaches tol
  iter2 <- 0L
  while (TRUE) {

    # obtain fit for main effects
    tempY <- response - fittedCont

    fitMain <- .newOutcomeFit(moMain = moMain,
                              moCont = NULL,
                              data = data,
                              response = tempY,
                              txObj = txObj,
                              iter = NULL,
                              suppress = TRUE)

    fittedMain <- drop(x = predict(object = fitMain, newdata = data))

    # redefine response to be contrast.
    tempY <- response - fittedMain

    # obtain contrasts fit
    fitCont <- .newOutcomeFit(moMain = NULL,
                              moCont = moCont,
                              data = data,
                              response = tempY,
                              txObj = txObj,
                              iter = NULL,
                              suppress = TRUE)

    # calculate fitted contrast
    fittedCont <- drop(x = predict(object = fitCont, newdata = data))

    # compare the main effects and contrasts components of this
    # iteration to those of last to determine max difference
    tst <- isTRUE(x = all.equal(target = contrast.old, 
                                current = fittedCont, 
                                tolerance = tol))
    tst <- isTRUE(x = all.equal(target = main.old, 
                                current = fittedMain,  
                                tolerance = tol)) && tst
    if (tst) break

    # store current iteration values for comparison at next iter
    contrast.old <- fittedCont
    main.old <- fittedMain
    iter2 <- iter2 + 1L
    if (iter2 == iter) {
      warning("convergence not attained within max iterations")
      break
    }
  }

  if (any(is.na(x = coef(object = fitMain))) || 
      any(is.na(x = coef(object = fitCont)))) {
    stop("fit results in NA parameter estimates")
  }

  if (!suppress) {
    cat("Fit outcome regression in", iter2, "iterations.\n")
    cat("Main Effects regression results:\n")
    print(x = fitMain)
    cat("Contrast regression results:\n")
    print(x = fitCont)
  }

  result <- new("OutcomeIterateFit",
                "fitObjM" = fitMain,
                "fitObjC" = fitCont)

  return( result )
}

#' @rdname newOutcomeFit
setMethod(f = ".newOutcomeFit", 
          signature = c(moMain = "modelObj",
                        moCont = "modelObj",
                        txObj = "TxInfoWithSubsets",
                        iter = "integer"), 
          definition = .iterateFit)

#' @rdname newOutcomeFit
setMethod(f = ".newOutcomeFit", 
          signature = c(moMain = "modelObj",
                        moCont = "modelObj",
                        txObj = "TxInfoNoSubsets",
                        iter = "integer"), 
          definition = .iterateFit)

#' @rdname newOutcomeFit
setMethod(f = ".newOutcomeFit", 
          signature = c(moMain = "ModelObj_SubsetList",
                        moCont = "ModelObj_SubsetList",
                        txObj = "TxInfoWithSubsets",
                        iter = "integer"), 
          definition = .iterateFit)



#' @rdname OutcomeIterateFit-methods
setMethod(f = "coef",
          signature = c(object = "OutcomeIterateFit"),
          definition = function(object, ...) {
              res1 <- coef(object@fitObjM, ...)
              res2 <- coef(object@fitObjC, ...)
              return( .matchLists(res1, res2) )
            })

#' @rdname OutcomeIterateFit-methods
setMethod(f = "fitObject", 
          signature = c(object = "OutcomeIterateFit"), 
          definition = function(object, ...) {
              res1 <- fitObject(object = object@fitObjM, ...)
              res2 <- fitObject(object = object@fitObjC, ...)
              return( .matchLists(res1, res2) )
            })

#' @rdname OutcomeIterateFit-methods
setMethod(f = "outcome", 
          signature = c(object = "OutcomeIterateFit"), 
          definition = function(object, ...) {
              res1 <- fitObject(object@fitObjM, ...)
              res2 <- fitObject(object@fitObjC, ...)
              return( .matchLists(res1, res2) )
            })

#' @rdname OutcomeIterateFit-methods
setMethod(f = "plot", 
          signature = c(x = "OutcomeIterateFit"), 
          definition = function(x, suppress=FALSE, ...) {

              plot(x@fitObjM, suppress = suppress, ...)
              plot(x@fitObjC, suppress = suppress, ...)

            })

#' @rdname OutcomeIterateFit-methods
setMethod(f = "predict", 
          signature = c(object = "OutcomeIterateFit"), 
          definition = function(object, ...) { stop("not allowed") })

#' \code{.predictAll(object, newdata)}
#'
#' \code{.predictAll(object, newdata)} combines the two components into a
#'   single optimal tx and decision function
#'
#' @rdname OutcomeIterateFit-methods
setMethod(f = ".predictAll", 
          signature = c(object = "OutcomeIterateFit",
                        newdata = "data.frame"), 
          definition = function(object, newdata, ...) {
              fittedM <- .predictAll(object = object@fitObjM, 
                                     newdata = newdata)

              mna <- is.na(x = fittedM$decisionFunc)
              fittedM$decisionFunc[mna] <- 0.0

              fittedC <- .predictAll(object = object@fitObjC, 
                                     newdata = newdata)

              cna <- is.na(x = fittedC$decisionFunc)
              fittedC$decisionFunc[cna] <- 0.0

              pred <- fittedM$decisionFunc + fittedC$decisionFunc
              pred[mna & cna] <- NA

              tst <- apply(X = pred, 
                           MARGIN = 1L,  
                           FUN = function(x) {all(is.na(x = x))})
              optimalTx <- fittedM$optimalTx

              optTx <- apply(X = pred[!tst,],  
                             MARGIN = 1L,  
                             FUN = function(x) {which.max(x = x)})
              superset <- .getSuperset(object = object@fitObjM@txInfo)
              optTx <- superset[optTx]

              optimalTx[!tst] <- optTx
              optimalTx <- .convertTx(object = object@fitObjM@txInfo, 
                                      txVec = optimalTx)

              return( list("optimalTx" = optimalTx,
                           "decisionFunc" = pred) )
            })

#' @rdname OutcomeIterateFit-methods
setMethod(f = "print", 
          signature = c(x = "OutcomeIterateFit"), 
          definition = function(x, ...) {
              print(x = x@fitObjM)
              print(x = x@fitObjC)
            })

#' @rdname OutcomeIterateFit-methods
setMethod(f = "show", 
          signature = c(object = "OutcomeIterateFit"), 
          definition = function(object) {
              show(object = object@fitObjM)
              show(object = object@fitObjC)
            })

#' @rdname OutcomeIterateFit-methods
setMethod(f = "summary",
          signature = c(object = "OutcomeIterateFit"),
          definition = function(object, ...) {
              res1 <- summary(object = object@fitObjM, ...)
              res2 <- summary(object = object@fitObjC, ...)
              return( .matchLists(res1, res2) )
            })

.matchLists <- function(lista, listb) {

  if (!is.list(x = lista[[ 1L ]])) return( c(lista,listb) )

  nmsa <- names(x = lista)
  nmsb <- names(x = listb)

  if (nmsa %in% c("moMain","moCont") && nmsb %in% c("moMain","moCont")) {
    return( c(lista,listb) )
  }

  if (!all(nmsa %in% nmsb) || !all(nmsb %in% nmsb)) {
    stop("this should never happen")
  }
  res <- list()
  for (i in 1L:length(x = nmsa)) {
    res[[ nmsa[i] ]] <- c(lista[[ nmsa[i] ]],
                          listb[[ nmsb[i] ]])
  }
  return( res )
}
