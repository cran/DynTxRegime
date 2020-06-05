setGeneric(name = ".checkFSetAndOutcomeModels",
           def = function(txObj, moMain, moCont, ...){ 
                   standardGeneric(f = ".checkFSetAndOutcomeModels") 
                 })

setMethod(f = ".checkFSetAndOutcomeModels",
          signature = c(txObj = "ANY",
                        moMain = "ANY",
                        moCont = "ANY"),
          definition = function(txObj, moMain, moCont, ..., data) {
              return( NULL )
            })


.modelObjSubsetCheckOM <- function(txObj, moMain, moCont, ..., data) {

  # subsets identified in fSet and its tx options
  subsets <- .getSubsets(object = txObj)

  # subset to which each patient belongs
  ptsSubset <- .getPtsSubset(object = txObj)

  # tx received by patients
  txReceived <- data[,.getTxName(object = txObj)]

  # subsets to be included in fitting model(s)
  subsetsModeled <- .extractModelNames(modelObj = moMain)
  subsetsModeled <- c(subsetsModeled, .extractModelNames(modelObj = moCont))
  subsetsModeled <- unique(x = subsetsModeled)

  # identify which patients fall into these subsets
  tst <- ptsSubset %in% subsetsModeled

  if (any(!tst)) {

    # if any patients are not included, ensure that they have only
    # one tx option and received only 1 tx

    subsetsNotModeled <- sort(x = unique(x = ptsSubset[!tst]))

    ssList <- NULL

    for (i in subsetsNotModeled) {

      if (length(x = subsets[[ i ]]) == 1L) {

        # if 1 feasible tx option, identify tx received
        txr <- unique(x = txReceived[ptsSubset == i])

        if (length(x = txr) != 1L) {
          # if more than 1 tx received stop
          stop("patients in subset ", i, 
               " received tx not in accordance with feasible tx sets;",
               " subset must be modeled", call. = FALSE)
        } else {
          ssList <- c(ssList, i)
        }

      } else {

        stop("subset ", i, " has > 1 tx; subset must be modeled")

      }
    }

    if (!is.null(x = ssList)) {
      message("subset(s) ", paste(ssList, collapse = ", "), 
              " excluded from outcome regression")
    }

  }

  if (any(tst)) {

    ssList <- NULL

    # ensure that tx received agree with feasible tx sets

    for (i in subsetsModeled) {

      # identify tx received
      txr <- unique(x = txReceived[ptsSubset == i])

      if (any(!{txr %in% subsets[[ i ]]})) ssList <- c(ssList, i)

    }

    if (!is.null(x = ssList)) {
      message("NOTE: subset(s) ", paste(ssList, collapse = ", "), 
              " received tx not in accordance with specified feasible tx sets")
    }
  }

  return( NULL )

}

setMethod(f = ".checkFSetAndOutcomeModels",
          signature = c(txObj = "TxInfoWithSubsets",
                        moMain = "ModelObjSubset",
                        moCont = "ModelObjSubset"),
          definition = .modelObjSubsetCheckOM)

setMethod(f = ".checkFSetAndOutcomeModels",
          signature = c(txObj = "TxInfoWithSubsets",
                        moMain = "ModelObjSubset",
                        moCont = "NULL"),
          definition = .modelObjSubsetCheckOM)

setMethod(f = ".checkFSetAndOutcomeModels",
          signature = c(txObj = "TxInfoWithSubsets",
                        moMain = "NULL",
                        moCont = "ModelObjSubset"),
          definition = .modelObjSubsetCheckOM)

setMethod(f = ".checkFSetAndOutcomeModels",
          signature = c(txObj = "TxInfoWithSubsets",
                        moMain = "ModelObj_SubsetList",
                        moCont = "ModelObj_SubsetList"),
          definition = .modelObjSubsetCheckOM)

setMethod(f = ".checkFSetAndOutcomeModels",
          signature = c(txObj = "TxInfoWithSubsets",
                        moMain = "ModelObj_SubsetList",
                        moCont = "NULL"),
          definition = .modelObjSubsetCheckOM)

setMethod(f = ".checkFSetAndOutcomeModels",
          signature = c(txObj = "TxInfoWithSubsets",
                        moMain = "NULL",
                        moCont = "ModelObj_SubsetList"),
          definition = .modelObjSubsetCheckOM)



.modelObjCheckOM <- function(txObj, moMain, moCont, ..., data) {

  # subsets identified in fSet and its tx options
  subsets <- .getSubsets(object = txObj)

  # subset to which each patient belongs
  ptsSubset <- .getPtsSubset(object = txObj)

  # tx received by patients
  txReceived <- data[,.getTxName(object = txObj)]

  # ensure that if any subsets have only one tx option, pts received only 1 tx

  ssList <- NULL

  for (i in names(x = subsets)) {

    if (length(x = subsets[[ i ]]) == 1L) {

      # if 1 feasible tx option, identify tx received
      txr <- unique(x = txReceived[ptsSubset == i])

      if (length(x = txr) != 1L) {
        # if more than 1 tx received stop
        stop("patients in subset ", i, 
             " received tx not in accordance with feasible tx sets;",
             " subset must be modeled", call. = FALSE)
      } else {
        ssList <- c(ssList, i)
      }
    }
  }

  if (!is.null(x = ssList)) {
    message("NOTE: subset(s) ", paste(ssList, collapse = ", "), 
            " excluded from outcome regression")
  }

  return( NULL )

}
setMethod(f = ".checkFSetAndOutcomeModels",
          signature = c(txObj = "TxInfoWithSubsets",
                        moMain = "modelObj",
                        moCont = "modelObj"),
          definition = .modelObjCheckOM)

setMethod(f = ".checkFSetAndOutcomeModels",
          signature = c(txObj = "TxInfoWithSubsets",
                        moMain = "modelObj",
                        moCont = "NULL"),
          definition = .modelObjCheckOM)

setMethod(f = ".checkFSetAndOutcomeModels",
          signature = c(txObj = "TxInfoWithSubsets",
                        moMain = "NULL",
                        moCont = "modelObj"),
          definition = .modelObjCheckOM)



.extractModelNames <- function(modelObj) {

  if (is.null(x = modelObj)) return( NULL )

  if (is.list(x = modelObj)) {
    nms <- names(x = modelObj)
    subsetsModeled <- sapply(X = nms,
                             FUN = function(x) {
                                     unlist(x = strsplit(x = x, split = ","))
                                   })
  } else {
    subsetsModeled <- unlist(x = strsplit(x = names(x = modelObj), split = ","))
  }
 
  return( subsetsModeled )
}
