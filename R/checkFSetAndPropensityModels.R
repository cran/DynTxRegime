setGeneric(name = ".checkFSetAndPropensityModels",
           def = function(txObj, moPropen, ...){ 
                   standardGeneric(f = ".checkFSetAndPropensityModels") 
                 })

setMethod(f = ".checkFSetAndPropensityModels",
          signature = c(txObj = "TxInfoNoSubsets",
                        moPropen = "ANY"),
          definition = function(txObj, moPropen, ..., data) {
              return( NULL )
            })

setMethod(f = ".checkFSetAndPropensityModels",
          signature = c(txObj = "TxInfoWithSubsets",
                        moPropen = "ModelObjSubset"),
          definition = function(txObj, moPropen, ..., data) {

              # subsets identified in fSet and its tx options
              subsets <- .getSubsets(object = txObj)

              # subset to which each patient belongs
              ptsSubset <- .getPtsSubset(object = txObj)

              # tx received by patients
              txReceived <- data[,.getTxName(object = txObj)]

              if (is.factor(txReceived)) {
                txReceived <- levels(txReceived)[txReceived]
              }

              # subsets to be included in fitting model(s)
              subsetsModeled <- .extractModelNames(modelObj = moPropen)

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
                          " excluded from propensity regression")
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

            })


setMethod(f = ".checkFSetAndPropensityModels",
          signature = c(txObj = "TxInfoWithSubsets",
                        moPropen = "modelObj"),
          definition = function(txObj, moPropen, ..., data) {

              # subsets identified in fSet and its tx options
              subsets <- .getSubsets(object = txObj)

              # subset to which each patient belongs
              ptsSubset <- .getPtsSubset(object = txObj)

              # tx received by patients
              txReceived <- data[,.getTxName(object = txObj)]

              # ensure that if any subsets have only one tx option, pts received only 1 tx

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
                    message("subset ", i, " excluded from propensity regression")
                  }
                }
              }

              return( NULL )

            })

setMethod(f = ".checkFSetAndPropensityModels",
          signature = c(txObj = "TxInfoWithSubsets",
                        moPropen = "ModelObj_SubsetList"),
          definition = function(txObj, moPropen, ..., data) {

              # subsets identified in fSet and its tx options
              subsets <- .getSubsets(object = txObj)

              # subset to which each patient belongs
              ptsSubset <- .getPtsSubset(object = txObj)

              # tx received by patients
              txReceived <- data[,.getTxName(object = txObj)]

              # subsets to be included in fitting model(s)
              subsetsModeled <- .extractModelNames(modelObj = moPropen)
  
              # identify which patients fall into these subsets
              tst <- ptsSubset %in% subsetsModeled

              if (any(!tst)) {

                subsetsNotModeled <- sort(x = unique(x = ptsSubset[!tst]))

                # if any patients are not included, ensure that they have only
                # one tx option and received only 1 tx

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
                          " excluded from propensity regression")
                }
              }

              if (any(tst)) {

                # ensure that treatments received agree with feasible tx sets

                ssList <- NULL
                for (i in subsetsModeled) {

                  # identify tx received
                  txr <- unique(x = txReceived[ptsSubset == i])

                  if (any(!{txr %in% subsets[[ i ]]})) {
                    ssList <- c(ssList, i)
                  }
                }
                if (!is.null(x = ssList)) {
                  message("NOTE: subset(s) ", paste(ssList, collapse = ", "), 
                          " received tx not in accordance with specified feasible tx sets")
                }
              }

              return( NULL )

            })
