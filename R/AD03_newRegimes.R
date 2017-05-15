setGeneric(name = ".newRegime", 
           def = function(object){standardGeneric(".newRegime")})

setMethod(f = ".newRegime",   
          signature = c(object = "function"), 
          definition = function(object){ 

                         #-------------------------------------------#
                         # Retrieve the formal arguments of user     #
                         # provided function.                        #
                         #-------------------------------------------#
                         nms <- names(formals(object))

                         #-------------------------------------------#
                         # Function must include data as an input    #
                         # argument, thus number of variables is 1   #
                         # less than the number of formal args.      #
                         #-------------------------------------------#
                         nVars <- length(nms) - 1L

                         #-------------------------------------------#
                         # if only data is provided, or data is not  #
                         # included in function definition, throw    #
                         # error                                     #
                         #-------------------------------------------#
                         if( nVars <= 0L || !('data' %in% nms) ) {
                           UserError("input",
                                     paste("Formal arguments of",
                                           "function input through",
                                           "regimes must contain all",
                                           "regime parameters and",
                                           "'data'."))
                         }

                         tst <- nms %in% c('data')
                         nms <- nms[!tst]

                         obj <- new("Regime",
                                    "nVars"  = nVars,
                                    "vNames" = nms,
                                    "func"   = object,
                                    "pars"   = numeric(nVars))

                         return(obj)
                       } )

setMethod(f = ".newRegime",   
          signature = c(object = "list"), 
          definition = function(object) { 

                         nDP <- length(object)

                         obj <- list()

                         for( i in 1L:nDP ) {
                           obj[[i]] <- .newRegime(object[[i]])
                         }

                         obj <- new("Regime_DecisionPointList", 
                                    "loo" = obj)

                         return( obj )
                       } )


