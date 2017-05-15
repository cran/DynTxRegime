#======================================================================#
# Inputs :                                                             #
#                                                                      #
#    X1 : An nxp matrix of covariates                                  #
#                                                                      #
#    X2 : An mxp matrix of covariates                                  #
#                                                                      #
#  kern : A character string indicating type of kernel                 #
#                                                                      #
# param : If required, a numeric value for the kernel parameter        #
#                                                                      #
# Outputs :                                                            #
#                                                                      #
# An nxm kernel matrix                                                 #
#======================================================================#
.kernelFunc <- function(X1, X2, kern, param) {
  #------------------------------------------------------------------#
  # To simplify comparison, convert character string to lower case   #
  #------------------------------------------------------------------#
  kern <- tolower(kern)

  #------------------------------------------------------------------#
  # If passed as data.frame objects, convert to matrices             #
  #------------------------------------------------------------------#
  if( is(X1,"data.frame") ) X1 <- data.matrix(X1)
  if( is(X2,"data.frame") ) X2 <- data.matrix(X2)

  if( !is(X1, "matrix") ) X1 <- matrix(X1, ncol = 1L)
  if( !is(X2, "matrix") ) X1 <- matrix(X2, ncol = 1L)

  if( kern == 'linear' ) {
    #--------------------------------------------------------------#
    # Linear kernel                                                #
    #--------------------------------------------------------------#
    Kern <- X1 %*% t(X2)
  } else if( kern == 'poly' ) {
    #--------------------------------------------------------------#
    # Polynomial kernel                                            #
    #--------------------------------------------------------------#
    Kern <- (1.0 + X1 %*% t(X2))^param
  } else if( kern == 'radial' ) {
    #--------------------------------------------------------------#
    # gaussian kernel                                              #
    #--------------------------------------------------------------#
    y <- t(X2)
    param <- 1.0 / (2.0 * param * param)

    Kern <- apply(X = X1, 
                  MARGIN = 1L, 
                  FUN = function(x){
                          tm <- colSums((x-y)^2)
                          exp(- param * tm)})
    Kern <- t(Kern)

  } else {
    UserError("input", "Kernel is not recognized.")
  }

  return(Kern)
}
