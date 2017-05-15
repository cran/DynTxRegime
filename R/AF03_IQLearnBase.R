# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                        CLASS IQLearnBase                         + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

#----------------------------------------------------------------------#
# Outcome Regression results obtained from an IQ-Learning method       #
#----------------------------------------------------------------------#
#  outcome  : OutcomeRegression object.                                #
#----------------------------------------------------------------------#
.checkValidity_IQLearnBase <- function(object) {

  errors <- character()

  if( !is(object@outcome, "SingleDecisionPoint") ) {
    msg <- "outcome is not of appropriate class"
    errors <- c(errors, msg)
  }

  if( !is(object@outcome, "SubsetsNotModeled") ) {
    msg <- "outcome is not of appropriate class"
    errors <- c(errors, msg)
  }

  if( length(errors) == 0L ) {
    return(TRUE)
  } else {
    return(errors)
  }
}

setClass(Class = "IQLearnBase", 
         contains = c("OutcomeOnly"),
         validity = .checkValidity_IQLearnBase)
