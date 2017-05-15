# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                    TxInfoIntegerNoSubsets CLASS                  + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

#----------------------------------------------------------------------#
# treatment name and superset of treatment options when treatment      #
# is an integer                                                        #
#----------------------------------------------------------------------#
# extends TxInfoNoSubsets, TxInfoInteger, SingleDecisionPoint, directly
#
# slots:
#
#   superSet  : an integer vector of all possible tx options
#
#----------------------------------------------------------------------#
setClass(Class = "TxInfoIntegerNoSubsets",
         contains = c("TxInfoInteger", "TxInfoNoSubsets", "SingleDecisionPoint") )
