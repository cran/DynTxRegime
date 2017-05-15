# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                   TxInfoFactorNoSubsets CLASS                    + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

#----------------------------------------------------------------------#
# treatment name and superset of treatment options when treatment      #
# is a factor                                                          #
#----------------------------------------------------------------------#
# extends TxInfoFactor, TxInfoNoSubsets, and SingleDecisionPoint
#----------------------------------------------------------------------#
setClass(Class = "TxInfoFactorNoSubsets",
         contains = c("TxInfoFactor", "TxInfoNoSubsets", "SingleDecisionPoint") )

