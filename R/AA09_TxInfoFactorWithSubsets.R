# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                   TxInfoFactorWithSubsets CLASS                  + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

#----------------------------------------------------------------------#
# treatment information for a single decision point when treatment     #
# is a character/factor and subsets are identified                     #
#----------------------------------------------------------------------#
# extends TxInfoFactor, TxSubset, TxInfoWithSubsets, and 
# 'SingleDecisionPoint'
#----------------------------------------------------------------------#
setClass(Class = "TxInfoFactorWithSubsets",
         contains = c("TxInfoFactor", "TxSubset", 
                      "TxInfoWithSubsets", "SingleDecisionPoint"))
