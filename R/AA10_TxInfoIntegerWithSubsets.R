# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                  TxInfoIntegerWithSubsets CLASS                  + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

#----------------------------------------------------------------------#
# treatment information for a single decision point when treatment     #
# is an integer and subsets are identified                             #
#----------------------------------------------------------------------#
# extends TxInfoInteger, TxSubset, TxInfoWithSubsets, and 
# 'SingleDecisionPoint'
#----------------------------------------------------------------------#
setClass(Class = "TxInfoIntegerWithSubsets",
         contains = c("TxInfoInteger", "TxSubset", 
                      "TxInfoWithSubsets", "SingleDecisionPoint"))
