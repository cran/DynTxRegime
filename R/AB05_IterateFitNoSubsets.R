# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                    IterateFitNoSubsets CLASS                     + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

#----------------------------------------------------------------------#
# Results of an outcome regression obtained iteratively.               #
#----------------------------------------------------------------------#
#   fitObjME  : An object of class modelObjFit for main effects model  #
#   fitObjC   : An object of class modelObjFit for contrast model      #
#   txInfo    : TxInfoNoSubsets object containing treatment information#
#----------------------------------------------------------------------#
setClass(Class = "IterateFitNoSubsets", 
         contains = c("IterateFit"))
