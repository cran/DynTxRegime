\name{.newTxInfo}
\docType{methods}
\alias{.newTxInfo}
\alias{.newTxInfo,NULL,character-method}
\alias{.newTxInfo,function,character-method}
\alias{.newTxInfo,list,list-method}
\alias{.newTxInfo,NULL,list-method}

\title{ Create Treatment Information Objects}

\description{
Given a definition for feasible sets and treatment name,
creates an appropriately classed treatment information object.
Method is not exported.
}

\usage{
.newTxInfo(fSet, txName, ...)
\S4method{.newTxInfo}{NULL,character}(fSet, txName, data, suppress, verify = TRUE)
\S4method{.newTxInfo}{function,character}(fSet, txName, data, suppress, verify = TRUE)
\S4method{.newTxInfo}{list,list}(fSet, txName, data, suppress, verify = TRUE)
\S4method{.newTxInfo}{NULL,list}(fSet, txName, data, suppress, verify = TRUE)
}

\arguments{
 \item{fSet}{NULL, \code{"function"}, or \code{"list"} of \code{"function"s} specifying
             subsets.}
 \item{txName}{The column header of \code{"data.frame"} indicating the
               treatment variable. If list, column header of
               \code{"data.frame"} indicating treatment at each decision point.}
 \item{...}{Used to pass arguments that are required but
            the class of which does not determine the
            method that is selected.}
 \item{data}{\code{"data.frame"} of covariates and treatment histories.}
 \item{suppress}{T/F indicating if screen prints are generated.}
 \item{verify}{T/F indicating if treatments should be verified.}
}

\author{
  Shannon T. Holloway <sthollow@ncsu.edu>
}

\examples{

data(bmiData)
bmiData$A1 <- as.factor(bmiData$A1)
bmiData$A2 <- as.factor(bmiData$A2)
bmiData$A3 <- 0L
bmiData$A3[bmiData$A1 == "MR"] <- 1L
bmiData$A4 <- 0L
bmiData$A5 <- as.factor("CD")

# Integer treatment with no subsetting
obj <- DynTxRegime:::.newTxInfo(fSet = NULL, 
                                txName = "A3", 
                                data = bmiData,  
                                suppress = TRUE,  
                                verify = TRUE)
is(obj)

# Integer treatment with only 1 tx in data
# Illegal if verification is requested
obj <- try(DynTxRegime:::.newTxInfo(fSet = NULL,  
                                    txName = "A4",  
                                    data = bmiData,  
                                    suppress = TRUE,  
                                    verify = TRUE))

# Integer treatment with only 1 tx in data
# legal if verification is turned off
obj <- DynTxRegime:::.newTxInfo(fSet = NULL,  
                                txName = "A4",  
                                data = bmiData,  
                                suppress = TRUE,  
                                verify = FALSE)
is(obj)

# Factor treatment with no subsetting
obj <- DynTxRegime:::.newTxInfo(fSet = NULL,  
                                txName = "A2",  
                                data = bmiData,  
                                suppress = TRUE,  
                                verify = TRUE)
is(obj)

# Factor treatment with only 1 tx in data
# Illegal if verification is requested
obj <- try(DynTxRegime:::.newTxInfo(fSet = NULL,  
                                txName = "A5",  
                                data = bmiData,  
                                suppress = TRUE,  
                                verify = TRUE))

# Factor treatment with only 1 tx in data
# legal if verification is turned off
obj <- DynTxRegime:::.newTxInfo(fSet = NULL,  
                                txName = "A5",  
                                data = bmiData,  
                                suppress = TRUE,  
                                verify = FALSE)
is(obj)

fSet1 <- function(data){
           subsets <- list(list("subset1",c(0L,1L)),
                           list("subset2",c(0L,1L)))
           txOpts <- character(nrow(data))
           txOpts[data$A1 == "CD"] <- "subset1"
           txOpts[data$A1 == "MR"] <- "subset2"
           return(list("subsets" = subsets, "txOpts" = txOpts))
         }

# Integer treatment with subsetting
obj <- DynTxRegime:::.newTxInfo(fSet = fSet1, 
                                txName = "A3", 
                                data = bmiData, 
                                suppress = TRUE, 
                                verify = TRUE)
is(obj)

# Integer treatment with subsetting only 1 tx in data
# Illegal if verification is on
obj <- try(DynTxRegime:::.newTxInfo(fSet = fSet1, 
                                    txName = "A4", 
                                    data = bmiData, 
                                    suppress = TRUE, 
                                    verify = TRUE))

# Integer treatment with subsetting only 1 tx in  data
# Legal if verification is off
obj <- DynTxRegime:::.newTxInfo(fSet = fSet1, 
                                txName = "A4", 
                                data = bmiData, 
                                suppress = TRUE, 
                                verify = FALSE)
is(obj)

fSet2 <- function(data){
           subsets <- list(list("subset1", c("CD","MR")),
                           list("subset2", c("CD","MR")))
          txOpts <- character(nrow(data))
          txOpts[data$A1 == "CD"] <- "subset1"
          txOpts[data$A1 == "MR"] <- "subset2"
          return(list("subsets" = subsets, "txOpts" = txOpts))
         }

# Factor treatment with subsetting
obj <- DynTxRegime:::.newTxInfo(fSet = fSet2, 
                                txName = "A2", 
                                data = bmiData, 
                                suppress = TRUE, 
                                verify = TRUE)
is(obj)

# Factor treatment with subsetting only 1 tx in data
# Illegal of verification is on
obj <- try(DynTxRegime:::.newTxInfo(fSet = fSet2, 
                                txName = "A5", 
                                data = bmiData, 
                                suppress = TRUE, 
                                verify = TRUE))

# Factor treatment with subsetting only 1 tx in data
# Legal of verification is off
obj <- DynTxRegime:::.newTxInfo(fSet = fSet2, 
                                txName = "A5", 
                                data = bmiData, 
                                suppress = TRUE, 
                                verify = FALSE)
is(obj)

# Multiple decision points no subsetting
obj <- DynTxRegime:::.newTxInfo(fSet = NULL, 
                                txName = list("A1","A2"), 
                                data  = bmiData, 
                                suppress = TRUE, 
                                verify = TRUE)
is(obj)
is(obj[[1L]])
is(obj[[2L]])

# Multiple decision points no subsetting
# Only 1 tx in data
# Legal if verification is turned off
obj <- DynTxRegime:::.newTxInfo(fSet = NULL, 
                                txName = list("A1","A4"), 
                                data  = bmiData, 
                                suppress = TRUE, 
                                verify = FALSE)
is(obj)
is(obj[[1L]])
is(obj[[2L]])

# Multiple decision points no subsetting
# Only 1 tx in data
# Illegal if verification is turned on
obj <- try(DynTxRegime:::.newTxInfo(fSet = NULL, 
                                    txName = list("A1","A4"), 
                                    data  = bmiData, 
                                    suppress = TRUE, 
                                    verify = TRUE))

# Multiple decision points with subsetting
obj <- DynTxRegime:::.newTxInfo(fSet = list(fSet1,fSet2), 
                                txName = list("A3","A2"), 
                                data  = bmiData, 
                                suppress = TRUE, 
                                verify = TRUE)
is(obj)
is(obj[[1L]])
is(obj[[2L]])

# Multiple decision points with subsetting
# Only 1 tx in data
# Legal if verification is turned off
obj <- DynTxRegime:::.newTxInfo(fSet = list(fSet1,fSet2), 
                                txName = list("A1","A5"), 
                                data  = bmiData, 
                                suppress = TRUE, 
                                verify = FALSE)
is(obj)
is(obj[[1L]])
is(obj[[2L]])

# Multiple decision points with subsetting
# Only 1 tx in data
# Illegal if verification is turned on
obj <- try(DynTxRegime:::.newTxInfo(fSet = list(fSet1,fSet2), 
                                    txName = list("A1","A5"), 
                                    data  = bmiData, 
                                    suppress = TRUE, 
                                    verify = TRUE))
}

\keyword{internal}

