\name{plugInValue}
\alias{plugInValue}
\title{Estimate Plug-in Value }

\description{Estimate the plug-in value of a fixed treatment regime.}
\usage{ plugInValue(optTx1, optTx2, response, tx1, tx2) }

\arguments{
  \item{optTx1   }{Object of class \code{"vector."}
                   First-stage treatments corresponding to the 
                   first-stage decision rule of the proposed regime.}

  \item{optTx2   }{Object of class \code{"vector."}
                   Second-stage treatments corresponding to the 
                   second-stage decision rule of the proposed regime.}

  \item{response }{Object of class \code{"vector."}
                   Outcome of interest.}

  \item{tx1      }{Object of class \code{"vector."}
                   First-stage randomized treatments.}

  \item{tx2      }{Object of class \code{"vector."}
                  Second-stage randomized treatments.}
}

\details{
The formula for the plug-in value estimate is

\deqn{\frac{\sum_i Y_i*ind1_i*ind1_i}{\sum_i ind1_i*ind2_i }}{%
(\sum_i Y_i*ind1_i*ind2_i)/(\sum_i ind1_i*ind2_i)}

where \eqn{ind1} and \eqn{ind2} are indicators that the first- and
second-stage randomized treatments were consistent with the decision rules.
}
\value{
  \item{value     }{estimated plug-in value of the regime}
  \item{fixedReg  }{estimated plug-in value of all possible fixed regimes}
}
\references{
  Laber, E.B., Linn, K.A., and Stefanski, L.A. 
(2014). 
  Interactive Q-learning. 
Biometrika, 101, 831-847.
}
\author{
  Shannon T. Holloway <sthollow@ncsu.edu>
}

\examples{
# Load and process data set 
  data(bmiData)

  # define response y to be the negative 12 month
  # change in BMI from baseline
  bmiData$y <- -100*(bmiData[,6] - bmiData[,4])/bmiData[,4]

  # generate examples of optimal treatments
  optTx1 <- 2*rbinom(210, 1, 0.4)-1
  optTx2 <- 2*rbinom(210, 1, 0.5)-1

# Plug-in Values
  tx1 <- numeric(nrow(bmiData)) + 1.0
  tx1[bmiData$A1 == "CD"] <- -1.0

  tx2 <- numeric(nrow(bmiData)) + 1.0
  tx2[bmiData$A2 == "CD"] <- -1.0

  plugInValue(optTx1 = optTx1, 
              optTx2 = optTx2, 
              response = bmiData$y,   
              tx1 = tx1,   
              tx2 = tx2)
}
