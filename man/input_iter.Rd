\name{iter}
\alias{iter}

\title{
Defining the iter input variable
}

\description{
Several of the statistical methods implemented in package
\pkg{DynTxRegime} allow for an iterative algorithm
when completing an outcome regression. 
This section details how this
input is to be defined.
}

\details{

  Outcome regression models are specified by the main effects components 
  (\code{moMain}) and the contrasts component (\code{moCont}). 
  Assuming that the  
  treatment is denoted as binary A, the full regression model is:  
  moMain + A*moCont. There are two ways to fit this model: (i)  
  in the full model formulation (moMain + A*moCont) or (ii) each  
  component, \code{moMain} and \code{moCont}, is fit separately. 
  \code{iter} specifies  
  if (i) or (ii) should be used.
                  

  \code{iter} >= 1 indicates that \code{moMain} and \code{moCont}
   are to be
  fit separately using an iterative algorithm. 
  \code{iter} is the maximum number of iterations.
  Assume Y = Ymain + Ycont;
  the iterative algorithm is as follows:

                  (1) hat(Ycont) = 0;

                  (2) Ymain = Y - hat(Ycont);

                  (3) fit Ymain ~ moMain;

                  (4) set Ycont = Y - hat(Ymain)

                  (5) fit Ycont ~ A*moCont;

                  (6) Repeat steps (2) - (5) until convergence or
                  a maximum of iter iterations. 

  This choice allows the user to specify, for example,
  a linear main effects component and a non-linear
  contrasts component.

  \code{iter} <= 0 indicates that the full model formulation is to be
  used. The components \code{moMain} and \code{moCont} will be 
  combined in the package and fit as a single object.
  Note that if \code{iter} <= 0, all non-model components of
  \code{moMain} and \code{moCont} must be identical. Specifically,
  the regression method and any non-default arguments
  should be identical. 
  By default, the specifications in \code{moMain} are used.
}


