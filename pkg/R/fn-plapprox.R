## This file is part of the FuzzyNumbers library.
##
## Copyright 2012 Marek Gagolewski
##
##
## FuzzyNumbers is free software: you can redistribute it and/or modify
## it under the terms of the GNU Lesser General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## FuzzyNumbers is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.See the
## GNU Lesser General Public License for more details.
##
## You should have received a copy of the GNU Lesser General Public License
## along with FuzzyNumbers. If not, see <http://www.gnu.org/licenses/>.


setGeneric("piecewiseLinearApproximation", function(object, ...) standardGeneric("piecewiseLinearApproximation"));

#' Piecewise linear approximation of a fuzzy number
#'
#' This method finds a piecewise linear approximation \eqn{P(A)}
#' of a given fuzzy number \eqn{A} by using the algorithm specified by the
#' \code{method} parameter.
#'
#' \code{method} may be one of:
#' \enumerate{
#' \item \code{Naive}:
#' We have core(A)==core(T(A)) and supp(A)==supp(T(A)) and the knots are
#' taken directly from the specified alpha cuts (linear interpolation).
#' }
#'
#' @examples
#' (A <- FuzzyNumber(-1,0,1,3,lower=function(x) sqrt(x),upper=function(x) 1-sqrt(x)))
#' (PA <- piecewiseLinearApproximation(A, "Naive"))
#' 
#' @exportMethod piecewiseLinearApproximation
setMethod(
   f="piecewiseLinearApproximation",
   signature(object="FuzzyNumber"),
   definition=function(object, method=c("BestEuclidean","ApproximateBestEuclidean","Naive"),
      knot.n=1, knot.alpha=0.5,
#       expected.interval=NULL, alpha.interval=NULL,
      optim.control=list(),
      ...)
   {
      method <- match.arg(method);

      if (!is.numeric(knot.n) || length(knot.n) != 1 || knot.n <= 0)
         stop("`knot.n' should be >= 1");

      if (length(knot.alpha) != knot.n || !is.numeric(knot.alpha) || any(!is.finite(knot.alpha) | knot.alpha <= 0 | knot.alpha >= 1))
         stop("incorrect `knot.alpha'");

      if (is.na(object@lower(0)) || is.na(object@upper(0)))
         stop("cannot approximate fuzzy numbers with no alpha bound generators");
      
      if (method == "Naive")
      {
         a <- alphacut(object, knot.alpha);

         if (knot.n > 1)
         {
            knot.left  <- a[,1];
            knot.right <- rev(a[,2]);
         } else
         {
            knot.left  <- a[1];
            knot.right <- a[2];
         }
         
         return(PiecewiseLinearFuzzyNumber(object@a1, object@a2, object@a3, object@a4,
            knot.n=knot.n, knot.alpha=knot.alpha, knot.left=knot.left, knot.right=knot.right));
      } else if (method == "ApproximateBestEuclidean")
      {
         a <- alphacut(object, knot.alpha);

         if (knot.n > 1)
         {
            start.left0  <- a[,1];
            start.right0 <- rev(a[,2]);
         } else
         {
            start.left0  <- a[1];
            start.right0 <- a[2];
         }

         res <- c(object@a1, start.left0, object@a2, object@a3, start.right0, object@a4);

         # reparametrize: (a1, DELTA)
         res <- c(res[1], diff(res));

         target <- function(res, ...)
            {
               res <- cumsum(res);
               distance(object,
                  PiecewiseLinearFuzzyNumber(res[1], res[knot.n+2], res[knot.n+3], res[2*knot.n+4],
                     knot.n=knot.n, knot.alpha=knot.alpha,
                     knot.left=res[2:(knot.n+1)], knot.right=res[(knot.n+4):(2*knot.n+3)]),
                  type="EuclideanSquared", ...);
            }

         optres <- optim(res, target, ...,
            method="L-BFGS-B", lower=c(2*object@a1, rep(0, 2*knot.n+3)), control=optim.control);

         if (optres$convergence != 0)
            warning(paste("L-BFGS-B algorithm have not converged (", optres$message, ")", sep=""));

#          optres <- cma_es(res, target, ...,
#             lower=c(2*object@a1, rep(0, 2*knot.n+3)));

#          print(optres); # this may be printed in verbose mode

         res <- optres$par;

         # undo reparametrization:
         res <- cumsum(res);
         

         return(PiecewiseLinearFuzzyNumber(res[1], res[knot.n+2], res[knot.n+3], res[2*knot.n+4],
            knot.n=knot.n, knot.alpha=knot.alpha, knot.left=res[2:(knot.n+1)], knot.right=res[(knot.n+4):(2*knot.n+3)]));
      
      } else if (method == "BestEuclidean")
      {
         # This exact method was proposed by Coroianu, Gagolewski, Grzegorzewski (submitted)
         
         if (knot.n != 1) stop("this method currently may only be used only for knot.n == 1");

         
      }

#       if (!is.numeric(expected.interval) || length(expected.interval) != 2 || any(!is.finite(expected.interval)))
#       {
#          if (is.na(object@lower(0)) || is.na(object@upper(0)))
#             stop("Integral of alphacut bounds cannot be computed");
#             
#          expected.interval <- expectedInterval(object, ...);
#       }
# 
#       if (!is.numeric(alpha.interval) || length(alpha.interval) != 2 || any(!is.finite(alpha.interval)))
#       {
#          if (is.na(object@lower(0)) || is.na(object@upper(0)))
#             stop("Integral of alphacut bounds cannot be computed");
# 
#          alpha.interval <- alphaInterval(object, ...);
#       }
#       
#       intLower <- expected.interval[1];
#       intUpper <- expected.interval[2];
#       intAlphaTimesLower <- alpha.interval[1];
#       intAlphaTimesUpper <- alpha.interval[2];
#       
#       if (method == "ExpectedIntervalPreserving")
#       {
#          # Here we use the method given in (Grzegorzewski, 2010)
#          
#          if (intAlphaTimesUpper-intAlphaTimesLower >= (intUpper-intLower)/3 )
#             # i.e. if ambiguity(A) >= width(A)/3
#          {
#             a1 <-  4*intLower-6*intAlphaTimesLower;
#             a2 <- -2*intLower+6*intAlphaTimesLower;
#             a3 <- -2*intUpper+6*intAlphaTimesUpper;
#             a4 <-  4*intUpper-6*intAlphaTimesUpper;
#          } else
#          {
#             Eval13 <- 2*intLower/3 +   intUpper/3; # Weighted Expected Value w=1/3
#             Eval23 <-   intLower/3 + 2*intUpper/3; # Weighted Expected Value w=2/3
#             Val <- intAlphaTimesLower + intAlphaTimesUpper; # Value
#             
#             if (Eval13 <= Val && Val <= Eval23)
#             {
#                a1 <-       3*intLower +   intUpper - 3*intAlphaTimesLower - 3*intAlphaTimesUpper;
#                a2 <- a3 <-  -intLower -   intUpper + 3*intAlphaTimesLower + 3*intAlphaTimesUpper;
#                a4 <-         intLower + 3*intUpper - 3*intAlphaTimesLower - 3*intAlphaTimesUpper;
#             } else if (Val < Eval13)
#             {
#                a1 <- a2 <- a3 <- intLower;
#                a4 <- 2*intUpper - intLower;
#             } else
#             {
#                a1 <- 2*intLower - intUpper;
#                a2 <- a3 <- a4 <- intUpper;
#             }
#          }
#          
#          return(TrapezoidalFuzzyNumber(a1, a2, a3, a4));
#          
#       } else if (method == "SupportCoreRestricted")
#       {
#          # Here we use the method given in (Grzegorzewski, Pasternak-Winiarska, 2011)
#       
#          u1 <- 4*intLower - 6*intAlphaTimesLower;
#          u2 <- 6*intAlphaTimesLower - 2*intLower;
#          if (object@a2 > u2)
#          {
#             if (object@a1 < u1)
#             {
#                a1 <- u1;
#                a2 <- u2;
#             } else
#             {
#                a1 <- object@a1;
#                a2 <- 2*intLower-object@a1;
#             }
#          } else
#          {
#             if (object@a1 < u1)
#             {
#                a1 <- 2*intLower-object@a2;
#                a2 <- object@a2;
#             } else
#             {
#                a1 <- object@a1;
#                a2 <- object@a2;
#             }
#          }
#          
#          
#          u3 <- 6*intAlphaTimesUpper - 2*intUpper;
#          u4 <- 4*intUpper - 6*intAlphaTimesUpper;
#          if (object@a4 > u4)
#          {
#             if (object@a3 < u3)
#             {
#                a3 <- u3;
#                a4 <- u4;
#             } else
#             {
#                a3 <- object@a3;
#                a4 <- 2*intUpper-object@a3;
#             }
#          } else
#          {
#             if (object@a3 < u3)
#             {
#                a3 <- 2*intUpper-object@a4;
#                a4 <- object@a4;
#             } else
#             {
#                a3 <- object@a3;
#                a4 <- object@a4;
#             }
#          }
#          
#          return(TrapezoidalFuzzyNumber(a1, a2, a3, a4));
#       }
   }
);

