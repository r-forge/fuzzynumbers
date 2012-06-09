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
#       optim.method=c("Nelder-Mead"),
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
## ----------------------------------------------------------------------
## ------------------------------------------------------ Naive ---------

         # naive piecewise linear interpolation of points
         # at given alpha-cuts, preserving original core and support
         
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

## ----------------------------------------------------- /Naive ---------
## ----------------------------------------------------------------------
      }


      if (method == "ApproximateBestEuclidean")
      {
## ----------------------------------------------------------------------
## ----------------------------------- ApproximateBestEuclidean ---------


         # Get the starting point ==> Naive approximator
         a <- alphacut(object, knot.alpha);

         if (knot.n > 1)
         {
            start.left0  <- c(object@a1,     a[,1] , object@a2);
            start.right0 <- c(object@a3, rev(a[,2]), object@a4);
         } else
         {
            start.left0  <- c(object@a1, a[1], object@a2);
            start.right0 <- c(object@a3, a[2], object@a4);
         }

         alpha.left   <- c(0,knot.alpha,1);
         alpha.right  <- c(1,rev(knot.alpha),0);

         # First we try the "disjoint sides" version

         
## ================== ApproximateBestEuclidean: PASS 1a: "disjoint" left optimizer

         target.left <- function(res, ...)
            {
               if (any(diff(res) < 0)) return(NA);
               left <- approxfun(alpha.left, res, method="linear");
               integrate(function(alpha)
                  {
                     (object@a1+(object@a2-object@a1)*object@lower(alpha)-left(alpha))^2
                  },
                  0, 1, ...)$value;  # Squared L2 - left
            }

         optres <- optim(start.left0, target.left, ..., method="Nelder-Mead", control=optim.control);
         if (optres$convergence == 1)
            warning("Nelder-Mead algorithm have not converged [left side] (iteration limit reached)");
         if (optres$convergence >  1)
            warning(paste("Nelder-Mead algorithm have not converged [left side] (", optres$message, ")", sep=""));
         res.left <- optres$par;

         
## ================== ApproximateBestEuclidean: PASS 1b: "disjoint" right optimizer
         
         target.right <- function(res, ...)
            {
               if (any(diff(res) < 0)) return(NA);
               right <- approxfun(alpha.right, res, method="linear");
               integrate(function(alpha) 
                  {
                     (object@a3+(object@a4-object@a3)*object@upper(alpha)-right(alpha))^2
                  },
                  0, 1, ...)$value;   # Squared L2 - right
            }


         optres <- optim(start.right0, target.right, ..., method="Nelder-Mead", control=optim.control);
         if (optres$convergence == 1)
            warning("Nelder-Mead algorithm have not converged [right side] (iteration limit reached)");
         if (optres$convergence >  1)
            warning(paste("Nelder-Mead algorithm have not converged [right side] (", optres$message, ")", sep=""));
         res.right <- optres$par;

         
## ================== ApproximateBestEuclidean: try left+right
         
         if (res.left[knot.n+2] <= res.right[1])
         {
            # the sides are disjoint => this is the "optimal" solution => FINISH
            return(PiecewiseLinearFuzzyNumber(res.left[1], res.left[knot.n+2], res.right[1], res.right[knot.n+2],
               knot.n=knot.n, knot.alpha=knot.alpha,
               knot.left=res.left[-c(1,knot.n+2)], knot.right=res.right[-c(1,knot.n+2)]));
         }


         # All right, if we are here then we have to optimize on all knots altogether...
         # print("DEBUG: not disjoint");
         # Open quesion: can we assume that a2==a3 ??? (currently we do not)

## ================== ApproximateBestEuclidean: PASS 2: use both sides together
            
         target <- function(res, ...)
            {
               if (any(diff(res) < 0)) return(NA);

               left <- approxfun(alpha.left, res[1:(knot.n+2)], method="linear");
               d2l <- integrate(function(alpha)
                  {
                     (object@a1+(object@a2-object@a1)*object@lower(alpha)-left(alpha))^2
                  }, 0, 1, ...)$value;

               right <- approxfun(alpha.right, res[-(1:(knot.n+2))], method="linear");
               d2r <- integrate(function(alpha)
                  {
                     (object@a3+(object@a4-object@a3)*object@upper(alpha)-right(alpha))^2
                  }, 0, 1, ...)$value;

               return(d2l+d2r); # Squared L2
            }

         optres <- optim(c(start.left0, start.right0), target, ..., method="Nelder-Mead", control=optim.control);
         if (optres$convergence == 1)
            warning("Nelder-Mead algorithm have not converged (iteration limit reached)");
         if (optres$convergence >  1)
            warning(paste("Nelder-Mead algorithm have not converged (", optres$message, ")", sep=""));
         res <- optres$par;

         
         # All right, we're done!
         return(PiecewiseLinearFuzzyNumber(res[1], res[knot.n+2], res[knot.n+3], res[2*knot.n+4],
            knot.n=knot.n, knot.alpha=knot.alpha, knot.left=res[2:(knot.n+1)], knot.right=res[(knot.n+4):(2*knot.n+3)]));

# ALTERNATIVE: The L-BFGS-B method (in reparametrized input space) - sometimes worse
#          # reparametrize: (a1, DELTA)
#          res <- c(res[1], diff(res));
# 
#          target <- function(res, ...)
#             {
#                res <- cumsum(res);
#                distance(object,
#                   PiecewiseLinearFuzzyNumber(res[1], res[knot.n+2], res[knot.n+3], res[2*knot.n+4],
#                      knot.n=knot.n, knot.alpha=knot.alpha,
#                      knot.left=res[2:(knot.n+1)], knot.right=res[(knot.n+4):(2*knot.n+3)]),
#                   type="EuclideanSquared", ...);
#             }
# 
#          optres <- optim(res, target, ...,
#             method="L-BFGS-B", lower=c(2*object@a1, rep(0, 2*knot.n+3)), control=optim.control);
# 
#          if (optres$convergence != 0)
#             warning(paste("L-BFGS-B algorithm have not converged (", optres$message, ")", sep=""));
# 
#          optres <- cma_es(res, target, ...,               # another try: CMA-ES (global optimizer, slow as hell)
#             lower=c(2*object@a1, rep(0, 2*knot.n+3)));
# 
# #          print(optres); # this may be printed out in verbose mode
# 
#          res <- optres$par;
# 
#          # undo reparametrization:
#          res <- cumsum(res);
         



## ---------------------------------- /ApproximateBestEuclidean ---------
## ----------------------------------------------------------------------
      }


      if (method == "BestEuclidean")
      {
## ----------------------------------------------------------------------
## ---------------------------------------------- BestEuclidean ---------

         # This exact method was proposed by Coroianu, Gagolewski, Grzegorzewski (submitted)
         
         if (knot.n != 1) stop("this method currently may only be used only for knot.n == 1");


         PhiInv <- matrix(c(

               (knot.alpha+3)/knot.alpha, -(3*knot.alpha+3)/knot.alpha,                                3,                               -1,                                0,                           0,
            -(3*knot.alpha+3)/knot.alpha,  (9*knot.alpha+3)/knot.alpha,                               -9,                                3,                                0,                           0,
                                       3,                           -9, (9*knot.alpha-12)/(knot.alpha-1), -(3*knot.alpha-6)/(knot.alpha-1),                                0,                           0,
                                      -1,                            3, -(3*knot.alpha-6)/(knot.alpha-1),  (2*knot.alpha-8)/(knot.alpha-1), -(3*knot.alpha-6)/(knot.alpha-1),                           3,
                                       0,                            0,                                0, -(3*knot.alpha-6)/(knot.alpha-1), (9*knot.alpha-12)/(knot.alpha-1),                          -9,
                                       0,                            0,                                0,                                3,                               -9, (9*knot.alpha+3)/knot.alpha

         ), nrow=6, ncol=6, byrow=TRUE);

#          print(PhiInv);
#          print(solve(PhiInv));
#          stopifnot(PhiInv == t(PhiInv));

         v1 <- integrate(function(alpha)
                  {
                     (object@a1+(object@a2-object@a1)*object@lower(alpha))
                  }, 0, knot.alpha, ...)$val;

         v3 <- integrate(function(alpha)
                  {
                     (object@a1+(object@a2-object@a1)*object@lower(alpha))
                  }, knot.alpha, 1, ...)$val;

         v5 <- integrate(function(alpha)
                  {
                     (object@a3+(object@a4-object@a3)*object@upper(alpha))
                  }, 0, knot.alpha, ...)$val;

         v7 <- integrate(function(alpha)
                  {
                     (object@a3+(object@a4-object@a3)*object@upper(alpha))
                  }, knot.alpha, 1, ...)$val;


         v2 <- integrate(function(alpha)
                  {
                     (object@a1+(object@a2-object@a1)*object@lower(alpha))*(alpha/knot.alpha)
                  }, 0, knot.alpha, ...)$val;

         v4 <- integrate(function(alpha)
                  {
                     (object@a1+(object@a2-object@a1)*object@lower(alpha))*((alpha-knot.alpha)/(1-knot.alpha))
                  }, knot.alpha, 1, ...)$val;

         v6 <- integrate(function(alpha)
                  {
                     (object@a3+(object@a4-object@a3)*object@upper(alpha))*((knot.alpha-alpha)/knot.alpha)
                  }, 0, knot.alpha, ...)$val;

         v8 <- integrate(function(alpha)
                  {
                     (object@a3+(object@a4-object@a3)*object@upper(alpha))*((alpha-1)/(knot.alpha-1))
                  }, knot.alpha, 1, ...)$val;

         b <- c(v1+v3+v5+v7,
                v2+v3+v5+v7,
                   v4+v5+v7,
                      v5+v7,
                      v5+v8,
                         v6
                   );

## ================== BestEuclidean: PASS 1: try with z==0

         d <- PhiInv %*% b;

         if (all(d[-1] >= 0))
         {  # We are done!
            res <- cumsum(d);
            return(PiecewiseLinearFuzzyNumber(res[1], res[knot.n+2], res[knot.n+3], res[2*knot.n+4],
               knot.n=knot.n, knot.alpha=knot.alpha, knot.left=res[2:(knot.n+1)], knot.right=res[(knot.n+4):(2*knot.n+3)]));
         }

## ================== BestEuclidean: PASS 2: calculate with z!=0
         
         cat(sprintf("DEBUG:        d =%s\n", paste(d, collapse=", ")))
         cat(sprintf("DEBUG: cumsum(d)=%s\n", paste(cumsum(d), collapse=", ")))

         return(NULL);
                  
## --------------------------------------------- /BestEuclidean ---------
## ----------------------------------------------------------------------
      }


      # None shall pass here
   }
);

