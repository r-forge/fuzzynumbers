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
   definition=function(
      object,
      method=c("BestEuclidean","ApproximateBestEuclidean","Naive"),
      knot.n=1,
      knot.alpha=0.5,
      optim.control=list(),
#       optim.method=c("Nelder-Mead"),
      verbose=FALSE,
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

         alpha.lower  <- c(0,knot.alpha,1);
         alpha.upper  <- c(1,rev(knot.alpha),0);

         # First we try the "disjoint sides" version

         # constraints
         ui <- matrix(0, nrow=(knot.n+2)-1, ncol=(knot.n+2));
         for (i in 1:((knot.n+2)-1))
         {
            ui[i,i]  <- -1;
            ui[i,i+1] <- 1;
         }
         ci <- rep(0, (knot.n+2)-1);

         
## ================== ApproximateBestEuclidean: PASS 1a: "disjoint" lower optimizer

         if (verbose) cat(sprintf("Pass 1a,"));

         target.lower <- function(res, ...)
            {
#                stopifnot(all(diff(res) >= 0)); # not needed, as we apply linear constraints below

               lower2 <- approxfun(alpha.lower, res, method="linear");  # no ties to specify - knot.alpha is unique
               integrate_discont_val(function(alpha)
                  {
                     (object@a1+(object@a2-object@a1)*object@lower(alpha)-lower2(alpha))^2
                  },
                  0, 1, discontinuities=object@discontinuities.lower, ...);  # Squared L2 - lower
            }

         # ensure that the starting point is not on the constraint region boundary
         start <- start.right0;
         diff_start <- diff(start)
         diff_start[diff_start <= 0] <- start[length(start)]*1e-12;
         start <- cumsum(c(start[1], diff_start));
            
         optres <- constrOptim(start, target.lower, ci=ci, ui=ui,
            method="Nelder-Mead", control=optim.control, ...);
         if (optres$convergence == 1)
            warning("Constrained Nelder-Mead algorithm have not converged [lower] (iteration limit reached)");
         if (optres$convergence >  1)
            warning(paste("Constrained Nelder-Mead algorithm have not converged [lower] (", optres$message, ")", sep=""));
         res.left <- optres$par;

         
## ================== ApproximateBestEuclidean: PASS 1b: "disjoint" upper optimizer

         if (verbose) cat(sprintf("1b,"));
         
         target.upper <- function(res, ...)
            {
#                stopifnot(all(diff(res) >= 0)); # not needed, as we apply linear constraints below

               upper2 <- approxfun(alpha.upper, res, method="linear");
               integrate_discont_val(function(alpha)
                  {
                     (object@a3+(object@a4-object@a3)*object@upper(alpha)-upper2(alpha))^2
                  },
                  0, 1, discontinuities=object@discontinuities.upper, ...);   # Squared L2 - upper
            }

         # ensure that the starting point is not on the constraint region boundary
         start <- start.right0;
         diff_start <- diff(start)
         diff_start[diff_start <= 0] <- start[length(start)]*1e-12;
         start <- cumsum(c(start[1], diff_start));

         optres <- constrOptim(start, target.upper, ci=ci, ui=ui,
            method="Nelder-Mead", control=optim.control, ...);
         if (optres$convergence == 1)
            warning("Constrained Nelder-Mead algorithm have not converged [upper] (iteration limit reached)");
         if (optres$convergence >  1)
            warning(paste("Constrained Nelder-Mead algorithm have not converged [upper] (", optres$message, ")", sep=""));
         res.right <- optres$par;

         
## ================== ApproximateBestEuclidean: try lower+upper
         
         if (res.left[knot.n+2] <= res.right[1])
         {
            if (verbose) cat(sprintf("DONE.\n"));
            
            # the sides are disjoint => this is the "optimal" solution => FINISH
            return(PiecewiseLinearFuzzyNumber(res.left[1], res.left[knot.n+2], res.right[1], res.right[knot.n+2],
               knot.n=knot.n, knot.alpha=knot.alpha,
               knot.left=res.left[-c(1,knot.n+2)], knot.right=res.right[-c(1,knot.n+2)]));
         }


         # All right, if we are here then we have to optimize on all knots altogether...
         # print("DEBUG: not disjoint");
         # Open quesion: can we assume that a2==a3 ??? (currently we do not)

## ================== ApproximateBestEuclidean: PASS 2: use both sides together

         if (verbose) cat(sprintf("2,"));
         
         target <- function(res, ...)
            {
#                stopifnot(all(diff(res) >= 0)); # not needed, as we apply linear constraints below

               lower2 <- approxfun(alpha.lower, res[1:(knot.n+2)], method="linear");  # no ties to specify - knot.alpha is unique
               d2l <- integrate_discont_val(function(alpha)
                  {
                     (object@a1+(object@a2-object@a1)*object@lower(alpha)-lower2(alpha))^2
                  }, 0, 1, discontinuities=object@discontinuities.lower, ...);

               upper2 <- approxfun(alpha.upper, res[-(1:(knot.n+2))], method="linear");  # no ties to specify - knot.alpha is unique
               d2r <- integrate_discont_val(function(alpha)
                  {
                     (object@a3+(object@a4-object@a3)*object@upper(alpha)-upper2(alpha))^2
                  }, 0, 1, discontinuities=object@discontinuities.upper, ...);

               return(d2l+d2r); # Squared L2
            }

         # constraints
         ui <- matrix(0, nrow=2*(knot.n+2)-1, ncol=2*(knot.n+2));
         for (i in 1:(2*(knot.n+2)-1))
         {
            ui[i,i]  <- -1;
            ui[i,i+1] <- 1;
         }
         ci <- rep(0, 2*(knot.n+2)-1);

         # ensure that the starting point is not on the constraint region boundary
         start <- c(start.left0, start.right0);
         diff_start <- diff(start)
         diff_start[diff_start <= 0] <- start[length(start)]*1e-12;
         start <- cumsum(c(start[1], diff_start));
         
         optres <- constrOptim(start, target, ci=ci, ui=ui,
            method="Nelder-Mead", control=optim.control, ...);
         
         if (optres$convergence == 1)
            warning("Constrained Nelder-Mead algorithm have not converged (iteration limit reached)");
         if (optres$convergence >  1)
            warning(paste("Constrained Nelder-Mead algorithm have not converged (", optres$message, ")", sep=""));
         res <- optres$par;

         
         # All right, we're done!
         if (verbose) cat(sprintf("DONE.\n"));
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


         w1 <- integrate_discont_val(function(alpha)
                  {
                     (object@a1+(object@a2-object@a1)*object@lower(alpha))
                  }, 0, knot.alpha, discontinuities=object@discontinuities.lower, ...);

         w3 <- integrate_discont_val(function(alpha)
                  {
                     (object@a1+(object@a2-object@a1)*object@lower(alpha))
                  }, knot.alpha, 1, discontinuities=object@discontinuities.lower, ...);

         w5 <- integrate_discont_val(function(alpha)
                  {
                     (object@a3+(object@a4-object@a3)*object@upper(alpha))
                  }, 0, knot.alpha, discontinuities=object@discontinuities.upper, ...);

         w7 <- integrate_discont_val(function(alpha)
                  {
                     (object@a3+(object@a4-object@a3)*object@upper(alpha))
                  }, knot.alpha, 1, discontinuities=object@discontinuities.upper, ...);


         int2 <- integrate_discont_val(function(alpha)
                  {
                     (object@a1+(object@a2-object@a1)*object@lower(alpha))*alpha
                  }, 0, knot.alpha, discontinuities=object@discontinuities.lower, ...);

         int4 <- integrate_discont_val(function(alpha)
                  {
                     (object@a1+(object@a2-object@a1)*object@lower(alpha))*alpha
                  }, knot.alpha, 1, discontinuities=object@discontinuities.lower, ...);

         int6 <- integrate_discont_val(function(alpha)
                  {
                     (object@a3+(object@a4-object@a3)*object@upper(alpha))*alpha
                  }, 0, knot.alpha, discontinuities=object@discontinuities.upper, ...);

         int8 <- integrate_discont_val(function(alpha)
                  {
                     (object@a3+(object@a4-object@a3)*object@upper(alpha))*alpha
                  }, knot.alpha, 1, discontinuities=object@discontinuities.upper, ...);

         w2 <- int2/knot.alpha;
         w4 <- (int4-knot.alpha*w3)/(1-knot.alpha);
         w6 <- w5-int6/knot.alpha;
         w8 <- (w7-int8)/(1-knot.alpha);
                  
         b <- c(w1+w3+w5+w7,
                w2+w3+w5+w7,
                   w4+w5+w7,
                      w5+w7,
                      w5+w8,
                         w6
                   );



## ================== BestEuclidean: PASS 1: try with z==0

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

#          print(c(v1,v2,v3,v4,v5,v6,v7,v8));
#          print(b)


         # try to find solution assuming z == 0
         if (verbose) cat(sprintf("Pass 1,"));
         d <- PhiInv %*% b;
         
#          print(PhiInv)
#          print(d)
         
         if (all(d[-1] >= 0)) # d[-1] must be nonnegative to be a solution
         {  # We are done!
            res <- cumsum(d);
            if (verbose) cat(sprintf("DONE.\n"));
            return(PiecewiseLinearFuzzyNumber(res[1], res[knot.n+2], res[knot.n+3], res[2*knot.n+4],
               knot.n=knot.n, knot.alpha=knot.alpha, knot.left=res[2:(knot.n+1)], knot.right=res[(knot.n+4):(2*knot.n+3)]));
         }

## ================== BestEuclidean: PASS 2: calculate with z!=0 (d[-1]<0-based)

#          cat(sprintf("DEBUG:        d =%s\n", paste(d, collapse=", ")))
#          cat(sprintf("DEBUG: cumsum(d)=%s\n", paste(cumsum(d), collapse=", ")))


         Phi <- matrix(c(
            2, -(knot.alpha-4)/2, -(knot.alpha-3)/2, 1, (knot.alpha+1)/2, (knot.alpha)/2,
            -(knot.alpha-4)/2, -(2*knot.alpha-6)/3, -(knot.alpha-3)/2, 1, (knot.alpha+1)/2, (knot.alpha)/2,
            -(knot.alpha-3)/2, -(knot.alpha-3)/2, -(knot.alpha-4)/3, 1, (knot.alpha+1)/2, (knot.alpha)/2,
            1, 1, 1, 1, (knot.alpha+1)/2, (knot.alpha)/2,
            (knot.alpha+1)/2, (knot.alpha+1)/2, (knot.alpha+1)/2, (knot.alpha+1)/2, (2*knot.alpha+1)/3, (knot.alpha)/2,
            (knot.alpha)/2, (knot.alpha)/2, (knot.alpha)/2, (knot.alpha)/2, (knot.alpha)/2, (knot.alpha)/3
         ), nrow=6, ncol=6, byrow=TRUE);
#          stopifnot(max(abs(Phi - solve(PhiInv))) < 1e-14);
#          stopifnot(Phi == t(Phi));

         try <- which(d[-1] < 0)+1;
         Phi_try <- Phi;
         Phi_try[,try] <- 0;
         for (i in try) Phi_try[i,i] <- -1;

         if (verbose) cat(sprintf("2,"));
         d <- solve(Phi_try, b);

         if (all(d[-1] >= 0))
         {  # We are done!
            d[try] <- 0; # substitute z >= 0 for d == 0
            res <- cumsum(d);
            if (verbose) cat(sprintf("DONE.\n"));
            return(PiecewiseLinearFuzzyNumber(res[1], res[knot.n+2], res[knot.n+3], res[2*knot.n+4],
               knot.n=knot.n, knot.alpha=knot.alpha, knot.left=res[2:(knot.n+1)], knot.right=res[(knot.n+4):(2*knot.n+3)]));
         }

         try_old <- try;
         
## ================== BestEuclidean: PASS 3: calculate with all possible combinations of z!=0

         iterations <- 3;
         if (verbose) cat(sprintf("3,"));
#          for (i in c(seq.int(1L,31L,by=2),seq.int(2L,31L,by=2)))
         for (i in 1L:31L)
         {
            # generate all 31 nonzero binary sequences of length 5
            # prefer those with 4 set to TRUE
            try <- (bitAnd(i,c(1L,2L,4L,8L,16L))!=0); 
#             try <- try[c(5L,2L,1L,3L,4L)];
            try <- which(try)+1;
            if (length(try) == length(try_old) && all(try == try_old)) next;
            
            Phi_try <- Phi;
            Phi_try[,try] <- 0;
            for (i in try) Phi_try[i,i] <- -1;
            
            d <- solve(Phi_try, b);

            print(try)
            print(Phi_try)
            print(solve(Phi_try))
            print(solve(Phi_try)%*%b)
            print(d)

            if (all(d[-1] >= 0))
            {  # We are done!
               d[try] <- 0; # substitute z >= 0 for d == 0
               res <- cumsum(d);
               if (verbose) cat(sprintf("DONE in %g iterations.\n", iterations));
               return(PiecewiseLinearFuzzyNumber(res[1], res[knot.n+2], res[knot.n+3], res[2*knot.n+4],
                  knot.n=knot.n, knot.alpha=knot.alpha, knot.left=res[2:(knot.n+1)], knot.right=res[(knot.n+4):(2*knot.n+3)]));
            }

            iterations <- iterations + 1;
         }

         warning(sprintf("Could not find solution for knot.alpha=%g!
         This may be due to innacuracy of numerical integration.", knot.alpha));
         return(NULL);
                  
## --------------------------------------------- /BestEuclidean ---------
## ----------------------------------------------------------------------
      }


      # None shall pass here
   }
);

