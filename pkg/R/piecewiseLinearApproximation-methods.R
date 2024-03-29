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
#' \item \code{NearestEuclidean}: see (Coroianu, Gagolewski, Grzegorzewski, 2013),
#' only for \code{knot.n==1}; uses numerical integration, see \code{\link{integrateAlpha}}
#'
#' \item \code{ApproximateNearestEuclidean}: this is done via numeric optimization ("Nelder-Mead" algorithm);
#' uses numerical integration, see \code{\link{integrateAlpha}}
#'
#' }
#' 
#' @param method one of: \code{"NearestEuclidean"}, \code{"ApproximateNearestEuclidean"}, \code{"Naive"}
#' @param verbose logical
#' @param ... further arguments passed to \code{\link{integrateAlpha}}
#' @param knot.n number of knots
#' @param knot.alpha alpha-cuts for knots
#' @param optim.control a list of control params for \code{\link{optim}}
#' 
#' @exportMethod piecewiseLinearApproximation
#' @name piecewiseLinearApproximation
#' @aliases piecewiseLinearApproximation,FuzzyNumber-method
#' @rdname piecewiseLinearApproximation-methods
#' @docType methods
#' @seealso \code{\link{trapezoidalApproximation}}
#' @family FuzzyNumber-method
#' @references
#' Coroianu L., Gagolewski M., Grzegorzewski P. (2013),
#' Nearest Piecewise Linear Approximation of Fuzzy Numbers, to appear in Fuzzy Sets and Systems.\cr
#' @examples
#' (A <- FuzzyNumber(-1,0,1,3,lower=function(x) sqrt(x),upper=function(x) 1-sqrt(x)))
#' (PA <- piecewiseLinearApproximation(A, "NearestEuclidean", knot.n=1, knot.alpha=0.2))
setMethod(
   f="piecewiseLinearApproximation",
   signature(object="FuzzyNumber"),
   definition=function(
      object,
      method=c("NearestEuclidean","ApproximateNearestEuclidean","Naive"),
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


      if (method == "ApproximateNearestEuclidean")
      {
## ----------------------------------------------------------------------
## ----------------------------------- ApproximateNearestEuclidean ---------


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


## ================== ApproximateNearestEuclidean: PASS 1a: "disjoint" lower optimizer

         if (verbose) cat(sprintf("Pass 1a,"));

         target.lower <- function(res, ...)
            {
#                stopifnot(all(diff(res) >= 0)); # not needed, as we apply linear constraints below

               lower2 <- approxfun(alpha.lower, res, method="linear");  # no ties to specify - knot.alpha is unique

               integrateAlpha(object, "lower", 0, 1, # Squared L2 - lower
                  transform=function(alpha, y) (y-lower2(alpha))^2, ...);
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


## ================== ApproximateNearestEuclidean: PASS 1b: "disjoint" upper optimizer

         if (verbose) cat(sprintf("1b,"));

         target.upper <- function(res, ...)
            {
#                stopifnot(all(diff(res) >= 0)); # not needed, as we apply linear constraints below

               upper2 <- approxfun(alpha.upper, res, method="linear");

               integrateAlpha(object, "upper", 0, 1, # Squared L2 - upper
                  transform=function(alpha, y) (y-upper2(alpha))^2, ...);
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


## ================== ApproximateNearestEuclidean: try lower+upper

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

## ================== ApproximateNearestEuclidean: PASS 2: use both sides together

         if (verbose) cat(sprintf("2,"));

         target <- function(res, ...)
            {
#                stopifnot(all(diff(res) >= 0)); # not needed, as we apply linear constraints below

               lower2 <- approxfun(alpha.lower, res[1:(knot.n+2)], method="linear");  # no ties to specify - knot.alpha is unique
               d2l <- integrateAlpha(object, "lower", 0, 1, # Squared L2 - lower
                  transform=function(alpha, y) (y-lower2(alpha))^2, ...);

               upper2 <- approxfun(alpha.upper, res[-(1:(knot.n+2))], method="linear");  # no ties to specify - knot.alpha is unique
               d2r <- integrateAlpha(object, "upper", 0, 1, # Squared L2 - upper
                  transform=function(alpha, y) (y-upper2(alpha))^2, ...);

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




## ---------------------------------- /ApproximateNearestEuclidean ---------
## ----------------------------------------------------------------------
      }


      if (method == "NearestEuclidean")
      {
## ----------------------------------------------------------------------
## ---------------------------------------------- NearestEuclidean ---------

         # This exact method was proposed by Coroianu, Gagolewski, Grzegorzewski (FSS 2013)

         if (knot.n != 1) stop("this method currently may only be used only for knot.n == 1")


         w1   <- integrateAlpha(object, "lower", 0, knot.alpha, ...)
         w3   <- integrateAlpha(object, "lower", knot.alpha, 1, ...)
         w5   <- integrateAlpha(object, "upper", 0, knot.alpha, ...)
         w7   <- integrateAlpha(object, "upper", knot.alpha, 1, ...)
         int2 <- integrateAlpha(object, "lower", 0, knot.alpha, weight=identity, ...)
         int4 <- integrateAlpha(object, "lower", knot.alpha, 1, weight=identity, ...)
         int6 <- integrateAlpha(object, "upper", 0, knot.alpha, weight=identity, ...)
         int8 <- integrateAlpha(object, "upper", knot.alpha, 1, weight=identity, ...)

         w2 <- int2/knot.alpha
         w4 <- (int4-knot.alpha*w3)/(1-knot.alpha)
         w6 <- w5-int6/knot.alpha
         w8 <- (w7-int8)/(1-knot.alpha)

         b <- c(w1+w3+w5+w7,
                w2+w3+w5+w7,
                   w4+w5+w7,
                      w5+w7,
                      w5+w8,
                         w6
                   )


         PhiInv <- matrix(c(

               (knot.alpha+3)/knot.alpha, -(3*knot.alpha+3)/knot.alpha,                                3,                               -1,                                0,                           0,
            -(3*knot.alpha+3)/knot.alpha,  (9*knot.alpha+3)/knot.alpha,                               -9,                                3,                                0,                           0,
                                       3,                           -9, (9*knot.alpha-12)/(knot.alpha-1), -(3*knot.alpha-6)/(knot.alpha-1),                                0,                           0,
                                      -1,                            3, -(3*knot.alpha-6)/(knot.alpha-1),  (2*knot.alpha-8)/(knot.alpha-1), -(3*knot.alpha-6)/(knot.alpha-1),                           3,
                                       0,                            0,                                0, -(3*knot.alpha-6)/(knot.alpha-1), (9*knot.alpha-12)/(knot.alpha-1),                          -9,
                                       0,                            0,                                0,                                3,                               -9, (9*knot.alpha+3)/knot.alpha

         ), nrow=6, ncol=6, byrow=TRUE)


         iter <- 1
         z <- rep(0, 6)
         K <- rep(FALSE, 6)
         d <- as.numeric(PhiInv %*% b)
         m <- which.min(d[-1])+1
         EPS <- 1e-9;

         if (verbose)
         {
            cat(sprintf("Pass  %g: K={%5s}, d=(%s)\n                    z=(%s)\n",
               iter,  paste(as.numeric(which(K)),collapse=""),
               paste(sprintf("%8.2g", d), collapse=", "),
               paste(sprintf("%8.2g", z), collapse=", ")))
         }

         while(d[m] < -EPS)
         {
            K[m] <- TRUE

#             z <- rep(0, 6); # for better accuracy?
#             d <- as.numeric(PhiInv %*% b);  # for better accuracy?

            deltaz <- rep(0.0, 6)
            deltaz[K] <- as.numeric(solve(PhiInv[K,K], -d[K], tol=.Machine$double.eps))
            if (min(deltaz[K]) < -EPS) warning(sprintf("min(deltaz[K])==%g", min(deltaz[K])))

            z <- z+deltaz

            d <- as.numeric(PhiInv%*%(b+z))
#             for (k in which(K)) d <- d+PhiInv[k,]*deltaz[k] # ALTERNATIVE, BUT MORE INACCURATE

            m <- which.min(d[-1])+1
            iter <- iter+1

            stopifnot(all(z>=0))
            if (max(abs(d[K])) > EPS) warning(sprintf("max(abs(d[K]))==%g", max(abs(d[K]))))
            d[K] <- 0.0 # for better accuracy

            if (verbose)
            {
               cat(sprintf("Pass  %g: K={%5s}, d=(%s)\n                    z=(%s)\n",
                  iter,  paste(as.numeric(which(K)),collapse=""),
                  paste(sprintf("%8.2g", d), collapse=", "),
                  paste(sprintf("%8.2g", z), collapse=", ")))
            }
         }

         d[c(F,T,T,T,T,T) & (d < 0)] <- 0.0; # kill EPS-error
         res <- cumsum(d)
         return(PiecewiseLinearFuzzyNumber(res[1], res[knot.n+2], res[knot.n+3], res[2*knot.n+4],
               knot.n=knot.n, knot.alpha=knot.alpha, knot.left=res[2:(knot.n+1)], knot.right=res[(knot.n+4):(2*knot.n+3)]))


# ## ================== OLD NearestEuclidean: PASS 1: try with z==0
#
#
#          # try to find solution assuming z == 0
#          d <- PhiInv %*% b;
#          if (verbose) cat(sprintf("Pass  1: K={     }, d=(%s)\n", paste(sprintf("%8.2g", d), collapse=", ")));
#
# #          print(PhiInv)
# #          print(d)
#
#          if (all(d[-1] >= -.Machine$double.eps)) # allow a small numeric EPS-error
#          {  # We are done!
#             d[c(F,T,T,T,T,T) & (d < 0)] <- 0.0; # kill EPS-error
#             res <- cumsum(d);
#             if (verbose) cat(sprintf("DONE.\n"));
#             return(PiecewiseLinearFuzzyNumber(res[1], res[knot.n+2], res[knot.n+3], res[2*knot.n+4],
#                knot.n=knot.n, knot.alpha=knot.alpha, knot.left=res[2:(knot.n+1)], knot.right=res[(knot.n+4):(2*knot.n+3)]));
#          }
#
# ## ================== OLD NearestEuclidean: PASS 2: calculate with z!=0 (d[-1]<0-based)
#
# #          cat(sprintf("DEBUG:        d =%s\n", paste(d, collapse=", ")))
# #          cat(sprintf("DEBUG: cumsum(d)=%s\n", paste(cumsum(d), collapse=", ")))
#
#
#          Phi <- matrix(c(
#             2, -(knot.alpha-4)/2, -(knot.alpha-3)/2, 1, (knot.alpha+1)/2, (knot.alpha)/2,
#             -(knot.alpha-4)/2, -(2*knot.alpha-6)/3, -(knot.alpha-3)/2, 1, (knot.alpha+1)/2, (knot.alpha)/2,
#             -(knot.alpha-3)/2, -(knot.alpha-3)/2, -(knot.alpha-4)/3, 1, (knot.alpha+1)/2, (knot.alpha)/2,
#             1, 1, 1, 1, (knot.alpha+1)/2, (knot.alpha)/2,
#             (knot.alpha+1)/2, (knot.alpha+1)/2, (knot.alpha+1)/2, (knot.alpha+1)/2, (2*knot.alpha+1)/3, (knot.alpha)/2,
#             (knot.alpha)/2, (knot.alpha)/2, (knot.alpha)/2, (knot.alpha)/2, (knot.alpha)/2, (knot.alpha)/3
#          ), nrow=6, ncol=6, byrow=TRUE);
# #          stopifnot(max(abs(Phi - solve(PhiInv))) < 1e-14);
# #          stopifnot(Phi == t(Phi));
#
#          try <- which(d[-1] < 0)+1;
#          Phi_try <- Phi;
#          Phi_try[,try] <- 0;
#          for (i in try) Phi_try[i,i] <- -1;
#
#          d <- solve(Phi_try, b);
#          if (verbose) cat(sprintf("Pass  2: K={%5s}, d=(%s)\n",
#             paste(try,collapse=""),
#             paste(sprintf("%8.2g", d), collapse=", ")));
#
#          if (all(d[-1] >= -.Machine$double.eps)) # allow a small numeric EPS-error
#          {  # We are done!
#             d[c(F,T,T,T,T,T) & (d < 0)] <- 0.0; # kill EPS-error
#             d[try] <- 0; # substitute z >= 0 for d == 0
#             res <- cumsum(d);
#             if (verbose) cat(sprintf("DONE.\n"));
#             return(PiecewiseLinearFuzzyNumber(res[1], res[knot.n+2], res[knot.n+3], res[2*knot.n+4],
#                knot.n=knot.n, knot.alpha=knot.alpha, knot.left=res[2:(knot.n+1)], knot.right=res[(knot.n+4):(2*knot.n+3)]));
#          }
#
#          try_old <- try;
#
# ## ================== OLD NearestEuclidean: PASS 3: calculate with all possible combinations of z!=0
#
#          iterations <- 3;
#
#          for (i in 1L:31L)
# #          for (i in c(seq.int(1L,31L,by=2),seq.int(2L,31L,by=2)))
#          {
#             # generate all 31 nonzero binary sequences of length 5
#             try <- (bitAnd(i,c(1L,2L,4L,8L,16L))!=0);
# #             try <- try[c(5L,2L,1L,3L,4L)];  # prefer those with 4 set to TRUE
#             try <- which(try)+1;
#             if (length(try) == length(try_old) && all(try == try_old)) next;
#
#             Phi_try <- Phi;
#             Phi_try[,try] <- 0;
#             for (i in try) Phi_try[i,i] <- -1;
#
#             d <- solve(Phi_try, b);
#             if (verbose) cat(sprintf("Pass %2g: K={%5s}, d=(%s)\n",
#                iterations,
#                paste(try,collapse=""),
#                paste(sprintf("%8.2g", d), collapse=", ")));
#
# #             print(try)
# #             print(Phi_try)
# #             print(solve(Phi_try))
# #             print(solve(Phi_try)%*%b)
# #             print(d)
#
#             if (all(d[-1] >= -.Machine$double.eps)) # allow a small numeric EPS-error
#             {  # We are done!
#                d[c(F,T,T,T,T,T) & (d < 0)] <- 0.0; # kill EPS-error
#                d[try] <- 0; # substitute z >= 0 for d == 0
#                res <- cumsum(d);
#                if (verbose) cat(sprintf("DONE in %g iterations.\n", iterations));
#                return(PiecewiseLinearFuzzyNumber(res[1], res[knot.n+2], res[knot.n+3], res[2*knot.n+4],
#                   knot.n=knot.n, knot.alpha=knot.alpha, knot.left=res[2:(knot.n+1)], knot.right=res[(knot.n+4):(2*knot.n+3)]));
#             }
#
#             iterations <- iterations + 1;
#          }
#
#          warning(sprintf("Could not find solution for knot.alpha=%g!
#          This may be due to innacuracy of numerical integration.", knot.alpha));
#          return(NULL);

## --------------------------------------------- /NearestEuclidean ---------
## ----------------------------------------------------------------------
      }


      # None shall pass here
   }
)

