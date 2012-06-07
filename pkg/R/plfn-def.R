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


#' S4 class representing a piecewise linear fuzzy number
#'
#' TO DO
#' @exportClass PiecewiseLinearFuzzyNumber
#' @name PiecewiseLinearFuzzyNumber-class
#' @docType class
setClass(
   Class="PiecewiseLinearFuzzyNumber",
   representation(
      knot.n="numeric",
      knot.alpha="numeric",
      knot.left="numeric",
      knot.right="numeric"
   ),
   prototype=prototype(
      left=function(x)  NA,
      right=function(x) NA,
      lower=function(alpha) NA,
      upper=function(alpha) NA
   ),
   validity=function(object)
   {
#       print("DEBUG: Validity call for PiecewiseLinearFuzzyNumber");
      
      if (object@knot.n < 0) return("`knot.n' should be >= 0");
      if (object@knot.n != length(object@knot.alpha)) return("length of `knot.alpha' should be equal to `knot.n'");
      if (object@knot.n != length(object@knot.left))  return("length of `knot.left' should be equal to `knot.n'");
      if (object@knot.n != length(object@knot.right)) return("length of `knot.right' should be equal to `knot.n'");

      if (object@knot.n > 0)
      {
         if (is.unsorted(object@knot.left))  return("`knot.left' should be sorted nondecreasingly");
         if (is.unsorted(object@knot.right)) return("`knot.right' should be sorted nondecreasingly");

         if (!is.finite(object@knot.left)  || any(object@knot.left < object@a1 | object@knot.left > object@a2))
            return("`knot.left' should be a vector with elements in [a1,a2]");
         if (!is.finite(object@knot.right) || any(object@knot.right < object@a3 | object@knot.left > object@a4))
            return("`knot.left' should be a vector with elements in [a3,a4]");

         if (!is.finite(object@knot.alpha) || any(object@knot.alpha < 0 | object@knot.alpha > 1))
            return("`knot.alpha' should be a vector with elements in [0,1]");
      }
      
      return(TRUE);
   },
   contains="FuzzyNumber"
);


setMethod(
   f="initialize",
   signature("PiecewiseLinearFuzzyNumber"),
   definition=function(.Object, ...)
   {
#  print("DEBUG: Initialization call for PiecewiseLinearFuzzyNumber");
      .Object <- callNextMethod();

      kl <- c(0,(.Object@knot.left -.Object@a1)/(.Object@a2-.Object@a1),1);
      kr <- c(0,(.Object@knot.right-.Object@a3)/(.Object@a4-.Object@a3),1);

      al <- c(0,.Object@knot.alpha,1);
      ar <- c(1,rev(.Object@knot.alpha),0);

      .Object@left  <- approxfun(kl, al, method="linear", yleft=NA, yright=NA);
      .Object@right <- approxfun(kr, ar, method="linear", yleft=NA, yright=NA);
      .Object@lower <- approxfun(al, kl, method="linear", yleft=NA, yright=NA);
      .Object@upper <- approxfun(ar, kr, method="linear", yleft=NA, yright=NA);

      return(.Object);
   }
);


#' Creates a piecewise linear fuzzy number
#'   
#' For convenience, objects of class \code{PiecewiseLinearFuzzyNumber}
#' may be created with the following function.
#' @param a1 TO DO
#' @param a2 TO DO
#' @param a3 TO DO
#' @param a4 TO DO
#' @param knot.n
#' @param knot.alpha
#' @param knot.left
#' @param knot.right
#' @export
PiecewiseLinearFuzzyNumber <- function(a1, a2, a3, a4,
   knot.n=0, knot.alpha=numeric(0),
   knot.left=numeric(0), knot.right=numeric(0))
{
   .Object <- new("PiecewiseLinearFuzzyNumber", a1=a1, a2=a2, a3=a3, a4=a4,
         knot.n=knot.n, knot.alpha=knot.alpha, knot.left=knot.left, knot.right=knot.right);
   .Object;
}



#' TO DO
#'
#' @export
#' @rdname show-methods
#' @docType methods
setMethod(
   f="show",
   signature(object="PiecewiseLinearFuzzyNumber"),
   definition=function(object) {
      cat(sprintf("Piecewise linear fuzzy number with %g knot(s), support=[%g,%g] and core=[%g,%g].\n",
                  object@knot.n, object@a1, object@a4, object@a2, object@a3));
   }
);


#' TO DO
#'
#' @exportMethod [
setMethod(
   f="[",
   signature=(x="PiecewiseLinearFuzzyNumber"),
   definition=function(x, i, j, drop)
   {
      if (i == "knot.n")     return(x@knot.n);
      if (i == "knot.alpha") return(x@knot.alpha);
      if (i == "knot.left")  return(x@knot.left);
      if (i == "knot.right") return(x@knot.right);

      if (i == "a1") return(x@a1);
      if (i == "a2") return(x@a2);
      if (i == "a3") return(x@a3);
      if (i == "a4") return(x@a4);
      if (i == "left")  return(x@left);
      if (i == "right") return(x@right);
      if (i == "lower") return(x@lower);
      if (i == "upper") return(x@upper);

#     return(callNextMethod()); # does not work...
   }
);