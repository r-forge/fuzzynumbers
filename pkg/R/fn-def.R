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


#' S4 class representing a fuzzy number
#'
#'
#' Formally, a fuzzy number \eqn{A} (Dubois, Prade, 1978) is a fuzzy subset of the
#' real line \eqn{R}
#' with membership function \eqn{\mu} given by:
#' \tabular{lll}{
#'                    \tab | \eqn{0}                      \tab if \eqn{x    < a1}, \cr
#'                    \tab | \eqn{left((x-a1)/(a2-a1))}   \tab if \eqn{a1 \le x  < a2}, \cr
#' \eqn{\mu(x)}     = \tab | \eqn{1}                      \tab if \eqn{a2 \le x  \le a3}, \cr
#'                    \tab | \eqn{right((x-a3)/(a4-a3))}  \tab if \eqn{a3   < x  \le a4}, \cr
#'                    \tab | \eqn{0}                      \tab if \eqn{a4   < x}, \cr
#' }
#' where \eqn{a1,a2,a3,a4\in R}, \eqn{a1 \le a2 \le a3 \le a4},
#' \eqn{left: [0,1]\to[0,1]}{left: [0,1]->[0,1]} is a nondecreasing function
#' called the \emph{left side of \eqn{A}},
#' and \eqn{right: [0,1]\to[0,1]}{right: [0,1]->[1,0]} is a nonincreasing function
#' called the \emph{right side of \eqn{A}}.
#' \cr
#' Alternatively, it may be shown that each fuzzy number \eqn{A} may be uniquely determined
#' by specifying its \eqn{\alpha}-cuts, \eqn{A(\alpha)}. We have \eqn{A(0)=[a1,a4]} and
#' \deqn{A(\alpha)=[a1+(a2-a1)*lower(\alpha), a3+(a4-a3)*upper(\alpha)]}
#' for \eqn{0<\alpha\le 1}, where \eqn{lower: [0,1]\to[0,1]}{lower: [0,1]->[0,1]}
#' and \eqn{upper: [0,1]\to[0,1]}{upper: [0,1]->[1,0]}
#' are, respectively, strictly increasing and decreasing functions
#' satisfying \eqn{lower(\alpha)=\inf\{x: \mu(x)\ge\alpha\}}{lower(\alpha)=inf(x: \mu(x)\ge\alpha)}
#' and  \eqn{upper(\alpha)=\sup\{x: \mu(x)\ge\alpha\}}{upper(\alpha)=sup(x: \mu(x)\ge\alpha)}.
#' \cr
#' Please note that many algorithms that deal with fuzzy numbers often use
#' \eqn{\alpha}-cuts rather than side functions.
#'
#' @exportClass FuzzyNumber
#' @name FuzzyNumber-class
#' @docType class
setClass(
   Class="FuzzyNumber",
   representation(
      a1="numeric",
      a2="numeric",
      a3="numeric",
      a4="numeric",
      lower="function",
      upper="function",
      left="function",
      right="function"
   ),
   prototype=prototype(
      left=function(x) NA,
      right=function(x) NA,
      lower=function(alpha) NA,
      upper=function(alpha) NA
   ),
   validity=function(object)
   {
      if (length(object@a1) != 1 || length(object@a2) != 1 ||
          length(object@a3) != 1 || length(object@a4) != 1 ||
          any(!is.finite(c(object@a1, object@a2, object@a3, object@a4))))
         return("Each of `a1', `a2', `a3', and `a4' should be a single real number");

      if (object@a1 > object@a2 || object@a2 > object@a3 || object@a3 > object@a4)
         return("Please provide a1 <= a2 <= a3 <= a4");


      if (length(formals(object@lower)) != 1)
      {
         return("`lower' should be a function with 1 parameter");
      } else if (!is.na(object@lower(0)) && (object@lower(0) < 0  || object@lower(1) > 1 || object@lower(0) > object@lower(1)))
      {
         return("`lower' should be an increasing function [0,1]->[0,1]");
      }

      if (length(formals(object@upper)) != 1)
      {
         return("`upper' should be a function with 1 parameter");
      } else if (!is.na(object@upper(0)) && (object@upper(1) < 0  || object@upper(0) > 1 || object@upper(1) > object@upper(0)))
      {
         return("`upper' should be a decreasing function [0,1]->[1,0]");
      }

      if (length(formals(object@left)) != 1)
      {
         return("`left' should be a function with 1 parameter");
      } else if (!is.na(object@left(0)) && (object@left(0) < 0  || object@left(1) > 1 || object@left(0) > object@left(1)))
      {
         return("`left' should be an increasing function [0,1]->[0,1]");
      }

      if (length(formals(object@right)) != 1)
      {
         return("`right' should be a function with 1 parameter");
      } else if (!is.na(object@right(0)) && (object@right(1) < 0  || object@right(0) > 1 || object@right(1) > object@right(0)))
      {
         return("`right' should be a decreasing function [0,1]->[1,0]");
      }


      if (is.na(object@right(0)) != is.na(object@left(0)))
         return("Either all or none of `left' and `right' should return NA");

      if (is.na(object@upper(0)) != is.na(object@lower(0)))
         return("Either all or none of `lower' and `upper' should return NA");

      # Everything is O.K.
      return(TRUE);
   }
);


#' Creates a Fuzzy Number
#'
#' For convenience, objects of class \code{FuzzyNumber}
#' may be created with this function.
#'
#' @param a1 a number specyfing left bound of the support
#' @param a2 a number specyfing left bound of the core
#' @param a3 a number specyfing right bound of the core
#' @param a4 a number specyfing right bound of the support
#' @param lower lower alpha-cut bound generator; a nondecreasing function [0,1]->[0,1] or returning NA
#' @param upper upper alpha-cut bound generator; a nonincreasing function [0,1]->[1,0] or returning NA
#' @param left lower side function generator; a nondecreasing function [0,1]->[0,1] or returning NA
#' @param right upper side function generator; a nonincreasing function [0,1]->[1,0] or returning NA
#' @export
FuzzyNumber <- function(a1, a2, a3, a4,
   lower=function(x) NA, upper=function(x) NA,
   left=function(x)  NA, right=function(x) NA)
{
   .Object <- new("FuzzyNumber", a1=a1, a2=a2, a3=a3, a4=a4,
       lower=lower, upper=upper, left=left, right=right);
   .Object;
}

#' Coverts a trapezoidal of a piecewise linear fuzzy number object to a fuzzy number
#'
#' @param object a trapezoidal or piecewiselinear fuzzy number
#' @export
as.FuzzyNumber <- function(object)
{
   if (!inherits(object, "FuzzyNumber"))
      stop("`object' does not inherit from the FuzzyNumber class");

   .Object <- new("FuzzyNumber",
         a1=object@a1, a2=object@a2, a3=object@a3, a4=object@a4,
         left=object@left, right=object@right,
         lower=object@lower, upper=object@upper);
   .Object;
}


#' Print basic information
#' @export
#' @docType methods
#' @rdname show-methods
#' @aliases show,FuzzyNumber,FuzzyNumber-method
setMethod(
   f="show",
   signature(object="FuzzyNumber"),
   definition=function(object)
   {
      cat(sprintf("Fuzzy number with:\n   support=[%g,%g],\n      core=[%g,%g].\n",
                  object@a1, object@a4, object@a2, object@a3))
   }
);


#' TO DO
#'
#' @exportMethod [
setMethod(
   f="[",
   signature=(x="FuzzyNumber"),
   definition=function(x, i, j, drop)
   {
      if (i == "a1") return(x@a1);
      if (i == "a2") return(x@a2);
      if (i == "a3") return(x@a3);
      if (i == "a4") return(x@a4);
      if (i == "left")  return(x@left);
      if (i == "right") return(x@right);
      if (i == "lower") return(x@lower);
      if (i == "upper") return(x@upper);
   }
);
