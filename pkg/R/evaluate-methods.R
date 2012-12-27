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




setGeneric("evaluate",
           function(object, x) standardGeneric("evaluate"))


#' Evaluate the membership function
#'
#' This function returns the value(s) of the membership function
#' of a fuzzy number at given point(s).
#'
#' @param object an instance of \code{FuzzyNumber} class
#' @param x a numeric vector
#' @return Value of the membership function at \code{x}
#' @exportMethod evaluate
#' @rdname evaluate-methods
#' @aliases evaluate,FuzzyNumber,FuzzyNumber-methods
#' @family FuzzyNumber-methods
#' @docType methods
#' @examples
#' T <- TrapezoidalFuzzyNumber(1,2,3,4);
#' print(evaluate(T, seq(0,5,by=0.5)));
setMethod(
   f="evaluate",
   signature(object="FuzzyNumber", x="numeric"),
   definition=function(object, x)
   {
#       print("DEBUG: Evaluate call for PiecewiseLinearFuzzyNumber");

      y <- rep(0.0, length(x))
      y[x >= object@a1 & x <  object@a2] <- object@left ((x[x >= object@a1 & x <  object@a2]-object@a1)/(object@a2-object@a1))
      y[x >  object@a3 & x <= object@a4] <- object@right((x[x >  object@a3 & x <= object@a4]-object@a3)/(object@a4-object@a3))
      y[x >= object@a2 & x <= object@a3] <- 1.0
      y
   }
)

