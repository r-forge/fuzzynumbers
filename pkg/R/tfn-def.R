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


#' S4 class representing a trapezoidal fuzzy number
#'
#' TO DO
#' @exportClass TrapezoidalFuzzyNumber
#' @name TrapezoidalFuzzyNumber-class
#' @docType class
setClass(
   Class="TrapezoidalFuzzyNumber",
   prototype=prototype(
      left=function(x) x,
      right=function(x) 1-x,
      lower=function(alpha) alpha,
      upper=function(alpha) 1-alpha
   ),
   contains="FuzzyNumber"
);


#' Creates a trapezoidal fuzzy number
#'   
#' For convenience, objects of class \code{TrapezoidalFuzzyNumber}
#' may be created with the following function.
#' @param a1 TO DO
#' @param a2 TO DO
#' @param a3 TO DO
#' @param a4 TO DO
#' @export   
TrapezoidalFuzzyNumber <- function(a1, a2, a3, a4)
{
   .Object <- new("TrapezoidalFuzzyNumber", a1=a1, a2=a2, a3=a3, a4=a4);
   .Object;
}



#' @rdname show-methods
#' @aliases show,TrapezoidalFuzzyNumber,TrapezoidalFuzzyNumber-method
setMethod(
   f="show",
   signature(object="TrapezoidalFuzzyNumber"),
   definition=function(object)
   {
      cat(sprintf("Trapezoidal fuzzy number with support=[%g,%g] and core=[%g,%g].\n",
                  object@a1, object@a4, object@a2, object@a3))
   }
);

