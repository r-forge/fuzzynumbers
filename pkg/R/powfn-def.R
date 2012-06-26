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



#' S4 class representing a fuzzy number with sides given by power functions
#'
#' TO DO
#' @exportClass TrapezoidalFuzzyNumber
#' @name TrapezoidalFuzzyNumber-class
#' @docType class
setClass(
   Class="PowerFuzzyNumber",
   representation(
      p.left="numeric",
      p.right="numeric"
   ),
   prototype=prototype(
      p.left=1.0,
      p.right=1.0
   ),
   validity=function(object)
   {
      if (object@p.left <= 0) return("`p.left' should be > 0");
      if (object@p.right <= 0) return("`p.right' should be > 0");

      return(TRUE);
   },
   contains="FuzzyNumber"
);



setMethod(
   f="initialize",
   signature("PowerFuzzyNumber"),
   definition=function(.Object, ...)
   {
      .Object <- callNextMethod();

      .Object@left   <- function(x)             x^(p.left);
      .Object@right  <- function(x)         (1-x)^(p.right);
      .Object@lower  <- function(alpha)     alpha^(1.0/p.left);
      .Object@upper  <- function(alpha)   1-alpha^(1.0/p.right);

      e <- new.env();
      environment(.Object@left)  <- e;
      environment(.Object@right) <- e;
      environment(.Object@lower) <- e;
      environment(.Object@upper) <- e;

      # Unfortunately, we have to copy these objects.....
      assign("p.left",  .Object@p.left,  envir=e);
      assign("p.right", .Object@p.right, envir=e);

      return(.Object);
   }
);


#' Creates a fuzzy number with sides given by power functions
#'
#' For convenience, objects of class \code{PowerFuzzyNumber}
#' may be created with this function.
#'
#' @param a1 a number specyfing left bound of the support
#' @param a2 a number specyfing left bound of the core
#' @param a3 a number specyfing right bound of the core
#' @param a4 a number specyfing right bound of the support
#' @param p.left a positive number specyfing the exponent for the left side
#' @param p.right a positive number specyfing the exponent for the right side
#' @export
PowerFuzzyNumber <- function(a1, a2, a3, a4, p.left=1.0, p.right=1.0)
{
   .Object <- new("PowerFuzzyNumber", a1=a1, a2=a2, a3=a3, a4=a4,
                                      p.left=p.left, p.right=p.right);
   .Object;
}



#' @rdname show-methods
#' @aliases show,TrapezoidalFuzzyNumber,TrapezoidalFuzzyNumber-method
setMethod(
   f="show",
   signature(object="PowerFuzzyNumber"),
   definition=function(object)
   {
      cat(sprintf("Fuzzy number given by power functions, and:\n   support=[%g,%g],\n      core=[%g,%g].\n",
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
      if (i == "p.left") return(x@p.left);
      if (i == "p.right") return(x@p.right);
   }
);

