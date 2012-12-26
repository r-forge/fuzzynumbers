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


#' S4 class representing a fuzzy number with possibly discontinuous side functions or alpha-cut bounds
#'
#' TO DO
#'
#' \section{Extends}{
#' Class \code{FuzzyNumber}, see \code{\link{FuzzyNumber-class}}.
#' }
#' @exportClass DiscontinuousFuzzyNumber
#' @name DiscontinuousFuzzyNumber-class
#' @seealso \code{\link{DiscontinuousFuzzyNumber}}
#' @docType class
setClass(
   Class="DiscontinuousFuzzyNumber",
   representation(
      discontinuities.left ="numeric",
      discontinuities.right="numeric",
      discontinuities.lower="numeric",
      discontinuities.upper="numeric"
   ),
   prototype=prototype(
      discontinuities.left =numeric(0),
      discontinuities.right=numeric(0),
      discontinuities.lower=numeric(0),
      discontinuities.upper=numeric(0)
   ),
   validity=function(object)
   {
      if (length(object@discontinuities.left) > 1 &&
            (is.unsorted(object@discontinuities.left) ||
               any(object@discontinuities.left < 0 | object@discontinuities.left > 1)))
         return("`discontinuities.left' should be an nondecreasingly sorted numeric vector with elements in [0,1]")

      if (length(object@discontinuities.right) > 1 &&
            (is.unsorted(object@discontinuities.right) ||
               any(object@discontinuities.right < 0 | object@discontinuities.right >= 1)))
         return("`discontinuities.right' should be an nondecreasingly sorted numeric vector with elements in [0,1]")

      if (length(object@discontinuities.lower) > 1 &&
            (is.unsorted(object@discontinuities.lower) ||
               any(object@discontinuities.lower < 0 | object@discontinuities.lower > 1)))
         return("`discontinuities.lower' should be an nondecreasingly sorted numeric vector with elements in [0,1]")

      if (length(object@discontinuities.upper) > 1 &&
            (is.unsorted(object@discontinuities.upper) ||
               any(object@discontinuities.upper < 0 | object@discontinuities.upper > 1)))
         return("`discontinuities.upper' should be an nondecreasingly sorted numeric vector with elements in [0,1]")

      # OK
      return(TRUE)
   },
   contains="FuzzyNumber"
)


#' Creates a fuzzy number with possibly discontinuous side functions or alpha-cut bounds
#'
#' For convenience, objects of class \code{DiscontinuousFuzzyNumber} (see \code{\link{DiscontinuousFuzzyNumber-class}})
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
#' @param discontinuities.left  nondecreasingly sorted numeric vector with elements in (0,1), possibly of length 0
#' @param discontinuities.right nondecreasingly sorted numeric vector with elements in (0,1), possibly of length 0
#' @param discontinuities.lower nondecreasingly sorted numeric vector with elements in (0,1), possibly of length 0
#' @param discontinuities.upper nondecreasingly sorted numeric vector with elements in (0,1), possibly of length 0
#' @export
DiscontinuousFuzzyNumber <- function(a1, a2, a3, a4,
   lower=function(a) rep(NA_real_, length(a)),
   upper=function(a) rep(NA_real_, length(a)),
   left=function(x)  rep(NA_real_, length(x)),
   right=function(x) rep(NA_real_, length(x)),
   discontinuities.left =numeric(0),
   discontinuities.right=numeric(0),
   discontinuities.lower=numeric(0),
   discontinuities.upper=numeric(0))
{
   .Object <- new("DiscontinuousFuzzyNumber", a1=a1, a2=a2, a3=a3, a4=a4,
       lower=lower, upper=upper, left=left, right=right,
       discontinuities.left =discontinuities.left,
       discontinuities.right=discontinuities.right,
       discontinuities.lower=discontinuities.lower,
       discontinuities.upper=discontinuities.upper);
   .Object;
}



#' TO DO
#'
#' @exportMethod [
setMethod(
   f="[",
   signature=(x="DiscontinuousFuzzyNumber"),
   definition=function(x, i, j, drop)
   {
      if (i == "a1") return(x@a1)
      if (i == "a2") return(x@a2)
      if (i == "a3") return(x@a3)
      if (i == "a4") return(x@a4)
      if (i == "left")  return(x@left)
      if (i == "right") return(x@right)
      if (i == "lower") return(x@lower)
      if (i == "upper") return(x@upper)
      if (i == "discontinuities.left")  return(x@discontinuities.left)
      if (i == "discontinuities.right") return(x@discontinuities.right)
      if (i == "discontinuities.lower") return(x@discontinuities.lower)
      if (i == "discontinuities.upper") return(x@discontinuities.upper)
   }
)
