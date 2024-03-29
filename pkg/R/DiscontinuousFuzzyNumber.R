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


#' S4 class representing a fuzzy number with discontinuous side functions or alpha-cut bounds
#'
#'
#' @section Slots:
#'  \describe{
#'    \item{\code{discontinuities.left}:}{Object of class \code{"numeric"} ~~ }
#'    \item{\code{discontinuities.right}:}{Object of class \code{"numeric"} ~~ }
#'    \item{\code{discontinuities.lower}:}{Object of class \code{"numeric"} ~~ }
#'    \item{\code{discontinuities.upper}:}{Object of class \code{"numeric"} ~~ }
#'  }
#'
#' @section Extends:
#' Class \code{\linkS4class{FuzzyNumber}}, directly. 
#'
#' @exportClass DiscontinuousFuzzyNumber
#' @name DiscontinuousFuzzyNumber-class
#' @seealso \code{\link{DiscontinuousFuzzyNumber}} for a convenient constructor
#' @docType class
#' @examples
#' showClass("DiscontinuousFuzzyNumber")
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
#' For convenience, objects of class \code{\linkS4class{DiscontinuousFuzzyNumber}}
#' may be created with this function.
#'
#' @param a1 a number specyfing left bound of the support
#' @param a2 a number specyfing left bound of the core
#' @param a3 a number specyfing right bound of the core
#' @param a4 a number specyfing right bound of the support
#' @param lower lower alpha-cut bound generator; a nondecreasing function [0,1]->[0,1] or returning NA_real_
#' @param upper upper alpha-cut bound generator; a nonincreasing function [0,1]->[1,0] or returning NA_real_
#' @param left lower side function generator; a nondecreasing function [0,1]->[0,1] or returning NA_real_
#' @param right upper side function generator; a nonincreasing function [0,1]->[1,0] or returning NA_real_
#' @param discontinuities.left  nondecreasingly sorted numeric vector with elements in (0,1), possibly of length 0
#' @param discontinuities.right nondecreasingly sorted numeric vector with elements in (0,1), possibly of length 0
#' @param discontinuities.lower nondecreasingly sorted numeric vector with elements in (0,1), possibly of length 0
#' @param discontinuities.upper nondecreasingly sorted numeric vector with elements in (0,1), possibly of length 0
#' @return Object of class \code{\linkS4class{DiscontinuousFuzzyNumber}}
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
       discontinuities.upper=discontinuities.upper)
   .Object
}

