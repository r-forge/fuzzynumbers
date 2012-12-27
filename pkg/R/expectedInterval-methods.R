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


setGeneric("expectedInterval",
           function(object, ...) standardGeneric("expectedInterval"))



#' TO DO
#'
#' @exportMethod expectedInterval
setMethod(
   f="expectedInterval",
   signature(object="FuzzyNumber"),
   definition=function(object, ...)
   {
      if (is.na(object@lower(0))) return(c(NA, NA))

      return(c(
         integrateAlpha(object, "lower", 0, 1, ...),
         integrateAlpha(object, "upper", 0, 1, ...)
      ))
   }
)




#' TO DO
#'
#' @exportMethod expectedInterval
setMethod(
   f="expectedInterval",
   signature(object="TrapezoidalFuzzyNumber"),
   definition=function(object, ...)
   {
      return(0.5*c((object@a2+object@a1), (object@a4+object@a3)))
   }
)





#' TO DO
#'
#' @exportMethod expectedInterval
setMethod(
   f="expectedInterval",
   signature(object="PiecewiseLinearFuzzyNumber"),
   definition=function(object, ...)
   {
      xl <- c(object@a1, object@knot.left,  object@a2)
      xr <- c(object@a3, object@knot.right, object@a4)
      dal <- diff(c(0,     object@knot.alpha,  1))
      dar <- diff(c(1, rev(object@knot.alpha), 0))
      
      return(c(
         sum( (xl[-object@knot.n-2]+0.5*diff(xl))*dal ),
         sum(-(xr[-object@knot.n-2]+0.5*diff(xr))*dar )
      ))
   }
)
