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



setGeneric("alphaInterval",
           function(object, ...) standardGeneric("alphaInterval"))

#' TO DO
#'
#' @exportMethod alphaInterval
setMethod(
   f="alphaInterval",
   signature(object="FuzzyNumber"),
   definition=function(object, ...)
   {
      if (is.na(object@lower(0))) return(c(NA, NA));

      return(c(
         integrateAlpha(object, "lower", 0, 1, weight=identity, ...),
         integrateAlpha(object, "upper", 0, 1, weight=identity, ...)
      ))
   }
)




#' TO DO
#'
#' @exportMethod value
setMethod(
   f="alphaInterval",
   signature(object="TrapezoidalFuzzyNumber"),
   definition=function(object, ...)
   {
      return(c(
         object@a1*0.5+(object@a2-object@a1)/3,
         object@a3*0.5+(object@a4-object@a3)/6
      ))
   }
)





#' TO DO
#'
#' @exportMethod value
setMethod(
   f="alphaInterval",
   signature(object="PiecewiseLinearFuzzyNumber"),
   definition=function(object, ...)
   {
      xl <- c(object@a1, object@knot.left,  object@a2)
      xr <- c(object@a3, object@knot.right, object@a4)
      al <- c(0,     object@knot.alpha,  1)
      ar <- c(1, rev(object@knot.alpha), 0)
      dxl <- diff(xl)
      dxr <- diff(xr)
      dal <- diff(al)
      dar <- diff(ar)
      
      return(c(
         sum( diff(al^2)*(xl[-object@knot.n-2]-al[-object@knot.n-2]*dxl/dal)/2+diff(al^3)*dxl/dal/3 ),
         -sum( diff(ar^2)*(xr[-object@knot.n-2]-ar[-object@knot.n-2]*dxr/dar)/2+diff(ar^3)*dxr/dar/3 )
      ))
   }
)
