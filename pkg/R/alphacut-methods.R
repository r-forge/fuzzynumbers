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



setGeneric("alphacut",
           function(object, alpha) standardGeneric("alphacut"))

#' Calculate given alpha-cuts
#'
#' @exportMethod alphacut
#' @rdname alphacut-methods
#' @aliases alphacut,FuzzyNumber,FuzzyNumber-methods
#' @family FuzzyNumber-methods
#' @docType methods
setMethod(
   f="alphacut",
   signature(object="FuzzyNumber", alpha="numeric"),
   definition=function(object, alpha)
   {
      x <- matrix(NA, nrow=length(alpha), ncol=2);
      x[alpha >= 0 & alpha <= 1, ] <-
        c(
            object@a1+(object@a2-object@a1)*object@lower(alpha[alpha >= 0 & alpha <= 1]),
            object@a3+(object@a4-object@a3)*object@upper(alpha[alpha >= 0 & alpha <= 1])
         )

      if (length(alpha) <= 1)
         return(as.numeric(x))
      else
         return(x)
   }
)
