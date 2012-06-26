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


#' TO DO
#'
#' @exportMethod expectedInterval
setMethod(
   f="expectedInterval",
   signature(object="PowerFuzzyNumber"),
   definition=function(object, ...)
   {
      return(c(
         (object@a1+object@p.left*(object@a2-object@a1)/(object@p.left+1) ),
         (object@a3+(object@a4-object@a3)/(object@p.right+1) )
      ));
   }
);


#' TO DO
#'
#' @exportMethod value
setMethod(
   f="alphaInterval",
   signature(object="PowerFuzzyNumber"),
   definition=function(object, ...)
   {
      return(c(
         (2*object@a2*object@p.left+object@a1)/(4*object@p.left+2),
         (2*object@a3*object@p.right+object@a4)/(4*object@p.right+2)
      ));
   }
);
