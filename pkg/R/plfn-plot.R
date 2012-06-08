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
#' @export
#' @rdname plot-methods
#' @docType methods
setMethod(
   f="plot",
   signature(x="PiecewiseLinearFuzzyNumber", y="missing"),
   definition=function(x, y, from=NULL, to=NULL, add=FALSE,
      type="l", xlab="x", ylab=expression(alpha), xlim=NULL, ylim=c(0,1),
      col=1, lty=1, pch=1, lwd=1, ...)
   {
      callNextMethod(x, at.alpha=x@knot.alpha,
         from=from, to=to, add=add, type=type, xlab=xlab, ylab=ylab,
         xlim=xlim, ylim=ylim, col=col, lty=lty, pch=pch, lwd=lwd, ...);
   }
);
   