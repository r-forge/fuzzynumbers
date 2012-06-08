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
   definition=function(x, y, from=NULL, to=NULL,
      draw.membership.function=TRUE, draw.alphacuts=!draw.membership.function,
      xlab=NULL, ylab=NULL, xlim=NULL, ylim=NULL,
      type="l", col=1, lty=1, pch=1, lwd=1,
      add=FALSE, ...)
   {
      callNextMethod(x, at.alpha=x@knot.alpha,
         from=from, to=to, type=type, xlab=xlab, ylab=ylab,
         xlim=xlim, ylim=ylim, col=col, lty=lty, pch=pch, lwd=lwd,
         draw.membership.function=draw.membership.function,
         draw.alphacuts=draw.alphacuts, add=add, ...);
   }
);
   