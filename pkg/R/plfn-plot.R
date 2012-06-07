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
      drawX     <- !(is.na(x@left(0)));
      drawAlpha <- !(is.na(x@lower(0)));

      add <- identical(add, TRUE);

      if (dev.cur() == 1L && add)
      {
         warning("`add' will be ignored as there is no existing plot")
         add <- FALSE;
      }


      if (is.null(from) || is.null(to))
      {
         xlim <-
            if (!is.null(xlim))
            {
               xlim;
            } else if (add)
            {
               usr <- par("usr")[1L:2L]
               if (par("xaxs") == "r") usr <- extendrange(usr, f = -1.0/27.0);
               usr
            } else
            {
               extendrange(c(x@a1, x@a4), f = 1.0/27.0); # core + epsilon
            }

         if (is.null(from)) from <- xlim[1L];
         if (is.null(to))   to <- xlim[2L];
      } else if (is.null(xlim))
      {
         xlim <- c(from, to);
      }

      if (from > x@a1) from <- x@a1;
      if (to   < x@a4) to   <- x@a4;

      matplot(c(from, x@a1, x@knot.left,  x@a2, x@a3, x@knot.right,      x@a4, to),
              c(0,    0,    x@knot.alpha, 1,    1,    rev(x@knot.alpha), 0,    0),
         type=type, xlab=xlab, ylab=ylab, xlim=xlim, ylim=ylim, col=col,
         lty=lty, pch=pch, lwd=lwd, add=add, ...);
   }
);
   