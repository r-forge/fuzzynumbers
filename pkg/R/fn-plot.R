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
#' @examples
#' plot(FuzzyNumber(0,1,2,3), col="gray")
#' plot(FuzzyNumber(0,1,2,3, left=function(x) x^2, right=function(x) 1-x^3), add=TRUE)
#' plot(FuzzyNumber(0,1,2,3, lower=function(x) x, upper=function(x) 1-x), add=TRUE, col=2)
#' @export
#' @rdname plot-methods
#' @docType methods
setMethod(
   f="plot",
   signature(x="FuzzyNumber", y="missing"),
   definition=function(x, y, from=NULL, to=NULL, n=101,
      at.alpha=NULL, type="l", xlab="x", ylab=expression(alpha),
      xlim=NULL, ylim=c(0,1), col=1, lty=1, pch=1, lwd=1,
      shadowdensity=15, shadowangle=45, shadowcol=col, shadowborder=NULL,
      add=FALSE, ...)
   {
      drawX     <- !(is.na(x@left(0)));
      drawAlpha <- !(is.na(x@lower(0)));
      
      add <- identical(add, TRUE);
      
      if (dev.cur() == 1L && add)
      {
         warning("`add' will be ignored as there is no existing plot")
         add <- FALSE;
      }

      if (n <= 0) n <- 0;
      
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

      if (!drawX && !drawAlpha)
      {
         matplot(c(from, x@a1), c(0,0), type=type, xlab=xlab, ylab=ylab, xlim=xlim, ylim=ylim, col=col, lty=lty, pch=pch, lwd=lwd, add=add, ...);
         rect(x@a1, 0, x@a2, 1, density=shadowdensity, col=shadowcol, angle=shadowangle, border=shadowborder);
         rect(x@a3, 1, x@a4, 0, density=shadowdensity, col=shadowcol, angle=shadowangle, border=shadowborder);
         matplot(c(x@a4, to),   c(0,0), type=type, col=col, lty=lty, pch=pch, lwd=lwd, add=TRUE, ...);
         matplot(c(x@a2, x@a3), c(1,1), type=type, col=col, lty=lty, pch=pch, lwd=lwd, add=TRUE, ...);
      } else
      {   
         if (drawAlpha && (!drawX || !is.null(at.alpha)))
         {
            if (!is.numeric(at.alpha) || is.unsorted(at.alpha) ||
                  any(at.alpha <= 0 | at.alpha >= 1) || length(at.alpha) == 0)
            {
               at.alpha <- seq(1/(n+1),1-1/(n+1),length.out=n);
            }

            if (length(at.alpha) == 0)
            {
               xvals1 <- numeric(0);
               xvals2 <- numeric(0);
               alpha1 <- numeric(0);
               alpha2 <- numeric(0);
            } else if (length(at.alpha) == 1)
            {
               xvals <- alphacut(x, at.alpha);
               xvals1 <- xvals[1];
               xvals2 <- rev(xvals[2]);
               alpha1 <- at.alpha;
               alpha2 <- at.alpha;
            } else
            {
               xvals <- alphacut(x, at.alpha);

               xvals1 <- xvals[,1];
               xvals2 <- rev(xvals[,2]);
               alpha1 <- at.alpha;
               alpha2 <- rev(at.alpha);
            }
         } else
         {
            xvals1 <- seq(x@a1, x@a2, length.out=n+2); xvals1 <- xvals1[-c(1,n+2)];
            xvals2 <- seq(x@a3, x@a4, length.out=n+2); xvals2 <- xvals2[-c(1,n+2)];
            alpha1 <- evaluate(x, xvals1);
            alpha2 <- evaluate(x, xvals2);
         }

         matplot(c(from, x@a1, xvals1, x@a2, x@a3, xvals2, x@a4, to),
                 c(0,    0,    alpha1, 1,    1,    alpha2, 0,    0),
                 type=type, xlab=xlab, ylab=ylab, xlim=xlim, ylim=ylim, col=col,
                 lty=lty, pch=pch, lwd=lwd, add=add, ...);

      }
   }
);
   