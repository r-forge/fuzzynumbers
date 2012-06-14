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
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
## GNU Lesser General Public License for more details.
##
## You should have received a copy of the GNU Lesser General Public License
## along with FuzzyNumbers. If not, see <http://www.gnu.org/licenses/>.


#' @export
integrate_discont_val <- function(f, lower, upper, ..., discontinuities=numeric(0),
          subdivisions=100, rel.tol = .Machine$double.eps^0.5, # greater accuracy
          abs.tol = rel.tol, stop.on.error = TRUE, keep.xy = FALSE, aux = NULL)

{
   if (!is.numeric(discontinuities))
      stop("`discontinuities' should be numeric");

   if (lower >= upper)
      stop("`lower' < `upper' NOT TRUE")

   discontinuities <- discontinuities[discontinuities > lower & discontinuities < upper];
   m <- length(discontinuities);
      
   if (m == 0)
      return(integrate(f=f, lower=lower, upper=upper, ...,
         subdivisions=subdivisions, rel.tol=rel.tol, abs.tol=abs.tol,
         stop.on.error=stop.on.error, keep.xy=keep.xy, aux=aux)$value);

   if (is.unsorted(discontinuities))
      stop("`discontinuities' should be ordered nondecreasingly");

   x <- c(lower, discontinuities, upper);
   v <- numeric(m+1);
      
   for (i in 1:(m+1))
   {
      v[i] <- integrate(f=f, lower=x[i]+rel.tol, upper=x[i+1]-rel.tol, ...,
         subdivisions=subdivisions, rel.tol=rel.tol, abs.tol=abs.tol,
         stop.on.error=stop.on.error, keep.xy=keep.xy, aux=aux)$value;
   }

   return(sum(v));
}
