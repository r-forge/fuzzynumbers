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


#' @aliases evaluate,TrapezoidalFuzzyNumber,TrapezoidalFuzzyNumber-method
#' @rdname evaluate-methods
setMethod(
   f="evaluate",
   signature(object="TrapezoidalFuzzyNumber", x="numeric"),
   definition=function(object, x)
   {
      y <- rep(0.0, length(x));
      y[x >= object@a1 & x <  object@a2] <- ((x[x >= object@a1 & x <  object@a2]-object@a1)/(object@a2-object@a1));
      y[x >  object@a3 & x <= object@a4] <- ((object@a4-x[x >  object@a3 & x <= object@a4])/(object@a4-object@a3));
      y[x >= object@a2 & x <= object@a3] <- 1.0;
      y;
   }
);


#' TO DO
#'
#' @exportMethod alphacut
setMethod(
   f="alphacut",
   signature(object="TrapezoidalFuzzyNumber", alpha="numeric"),
   definition=function(object, alpha)
   {
      x <- matrix(NA, nrow=length(alpha), ncol=2);
      x[alpha >= 0 & alpha <= 1, ] <-
        c(
            object@a1+(object@a2-object@a1)*(  alpha[alpha >= 0 & alpha <= 1]),
            object@a3+(object@a4-object@a3)*(1-alpha[alpha >= 0 & alpha <= 1])
         );
      
      if (length(alpha) <= 1)
      {
         return(as.numeric(x));
      } else
      {
         return(x);  
      }
   }
);




#' TO DO
#'
#' @exportMethod expectedInterval
setMethod(
   f="expectedInterval",
   signature(object="TrapezoidalFuzzyNumber"),
   definition=function(object)
   {
      return(0.5*c((object@a2+object@a1), (object@a4+object@a3)));
   }
);



#' TO DO
#'
#' @exportMethod expectedValue
setMethod(
   f="expectedValue",
   signature(object="TrapezoidalFuzzyNumber"),
   definition=function(object)
   {
      return(mean(expectedInterval(object)));
   }
);


#' TO DO
#'
#' @exportMethod weightedExpectedValue
setMethod(
   f="weightedExpectedValue",
   signature(object="TrapezoidalFuzzyNumber", w="numeric"),
   definition=function(object, w)
   {
      EI <- expectedInterval(object);
      return((1-w)*EI[1] + w*EI[2]);
   }
);


#' TO DO
#'
#' @exportMethod width
setMethod(
   f="width",
   signature(object="TrapezoidalFuzzyNumber"),
   definition=function(object)
   {
      return(diff(expectedInterval(A)));
   }
);


#' TO DO
#'
#' @exportMethod value   
setMethod(
   f="alphaInterval",
   signature(object="TrapezoidalFuzzyNumber"),
   definition=function(object)
   {
      return(c(
         object@a1*0.5+(object@a2-object@a1)/3,
         object@a3*0.5+(object@a4-object@a3)/6
      ));
   }
);


#' TO DO
#'
#' @exportMethod value
setMethod(
   f="value",
   signature(object="TrapezoidalFuzzyNumber"),
   definition=function(object)
   {
      return(sum(alphaInterval(object)));
   }
);


#' TO DO
#'
#' @exportMethod ambiguity
setMethod(
   f="ambiguity",
   signature(object="TrapezoidalFuzzyNumber"),
   definition=function(object)
   {
      return(diff(alphaInterval(object)));
   }
);
