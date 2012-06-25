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



#' Evaluate the membership function
#'
#' This function returns the value(s) of the membership function
#' of a fuzzy number at given point(s).
#'
#' @param object an instance of \code{FuzzyNumber} class
#' @param x a numeric vector
#' @return Value of the membership function at \code{x}
#' @export
#' @rdname evaluate-methods
#' @docType methods
#' @examples
#' T <- TrapezoidalFuzzyNumber(1,2,3,4);
#' print(evaluate(T, seq(0,5,by=0.5)));
setGeneric("evaluate",
           function(object, x) standardGeneric("evaluate"));

setGeneric("alphacut",
           function(object, alpha) standardGeneric("alphacut"));

setGeneric("supp",
           function(object) standardGeneric("supp"));

setGeneric("core",
           function(object) standardGeneric("core"));

setGeneric("expectedInterval",
           function(object, ...) standardGeneric("expectedInterval"));

setGeneric("expectedValue",
           function(object, ...) standardGeneric("expectedValue"));

setGeneric("weightedExpectedValue",
           function(object, w, ...) standardGeneric("weightedExpectedValue"));

setGeneric("width",
           function(object, ...) standardGeneric("width"));

setGeneric("alphaInterval",
           function(object, ...) standardGeneric("alphaInterval"));

setGeneric("value",
           function(object, ...) standardGeneric("value"));

setGeneric("ambiguity",
           function(object, ...) standardGeneric("ambiguity"));


#' @aliases evaluate,FuzzyNumber,FuzzyNumber-method
#' @rdname evaluate-methods
setMethod(
   f="evaluate",
   signature(object="FuzzyNumber", x="numeric"),
   definition=function(object, x)
   {
#       print("DEBUG: Evaluate call for PiecewiseLinearFuzzyNumber");

      y <- rep(0.0, length(x));
      y[x >= object@a1 & x <  object@a2] <- object@left ((x[x >= object@a1 & x <  object@a2]-object@a1)/(object@a2-object@a1));
      y[x >  object@a3 & x <= object@a4] <- object@right((x[x >  object@a3 & x <= object@a4]-object@a3)/(object@a4-object@a3));
      y[x >= object@a2 & x <= object@a3] <- 1.0;
      y;
   }
);



#' TO DO
#'
#' @exportMethod alphacut
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
#' @exportMethod supp
setMethod(
   f="supp",
   signature(object="FuzzyNumber"),
   definition=function(object)
   {
      c(object@a1, object@a4);
   }
);




#' TO DO
#'
#' @exportMethod core
setMethod(
   f="core",
   signature(object="FuzzyNumber"),
   definition=function(object)
   {
      c(object@a2, object@a3);
   }
);



#' TO DO
#'
#' @exportMethod expectedInterval
setMethod(
   f="expectedInterval",
   signature(object="FuzzyNumber"),
   definition=function(object, ...)
   {
      if (is.na(object@lower(0))) return(c(NA, NA));

      return(c(
         integrateAlpha(object, "lower", 0, 1, ...),
         integrateAlpha(object, "upper", 0, 1, ...)
      ));
   }
);



#' TO DO
#'
#' @exportMethod expectedValue
setMethod(
   f="expectedValue",
   signature(object="FuzzyNumber"),
   definition=function(object, ...)
   {
      return(mean(expectedInterval(object, ...)));
   }
);



#' TO DO
#'
#' @exportMethod weightedExpectedValue
setMethod(
   f="weightedExpectedValue",
   signature(object="FuzzyNumber", w="numeric"),
   definition=function(object, w, ...)
   {
      EI <- expectedInterval(object, ...);
      return((1-w)*EI[1] + w*EI[2]);
   }
);


#' TO DO
#'
#' @exportMethod width
setMethod(
   f="width",
   signature(object="FuzzyNumber"),
   definition=function(object, ...)
   {
      return(diff(expectedInterval(object, ...)));
   }
);


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
      ));
   }
);



#' TO DO
#'
#' @exportMethod value
setMethod(
   f="value",
   signature(object="FuzzyNumber"),
   definition=function(object, ...)
   {
      return(sum(alphaInterval(object, ...)));
   }
);






#' TO DO
#'
#' @exportMethod ambiguity
setMethod(
   f="ambiguity",
   signature(object="FuzzyNumber"),
   definition=function(object, ...)
   {
      return(diff(alphaInterval(object, ...)));
   }
);
