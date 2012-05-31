#' S4 class to represent a fuzzy number
#' 
#' @slot a1
#' @slot a2
#' @slot a3
#' @slot a4
#' @slot lower
#' @slot upper
#' @slot left
#' @slot right
#' @export
setClass("FuzzyNumber",
   representation(
      a1="numeric",a2="numeric",a3="numeric",a4="numeric",
      lower="function",upper="function",
      left="function", right="function")
   );

FuzzyNumber <- function(a1, a2, a3, a4,
   lower=function(x) NA, upper=function(x) NA,
   left=function(x)  NA, right=function(x) NA)
      new("FuzzyNumber", a1=a1, a2=a2, a3=a3, a4=a4,
          lower=lower, upper=upper, left=left, right=right);

setValidity("FuzzyNumber",
   function(object)
   {
      if (length(object@a1) != 1 || length(object@a2) != 1 ||
          length(object@a3) != 1 || length(object@a4) != 1 ||
          any(!is.finite(c(object@a1, object@a2, object@a3, object@a4))))
         return("Each of `a1', `a2', `a3', and `a4' should be single real number");
      
      if (object@a1 > object@a2 || object@a2 > object@a3 || object@a3 > object@a4)
         return("Please provide a1 <= a2 <= a3 <= a4");
         
      
      # Check given functions
      if (length(formals(object@lower)) != length(formals(object@upper)))
         return("Either all or none of `lower' and `upper' should be NULL");
      
      if (length(formals(object@left)) != length(formals(object@right)))
         return("Either all or none of `left' and `right' should be NULL");
      
#       if (length(formals(object@lower)) == 0 && length(formals(object@left)) == 0)
#          return("Please provide left and right side functions or lower and upper alpha-cut bounds");
      
      
      if (length(formals(object@lower)) > 1)
      {
         return("`lower' should be NULL or be a function with 1 parameter");
      } else if (!is.na(object@lower(0)) && (object@lower(0) < 0  || object@lower(1) > 1 || object@lower(0) > object@lower(1)))
      {
         return("`lower' should be an increasing function [0,1]->[0,1]");
      }

      if (length(formals(object@upper)) > 1)
      {
         return("`upper' should be NULL or be a function with 1 parameter");
      } else if (!is.na(object@upper(0)) && (object@upper(1) < 0  || object@upper(0) > 1 || object@upper(1) > object@upper(0)))
      {
         return("`upper' should be a decreasing function [0,1]->[1,0]");
      }

      if (length(formals(object@left)) > 1)
      {
         return("`left' should be NULL or be a function with 1 parameter");
      } else if (!is.na(object@left(0)) && (object@left(0) < 0  || object@left(1) > 1 || object@left(0) > object@left(1)))
      {
         return("`left' should be an increasing function [0,1]->[0,1]");
      }

      if (length(formals(object@right)) > 1)
      {
         return("`right' should be NULL or be a function with 1 parameter");
      } else if (!is.na(object@right(0)) && (object@right(1) < 0  || object@right(0) > 1 || object@right(1) > object@right(0)))
      {
         return("`right' should be a decreasing function [0,1]->[1,0]");
      }
      
      # Everything is O.K.
      return(TRUE);
   });


setMethod(f="initialize", signature(.Object="FuzzyNumber"),
          definition=function(.Object,
             a1=numeric(0), a2=numeric(0), a3=numeric(0), a4=numeric(0),
             lower=function(x) NA, upper=function(x) NA, left=function(x) NA, right=function(x) NA)
          {
             .Object@left  <- left;
             .Object@right <- right;
             .Object@lower <- lower;
             .Object@upper <- upper;
             .Object@a1 <- a1;
             .Object@a2 <- a2;
             .Object@a3 <- a3;
             .Object@a4 <- a4;
             
             callNextMethod();
             .Object;
          });


setMethod(f="show", signature(object="FuzzyNumber"),
          definition=function(object) {
             cat(sprintf("Fuzzy number with support=[%g,%g] and core=[%g,%g].\n",
                         object@a1, object@a4, object@a2, object@a3))
          });

