#' S4 class to represent a trapezoidal fuzzy number.
#' 
#' @slot a1
#' @slot a2
#' @slot a3
#' @slot a4
#' @export
setClass("TrapezoidalFuzzyNumber",
      contains="FuzzyNumber"
   );

TrapezoidalFuzzyNumber <- function(a1, a2, a3, a4)
   new("TrapezoidalFuzzyNumber", a1=a1, a2=a2, a3=a3, a4=a4);

setMethod(f="initialize", signature(.Object="TrapezoidalFuzzyNumber"),
          definition=function(.Object,
             a1=numeric(0), a2=numeric(0), a3=numeric(0), a4=numeric(0))
          {
             .Object@left  <- function(x) x;
             .Object@right <- function(x) 1-x;
             .Object@lower <- function(x) x;
             .Object@upper <- function(x) 1-x;
             .Object@a1 <- a1;
             .Object@a2 <- a2;
             .Object@a3 <- a3;
             .Object@a4 <- a4;
             
             callNextMethod();
             .Object;
          });


setMethod(f="show", signature(object="TrapezoidalFuzzyNumber"),
          definition=function(object) {
             cat(sprintf("Trapezoidal fuzzy number with support=[%g,%g] and core=[%g,%g].\n",
                   object@a1, object@a4, object@a2, object@a3))
          });

