setGeneric("evaluate", function(object, x) standardGeneric("evaluate"));

setMethod(
   f="evaluate",
   signature(object="FuzzyNumber", x="numeric"),
   definition=function(object, x)
   {
      y <- rep(0.0, length(x));
      y[x >= object@a1 & x <  object@a2] <- object@left ((x[x >= object@a1 & x <  object@a2]-object@a1)/(object@a2-object@a1));
      y[x >  object@a3 & x <= object@a4] <- object@right((x[x >  object@a3 & x <= object@a4]-object@a3)/(object@a4-object@a3));
      y[x >= object@a2 & x <= object@a3] <- 1.0;
      y;
   });     


setGeneric("alphacut", function(object, alpha) standardGeneric("alphacut"));

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
   });



setGeneric("supp", function(object) standardGeneric("supp"));

setMethod(
   f="supp",
   signature(object="FuzzyNumber"),
   definition=function(object)
   {
      c(object@a1, object@a4);
   });



setGeneric("core", function(object) standardGeneric("core"));

setMethod(
   f="core",
   signature(object="FuzzyNumber"),
   definition=function(object)
   {
      c(object@a2, object@a3);
   });


setGeneric("expectedInterval", function(object, ...) standardGeneric("expectedInterval"));

setMethod(
   f="expectedInterval",
   signature(object="FuzzyNumber"),
   definition=function(object, subdivisions=100, rel.tol = .Machine$double.eps^0.25, abs.tol = rel.tol)
   {
      if (is.na(object@lower(0))) return(c(NA, NA));
      
      el <- object@a1+(object@a2-object@a1)*integrate(object@lower, 0, 1, subdivisions=subdivisions, rel.tol=rel.tol, abs.tol=abs.tol)$value;
      eu <- object@a3+(object@a4-object@a3)*integrate(object@upper, 0, 1, subdivisions=subdivisions, rel.tol=rel.tol, abs.tol=abs.tol)$value;
      
      return(c(el, eu));         
   });


setGeneric("expectedValue", function(object, ...) standardGeneric("expectedValue"));

setMethod(
   f="expectedValue",
   signature(object="FuzzyNumber"),
   definition=function(object, subdivisions=100, rel.tol = .Machine$double.eps^0.25, abs.tol = rel.tol)
   {
      return(mean(expectedInterval(object, subdivisions=subdivisions, rel.tol=rel.tol, abs.tol=abs.tol)));
   });


setGeneric("weightedExpectedValue", function(object, w, ...) standardGeneric("weightedExpectedValue"));

setMethod(
   f="weightedExpectedValue",
   signature(object="FuzzyNumber", w="numeric"),
   definition=function(object, w, subdivisions=100, rel.tol = .Machine$double.eps^0.25, abs.tol = rel.tol)
   {
      EI <- expectedInterval(object, subdivisions=subdivisions, rel.tol=rel.tol, abs.tol=abs.tol);
      return((1-w)*EI[1] + w*EI[2]);
   });


setGeneric("value", function(object, ...) standardGeneric("value"));

setMethod(
   f="value",
   signature(object="FuzzyNumber"),
   definition=function(object, subdivisions=100, rel.tol = .Machine$double.eps^0.25, abs.tol = rel.tol)
   {
      if (is.na(object@lower(0))) return(c(NA, NA));
      
      v <- integrate(function(x) {
         x*(object@a1+(object@a2-object@a1)*object@lower(x)+object@a3+(object@a4-object@a3)*object@upper(x))
      }, 0, 1, subdivisions=subdivisions, rel.tol=rel.tol, abs.tol=abs.tol)$value;
      
      return(v);
   });




setGeneric("width", function(object, ...) standardGeneric("width"));

setMethod(
   f="width",
   signature(object="FuzzyNumber"),
   definition=function(object, subdivisions=100, rel.tol = .Machine$double.eps^0.25, abs.tol = rel.tol)
   {
      return(diff(expectedInterval(object, subdivisions=subdivisions, rel.tol=rel.tol, abs.tol=abs.tol)));
   });



setGeneric("ambiguity", function(object, ...) standardGeneric("ambiguity"));

setMethod(
   f="ambiguity",
   signature(object="FuzzyNumber"),
   definition=function(object, subdivisions=100, rel.tol = .Machine$double.eps^0.25, abs.tol = rel.tol)
   {
      if (is.na(object@lower(0))) return(c(NA, NA));
      
      v <- integrate(function(x) {
         x*(object@a3+(object@a4-object@a3)*object@upper(x)-object@a1+(object@a2-object@a1)*object@lower(x))
      }, 0, 1, subdivisions=subdivisions, rel.tol=rel.tol, abs.tol=abs.tol)$value;
      
      return(v);
   });


setGeneric("distance", function(object1, object2, ...) standardGeneric("distance"));

setMethod(
   f="distance",
   signature(object1="FuzzyNumber", object2="FuzzyNumber"),
   definition=function(object1, object2, type=c("Euclidean", "EuclideanSquared"), subdivisions=100, rel.tol = .Machine$double.eps^0.25, abs.tol = rel.tol)
   {
      if (is.na(object1@lower(0)) || is.na(object2@lower(0))) return(NA);
      type = match.arg(type);
      
      if (type == "Euclidean" || type == "EuclideanSquared")
      {
         dL <- integrate(function(alpha) {
            (object1@a1+(object1@a2-object1@a1)*object1@lower(alpha) - object2@a1+(object2@a2-object2@a1)*object2@lower(alpha))^2
         }, 0, 1, subdivisions=subdivisions, rel.tol=rel.tol, abs.tol=abs.tol)$value
         
         dU <- integrate(function(alpha) {
            (object1@a3+(object1@a4-object1@a3)*object1@upper(alpha) - object2@a3+(object2@a4-object2@a3)*object2@upper(alpha))^2
         }, 0, 1, subdivisions=subdivisions, rel.tol=rel.tol, abs.tol=abs.tol)$value
         
         if (type == "Euclidean") return (sqrt(dL+dU)) else return (dL+dU);
      }
   });

