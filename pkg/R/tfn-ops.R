setMethod(
   f="evaluate",
   signature(object="TrapezoidalFuzzyNumber", x="numeric"),
   definition=function(object, x)
   {
      y <- rep(0.0, length(x));
      y[x >= object@a1 & x <  object@a2] <- ((  x[x >= object@a1 & x <  object@a2]-object@a1)/(object@a2-object@a1));
      y[x >  object@a3 & x <= object@a4] <- ((1-x[x >  object@a3 & x <= object@a4]-object@a3)/(object@a4-object@a3));
      y[x >= object@a2 & x <= object@a3] <- 1.0;
      y;
   });     


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
   });




setMethod(
   f="expectedInterval",
   signature(object="TrapezoidalFuzzyNumber"),
   definition=function(object)
   {
      return(c((object@a1+object@a2)*0.5, (object@a3+object@a4)*0.5));         
   });


setMethod(
   f="expectedValue",
   signature(object="TrapezoidalFuzzyNumber"),
   definition=function(object)
   {
      return(mean(expectedInterval(object)));
   });


setMethod(
   f="weightedExpectedValue",
   signature(object="TrapezoidalFuzzyNumber", w="numeric"),
   definition=function(object, w)
   {
      EI <- expectedInterval(object);
      return((1-w)*EI[1] + w*EI[2]);
   });


setMethod(
   f="value",
   signature(object="TrapezoidalFuzzyNumber"),
   definition=function(object)
   {
      return(
         object@a1*0.5+(object@a2-object@a1)/3 +
         object@a3*0.5+(object@a4-object@a3)/6
      );
   });



setMethod(
   f="width",
   signature(object="TrapezoidalFuzzyNumber"),
   definition=function(object)
   {
      return(diff(expectedInterval(A)));
   });



setMethod(
   f="ambiguity",
   signature(object="TrapezoidalFuzzyNumber"),
   definition=function(object)
   {
      return(
         object@a3*0.5+(object@a4-object@a3)/6 -
         object@a1*0.5+(object@a2-object@a1)/3
      );
   });
