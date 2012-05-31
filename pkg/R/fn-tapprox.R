setGeneric("trapezoidalApproximation", function(object, ...) standardGeneric("trapezoidalApproximation"));

# Naive
# We have core(A)==core(T(A)) and supp(A)==supp(T(A))

# ExpectedIntervalPreserving
# L2-nearest trapezoidal approximation preserving the expected interval given in
# (Grzegorzewski, 2010; Ban, 2008; Yeh, 2008)
# Unfortunately, for highly skewed membership functions this approximation operator may have
# quite unfavourable behavior. E.g. if Val(A) < EV_{1/3}(A) or Val(A) > EV_{2/3}(A),
# then it may happen that the core of the output 
# and the core of the original fuzzy number A are disjoint
# (cf. Grzegorzewski, Pasternak-Winiarska, 2011)

# SupportCoreRestricted
# This method was proposed in (Grzegorzewski, Pasternak-Winiarska, 2011).
# L2-nearest trapezoidal approximation with constraints
# core(A) \subseteq core(T(A)) and supp(T(A)) \subseteq supp(A), i.e.
# for which each point that surely belongs to A also belongs to T(A),
# and each point that surely does not belong to A also does not belong to T(A).


   
# Ban A.I. (2008), Approximation of fuzzy numbers by trapezoidal fuzzy numbers
# preserving the expected interval, Fuzzy Sets and Systems 159, pp. 1327-1344.
# Grzegorzewski P. (2010), Algorithms for trapezoidal approximations of fuzzy numbers
# preserving the expected interval, in: Bouchon-Meunier B. et al (Eds.),
# Foundations of Reasoning Under Uncertainty, Springer, pp. 85-98.
# Grzegorzewski P, Pasternak-Winiarska K. (2011), Trapezoidal approximations of fuzzy numbers
# with restrictions on the support and core, Proc. EUSFLAT/LFA 2011, Atlantic Press, pp. 749-756.

# Yeh C.-T. (2008), Trapezoidal and triangular approximations preserving the expected interval,
# Fuzzy Sets and Systems 159, pp. 1345-1353.

# > (A <- FuzzyNumber(-1,0,1,40,lower=function(x) sqrt(x),upper=function(x) 1-sqrt(x)))
# Fuzzy number with support=[-1,40] and core=[0,1].
# > (S <- trapezoidalApproximation(D, "ExpectedIntervalPreserving")) # Note that cores are disjoint!
# Trapezoidal fuzzy number with support=[-0.333333,28.3333] and core=[-0.333333,-0.333333].
# > expectedInterval(A)
# [1] -0.3333333 13.9999971
# > expectedInterval(S)
# [1] -0.3333333 13.9999971

setMethod(
   f="trapezoidalApproximation",
   signature(object="FuzzyNumber"),
   definition=function(object, method=c("ExpectedIntervalPreserving", "SupportCoreRestricted", "Naive"),
      intLower=NA, intUpper=NA, intAlphaTimesLower=NA, intAlphaTimesUpper=NA,
      subdivisions=100, rel.tol = .Machine$double.eps^0.25, abs.tol = rel.tol)
   {
      method <- match.arg(method);
      
      if (method == "Naive")
         return(TrapezoidalFuzzyNumber(object@a1, object@a2, object@a3, object@a4));
      
      if (is.na(intLower) && is.na(intAlphaTimesLower))
      {
         if (is.na(object@lower(0))) error("Integral for lower alphacut bound cannot be computed");
         
         intLower <- integrate(object@lower, 0, 1, subdivisions=subdivisions, rel.tol=rel.tol, abs.tol=abs.tol)$value;
         intAlphaTimesLower <- integrate(function(alpha) object@lower(alpha)*alpha, 0, 1, subdivisions=subdivisions, rel.tol=rel.tol, abs.tol=abs.tol)$value;
      }
      
      if (is.na(intUpper) && is.na(intAlphaTimesUpper))
      {
         if (is.na(object@lower(0))) error("Integral for upper alphacut bound cannot be computed");
         
         intUpper <- integrate(object@upper, 0, 1, subdivisions=subdivisions, rel.tol=rel.tol, abs.tol=abs.tol)$value;
         intAlphaTimesUpper <- integrate(function(alpha) object@upper(alpha)*alpha, 0, 1, subdivisions=subdivisions, rel.tol=rel.tol, abs.tol=abs.tol)$value;
      }    
      
      intLower <- object@a1+(object@a2-object@a1)*intLower;
      intUpper <- object@a3+(object@a4-object@a3)*intUpper;
      intAlphaTimesLower <- object@a1*0.5+(object@a2-object@a1)*intAlphaTimesLower;
      intAlphaTimesUpper <- object@a3*0.5+(object@a4-object@a3)*intAlphaTimesUpper;
      
      if (method == "ExpectedIntervalPreserving")
      {
         # Here we use the method given in (Grzegorzewski, 2010)
         
         if (intAlphaTimesUpper-intAlphaTimesLower >= (intUpper-intLower)/3 )
            # i.e. if ambiguity(A) >= width(A)/3
         {
            a1 <-  4*intLower-6*intAlphaTimesLower;
            a2 <- -2*intLower+6*intAlphaTimesLower;
            a3 <- -2*intUpper+6*intAlphaTimesUpper;
            a4 <-  4*intUpper-6*intAlphaTimesUpper;
         } else
         {
            Eval13 <- 2*intLower/3 +   intUpper/3; # Weighted Expected Value w=1/3
            Eval23 <-   intLower/3 + 2*intUpper/3; # Weighted Expected Value w=2/3
            Val <- intAlphaTimesLower + intAlphaTimesUpper; # Value
            
            if (Eval13 <= Val && Val <= Eval23)
            {
               a1 <-       3*intLower +   intUpper - 3*intAlphaTimesLower - 3*intAlphaTimesUpper;
               a2 <- a3 <-  -intLower -   intUpper + 3*intAlphaTimesLower + 3*intAlphaTimesUpper;
               a4 <-         intLower + 3*intUpper - 3*intAlphaTimesLower - 3*intAlphaTimesUpper;
            } else if (Val < Eval13)
            {
               a1 <- a2 <- a3 <- intLower;
               a4 <- 2*intUpper - intLower;
            } else
            {
               a1 <- 2*intLower - intUpper;
               a2 <- a3 <- a4 <- intUpper;
            }
         }
         
         return(TrapezoidalFuzzyNumber(a1, a2, a3, a4));
         
      } else if (method == "SupportCoreRestricted")
      {
         u1 <- 4*intLower - 6*intAlphaTimesLower;
         u2 <- 6*intAlphaTimesLower - 2*intLower;
         if (object@a2 > u2)
         {
            if (object@a1 < u1)
            {
               a1 <- u1;
               a2 <- u2;
            } else
            {
               a1 <- object@a1;
               a2 <- 2*intLower-object@a1;
            }
         } else
         {
            if (object@a1 < u1)
            {
               a1 <- 2*intLower-object@a2;
               a2 <- object@a2;
            } else
            {
               a1 <- object@a1;
               a2 <- object@a2;
            }
         }
         
         
         u3 <- 6*intAlphaTimesUpper - 2*intUpper;
         u4 <- 4*intUpper - 6*intAlphaTimesUpper;
         if (object@a4 > u4)
         {
            if (object@a3 < u3)
            {
               a3 <- u3;
               a4 <- u4;
            } else
            {
               a3 <- object@a3;
               a4 <- 2*intUpper-object@a3;
            }
         } else
         {
            if (object@a3 < u3)
            {
               a3 <- 2*intUpper-object@a4;
               a4 <- object@a4;
            } else
            {
               a3 <- object@a3;
               a4 <- object@a4;
            }
         }
         
         return(TrapezoidalFuzzyNumber(a1, a2, a3, a4));
      }
   });

