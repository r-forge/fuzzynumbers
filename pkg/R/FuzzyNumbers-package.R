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


#' \pkg{FuzzyNumbers} is an open source (LGPL 3) package for R.
#' It provides S4 classes and methods to deal with Fuzzy Numbers
#' and allows for computations of arithmetic operations on FNs,
#' approximation by trapezoidal and piecewise linear FNs,
#' random fuzzy numbers generation [TO DO] etc.
#' The package may be used by the practitioners as well as by the researchers
#' in fuzzy numbers theory (e.g. for testing new algorithms,
#' generating numerical examples, preparing figures).
#'
#' Fuzzy set theory lets us effectively and quite intuitively represent
#' imprecise or vague information. Fuzzy numbers, which form a particular
#' subclass of fuzzy sets of the real line, play a significant role
#' in many important theoretical and/or practical considerations.
#' This is because we often describe our knowledge about objects
#' through numbers, e.g. "I'm about 180 cm tall"
#' or "The rocket was launched between 2 and 3 p.m.".
#'
#' For the formal definition of a fuzzy number
#' please refer to the \code{\linkS4class{FuzzyNumber}} man page.
#' Note that this package also deals with particular types
#' of fuzzy numbers like trapezoidal, piecewise linear, or ``parametric'' FNs
#' (see \code{\linkS4class{TrapezoidalFuzzyNumber}}
#' \code{\linkS4class{PiecewiseLinearFuzzyNumber}},
#' \code{\linkS4class{PowerFuzzyNumber}},
#' \code{\linkS4class{DiscontinuousFuzzyNumber}})
#'
#' The package aims to provide the following functionality:
#' \enumerate{
#'    \item Representation of arbitrary fuzzy numbers
#' (including FNs with discontinuous side functions and/or alpha-cuts),
#' as well as their particular types, e.g. trapezoidal and piecewise linear fuzzy numbers,
#'    \item Defuzzification and Approximation by Triangular
#'      and Piecewise Linear FNs (see e.g. \code{\link{expectedValue}},
#'      \code{\link{value}}, \code{\link{trapezoidalApproximation}},
#'      \code{\link{piecewiseLinearApproximation}}),
#'    \item Visualization of FNs (see \code{\link{plot}}),
#'    \item Operations on FNs [TO DO],
#'    \item Aggregation of FNs [TO DO],
#'    \item Ranking of FNs [TO DO],
#'    \item Random FN generation [TO DO],
#'    \item \dots
#' }
#'
#' Please feel free to send any comments and feature requests to the author
#' (see his homepage at \url{http://www.ibspan.waw.pl/~gagolews}).
#'
#' For a complete list of classes and methods
#' call \code{library(help="FuzzyNumbers")}.
#' Moreover, you will surely be interested in a step-by-step guide to the
#' package usage and features which is available at
#' \url{http://www.ibspan.waw.pl/~gagolews/FuzzyNumbers/doc/FuzzyNumbers-Tutorial.pdf}.
#' \cr\cr
#'
#' \bold{Keywords}: Fuzzy Numbers, Fuzzy Sets, Shadowed Sets,
#' Trapezoidal Approximation, Piecewise Linear Approximation,
#' Approximate Reasoning, Imprecision, Vagueness, Randomness.
#'
#' @name FuzzyNumbers-package
#' @docType package
#' @title Tools to deal with fuzzy numbers in R
#' @author Marek Gagolewski \email{gagolews@@ibspan.waw.pl}
#' @references
#' \pkg{FuzzyNumbers} Homepage, \url{http://www.ibspan.waw.pl/~gagolews/FuzzyNumbers/}.\cr
#' Ban A.I. (2008), Approximation of fuzzy numbers by trapezoidal fuzzy numbers
#' preserving the expected interval, Fuzzy Sets and Systems 159, pp. 1327-1344.\cr
#' Ban A.I. (2009), On the nearest parametric approximation of a fuzzy number - Revisited,
#' Fuzzy Sets and Systems 160, pp. 3027--3047.\cr
#' Chanas S. (2001), On the interval approximation of a fuzzy number,
#' Fuzzy Sets and Systems 122, pp. 353-356.\cr
#' Coroianu L., Gagolewski M., Grzegorzewski P. (2013),
#' Nearest Piecewise Linear Approximation of Fuzzy Numbers, to appear in Fuzzy Sets and Systems.\cr
#' Delgado M., Vila M.A., Voxman W. (1998), On a canonical representation of a fuzzy number,
#' Fuzzy Sets and Systems 93, pp. 125-135.\cr
#' Dubois D., Prade H. (1978), Operations on fuzzy numbers, Int. J. Syst. Sci. 9, pp. 613-626.\cr
#' Dubois D., Prade H. (1987), The mean value of a fuzzy number, Fuzzy Sets and Systems 24, pp. 279-300.\cr
#' Gagolewski M. (2012), A Guide to the \code{FuzzyNumbers} Package for R,
#' \url{http://www.ibspan.waw.pl/~gagolews/FuzzyNumbers/doc/FuzzyNumbers-Tutorial.pdf}, 2012.\cr
#' Grzegorzewski P. (2010), Algorithms for trapezoidal approximations of fuzzy numbers
#' preserving the expected interval, In: Bouchon-Meunier B. et al (Eds.),
#' Foundations of Reasoning Under Uncertainty, Springer, pp. 85-98.\cr
#' Grzegorzewski P. (1998), Metrics and orders in space of fuzzy numbers,
#' Fuzzy Sets and Systems 97, pp. 83-94.\cr
#' Grzegorzewski P, Pasternak-Winiarska K. (2011), Trapezoidal approximations of fuzzy numbers
#' with restrictions on the support and core, Proc. EUSFLAT/LFA 2011, Atlantic Press, pp. 749-756.\cr
#' Klir G.J., Yuan B. (1995), Fuzzy sets and fuzzy logic. Theory and applications, Prentice Hall, New Jersey.\cr
#' Stefanini L., Sorini L. (2009), Fuzzy arithmetic with parametric LR fuzzy numbers,
#' In: Proc. IFSA/EUSFLAT 2009, pp. 600-605.\cr
#' Yeh C.-T. (2008), Trapezoidal and triangular approximations preserving the expected interval,
#' Fuzzy Sets and Systems 159, pp. 1345-1353.\cr
invisible(NULL)
