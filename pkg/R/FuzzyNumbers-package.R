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
## along with FuzzyNumbers If not, see <http://www.gnu.org/licenses/>.


#' FuzzyNumbers is an open source (LGPL 3) package for R.
#'     It provides S4 classes and methods to deal with Fuzzy Numbers.
#'     It may be used both by the practitioners (computations of many
#'     operations on fuzzy numbers, approximation by trapezoidal and piecewise
#'     linear fuzzy numbers, etc.) but also by the researchers
#'     in fuzzy numbers theory.
#'
#' Fuzzy set theory lets us effectively and quite intuitively represent
#' imprecise or vague information. Fuzzy numbers, which form a particular
#' subclass of fuzzy sets of the real line, play a significant role
#' in many important theoretical as well as practical considerations
#' since we often describe our knowledge about objects
#' through numbers, e.g. "I'm about 180 cm tall"
#' or "The rocket was launched between 2 and 3 p.m.".
#'
#' For the formal definition of a fuzzy number
#' please refer to the \code{\link{FuzzyNumber}} class man page.
#' Note that this package also deals with particular types
#' of fuzzy numbers like Trapezoidal and Piecewise Linear
#' Fuzzy Numbers (see \code{\link{TrapezoidalFuzzyNumber}}
#' and [TO DO]).
#'
#' The package aims to provide the following functionality.
#' \enumerate{
#'    \item Operations on Fuzzy Numbers [TO DO],
#'    \item Defuzzification and Approximation of Fuzzy Numbers by Triangular
#'      and Piecewise Linear Fuzzy Numbers (see \code{\link{expectedValue}},
#'      \code{\link{weightedExpectedValue}}, \code{\link{value}}, \code{\link{trapezoidalApproximation}}),
#'    \item Visualization (see \code{\link{plot}}),
#'    \item Aggregation of Fuzzy Numbers [TO DO],
#'    \item Ranking of Ruzzy Numbers [TO DO],
#'    \item \dots
#' }
#'
#' Please feel free to send any comments and feature requests to the author
#' (see also \url{http://www.ibspan.waw.pl/~gagolews}).
#'
#' For a complete list of classes and methods, call \code{library(help="FuzzyNumbers")}.
#' \cr\cr
#'
#' \bold{Keywords}: Fuzzy Numbers, Fuzzy Sets, Shadowed Sets,
#' Trapezoidal Approximation, Piecewise Linear Approximation,
#' Approximate Reasoning, Imprecision, Vagueness.
#'
#' @name FuzzyNumbers-package
#' @docType package
#' @title Tools to deal with fuzzy numbers in R
#' @author Marek Gagolewski \email{gagolews@@ibspan.waw.pl}
#' @references
#' FuzzyNumbers Package Homepage, \url{http://www.ibspan.waw.pl/~gagolews/FuzzyNumbers/}.\cr
#' Dubois D., Prade H. (1978), Operations on fuzzy numbers, \emph{Int. J. Syst. Sci.} 9, pp. 613-626.\cr
#' Ban A.I. (2008), Approximation of fuzzy numbers by trapezoidal fuzzy numbers
#' preserving the expected interval, Fuzzy Sets and Systems 159, pp. 1327-1344.\cr
#' Grzegorzewski P. (2010), Algorithms for trapezoidal approximations of fuzzy numbers
#' preserving the expected interval, in: Bouchon-Meunier B. et al (Eds.),
#' Foundations of Reasoning Under Uncertainty, Springer, pp. 85-98.\cr
#' Grzegorzewski P, Pasternak-Winiarska K. (2011), Trapezoidal approximations of fuzzy numbers
#' with restrictions on the support and core, Proc. EUSFLAT/LFA 2011, Atlantic Press, pp. 749-756.\cr
#' Yeh C.-T. (2008), Trapezoidal and triangular approximations preserving the expected interval,
#' Fuzzy Sets and Systems 159, pp. 1345-1353.\cr
NA



.onLoad <- function(lib, pkg)
{
   packageStartupMessage("FuzzyNumbers loaded. For more information please visit http://www.ibspan.waw.pl/~gagolews/FuzzyNumbers/.");
}
