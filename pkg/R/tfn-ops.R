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




setMethod("+",
   signature(e1 = "TrapezoidalFuzzyNumber", e2 = "TrapezoidalFuzzyNumber"),
   function (e1, e2)
   {
      TrapezoidalFuzzyNumber(e1@a1+e2@a1, e1@a2+e2@a2, e1@a3+e2@a3, e1@a4+e2@a4);
   }
);

setMethod("-",
   signature(e1 = "TrapezoidalFuzzyNumber", e2 = "TrapezoidalFuzzyNumber"),
   function (e1, e2)
   {
      TrapezoidalFuzzyNumber(e1@a1-e2@a4, e1@a2-e2@a3, e1@a3-e2@a2, e1@a4-e2@a1);
   }
);

