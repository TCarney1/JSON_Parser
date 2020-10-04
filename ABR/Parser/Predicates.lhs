% Predicates.#ext
% This file was produced from Parser.lit

% ABRHLibs -- a personal library of Haskell modules
% Copyright (C) 2007, 2008,  Andrew Rock
% 
% This program is free software; you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation; either version 2 of the License, or
% (at your option) any later version.
% 
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
% 
% You should have received a copy of the GNU General Public License
% along with this program; if not, write to the Free Software
% Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

\module{Parser.Predicates} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

The \highlighttt{ABR.Parser.Predicates} module provides some
frequently used predicates that depend on lexing/parsing. 

\begin{code}
module ABR.Parser.Predicates (
      isCardinal, isFixed, isFloat, isSignedCardinal, 
      isSignedFixed, isSignedFloat
   ) where
\end{code}

\begin{code}
import ABR.Control.Check
import ABR.Parser.Checks
import ABR.Parser.Lexers
\end{code}

\submodule{Maintenance notes} %%%%%%%%%%%%%%%%%%%%%%

Reviewed 2015-02-03. Passed {\tt hlint}.\\
Reviewed 2013-11-22.\\
Reviewed 2012-11-03: Split from {\tt ABR.Text.Parser}.
   

\submodule{isNumber predicates} %%%%%%%%%%%%%%%%%%%%%%%%%%%%

\noindent \highlighttt{isCardinal},  \highlighttt{isFixed},
\highlighttt{isFloat}, \highlighttt{isSignedCardinal},
\highlighttt{isSignedFixed} and \highlighttt{isSignedFloat} 
are all predicated that test whether a string could be parsed
as a number.

\begin{code}
isCardinal, isFixed, isFloat, isSignedCardinal, 
   isSignedFixed, isSignedFloat :: String -> Bool
isCardinal cs = case checkLex cardinalL cs of
   CheckPass _ -> True
   _           -> False
isFixed cs = case checkLex fixedL cs of
   CheckPass _ -> True
   _           -> False
isFloat cs = case checkLex floatL cs of
   CheckPass _ -> True
   _           -> False
isSignedCardinal cs = case checkLex signedCardinalL cs of
   CheckPass _ -> True
   _           -> False
isSignedFixed cs = case checkLex signedFixedL cs of
   CheckPass _ -> True
   _           -> False
isSignedFloat cs = case checkLex signedFloatL cs of
   CheckPass _ -> True
   _           -> False
\end{code}

