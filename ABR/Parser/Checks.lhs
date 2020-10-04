% Checks.#ext
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

\module{Parser.Checks} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

The \highlighttt{ABR.Parser.Checks} module provides a some
functions for easy implementation of the parsing sequence. 

\begin{code}
module ABR.Parser.Checks (checkLex, checkParse) where
\end{code}

\begin{code}
import ABR.Util.Pos
import ABR.Parser
import ABR.Control.Check
\end{code}

\submodule{Maintenance notes} %%%%%%%%%%%%%%%%%%%%%%

Reviewed 2015-02-03. Passed {\tt hlint}.\\
Reviewed 2013-11-22.\\
Reviewed 2009-04-08: Separated from {\tt ABR.Parser}.
   

\submodule{Easy lexer and parser sequencing} %%%%%%%%%%%%%

\highlighttt{checkLex}~$l~\mathit{source}$ uses the
check abstraction to sequence the prelexing of the
$\mathit{source}$, lexing using $l$, error
detection and construction of error messages.

\begin{code}
checkLex ::
   Lexer -> Check String [((Tag,Lexeme),Pos)] String
checkLex lexer source = case lexer (preLex source) of
   Fail pos msg ->
      CheckFail $ "Lexer failed.\n\n"
           ++ errMsg pos msg source
   Error pos msg -> 
      CheckFail $ "Lexer error.\n\n"
           ++ errMsg pos msg source
   OK (ls,_)     ->
      CheckPass ls
\end{code}

\noindent \highlighttt{checkParse}~$l~p~\mathit{source}$ uses the
check abstraction to sequence the prelexing of the
$\mathit{source}$, lexing using $l$, parsing using $p$, error
detection and construction of error messages.

\begin{code}
checkParse :: Lexer -> Parser a -> Check String a String
checkParse lexer parser source
   = case checkLex lexer source of
        CheckPass tlps -> 
           case parser tlps of
              Fail pos msg ->
                 CheckFail $ "Parser failed.\n\n"
                             ++ errMsg pos msg source
              Error pos msg -> 
                 CheckFail $ "Parser error.\n\n"
                             ++ errMsg pos msg source
              OK (thing,_) ->
                 CheckPass thing
        CheckFail msg -> 
           CheckFail msg
\end{code}

