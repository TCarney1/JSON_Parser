% Check.#ext
% This file was produced from Check.lit

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

\module{Control.Check} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Module \highlighttt{ABR.Control.Check} implements checks
as operations to be performed that may
succeed or fail. Checks are often performed in a
sequence. Composing lots of checks can lead to big,
ugly cascades of case expressions. This module
provides a way to do it more compactly and
neatly.\footnote{Thanks to Daniel Young for
suggested extensions to this module.}

\begin{code}
module ABR.Control.Check (
      CheckResult(..), Check, (&?), (+?), (??), (*?), (#?)
   ) where
\end{code}

\begin{code}
infixl 2 &?, +?, #?, ??, *?
\end{code}

\submodule{Maintenance notes} %%%%%%%%%%%%%%%%%%%%%%

Reviewed 2016-05-30. Replaced {\tt pam} with \verb"<*>".\\
Reviewed 2015-02-02. Passed {\tt hlint}.\\
Reviewed 2014-05-28: Made all {\tt -Wall}s go away.\\
Reviewed 20013-11-07.\\
Reviewed 2009-04-08: Changed to {\tt ABR.\emph{Control}.Check}.
   

\submodule{Data type} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

The result of a {\tt Check}, a
\highlighttt{CheckResult} is either a
\highlighttt{CheckPass} with the correct result, or
a \highlighttt{CheckFail} with some alternate data,
probably an error message string.

\begin{code}
data CheckResult passType failType =   CheckPass passType
                                     | CheckFail failType
   deriving (Eq, Ord, Show)
\end{code}

\noindent A \highlighttt{Check} takes some object and returns a
{\tt CheckResult}.
                      
\begin{code}
type Check objectType passType failType
        = objectType -> CheckResult passType failType
\end{code}


\submodule{Sequencing checks} %%%%%%%%%%%%%%%%%%%%%%%%%%%%

$c_{1}$~\highlighttt{\&?}~$c_{2}$ sequence composes check
$c_{1}$ and $c_{2}$ in that order. $c_{1}$ is applied
first. If it succeeds, then $c_{2}$ is applied to
the result. 

\begin{code}
(&?) :: Check a b d -> Check b c d -> Check a c d
(&?) c1 c2 x = case c1 x of
    CheckPass x' -> c2 x'
    CheckFail x' -> CheckFail x'
\end{code}


\submodule{Parallel checks} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

$r_{1}$~\highlighttt{+?}~$r_{2}$ combines check
results $r_{1}$ and $r_{2}$. If both results are
passes only $r_{2}$ is returned. If only one result
is a pass, then the other failing result is
returned. If both are fails, then a fail is
returned with the catenation of the error messages.
This leads to the restriction that the fail data
type must be a list type.

\begin{code}
(+?) :: CheckResult a [b] -> CheckResult a [b]
        -> CheckResult a [b]
r1 +? r2 = case r1 of
   CheckPass _  -> r2
   CheckFail m1 -> case r2 of
      CheckPass _  -> r1
      CheckFail m2 -> CheckFail (m1 ++ m2)
\end{code}


\noindent $c$~\highlighttt{\#?}~$\mathit{xs}$ applies check
$c$ to each of the elements of $\mathit{xs}$ in parallel,
returning only the last result if all checks pass
or all of the error messages catenated if any
checks fail.

\begin{code}
(#?) :: Check a b [c] -> Check [a] b [c]
c #? xs = foldl1 (+?) $ map c xs
\end{code}


\noindent $\mathit{cs}$~\highlighttt{??}~$x$ applies all the
checks in $\mathit{cs}$ to $x$ in parallel; returning only
the last result if all checks pass or all of the
error messages catenated if any checks fail.

\begin{code}
(??) :: [Check a b [c]] -> Check a b [c]
cs ?? x = foldl1 (+?) $ cs <*> [x]
\end{code}


\noindent $\mathit{cs}$~\highlighttt{*?}~$\mathit{xs}$ applies all
the checks in $\mathit{cs}$ to all of the elements of $\mathit{xs}$
in parallel; returning only the last result (the
last check applied to the last thing) if all checks
pass or all of the error messages catenated if any
checks fail.

\begin{code}
(*?) :: [Check a b [c]] -> Check [a] b [c]
cs *? xs = foldl1 (+?) [c x | c <- cs, x <- xs]
\end{code}

