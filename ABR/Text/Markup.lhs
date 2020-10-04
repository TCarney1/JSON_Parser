% Markup.#ext
% This file was produced from String.lit

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

\module{Text.Markup} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Module \highlighttt{ABR.Text.Markup} is a collection of functions
that operate on strings wrt to Markup Languages.

\begin{code}
module ABR.Text.Markup (
      encodeHTML, encodeHTML', makeHTMLSafe, makeHTMLSafe',
      makeLatexSafe, latex2html
   ) where
\end{code}

\begin{code}
import Data.Char
\end{code}

\submodule{Maintenance notes} %%%%%%%%%%%%%%%%%%%%%%

Reviewed 2014-05-30: Made all the {\tt -Wall}s go away.\\
Reviewed 2013-11-22.\\
Reviewd 2009-04-13: Split off from ABR.Text.String.
   

\submodule{Making text safe for HTML} %%%%%%%%%%%%%%%%%%%

\highlighttt{encodeHTML}~$c$ returns $c$'s
special character encoding if $c \in \{$\verb"<",
\verb">", \verb"&", \verb'"'$\}$, otherwise $c$.
\highlighttt{encodeHTML'}~$c$ returns $c$'s
special character encoding, additionally encoding
all control characters.

\begin{code}
encodeHTML, encodeHTML' :: Char -> String
encodeHTML c = case c of
   '<' -> "&lt;"
   '>' -> "&gt;"
   '&' -> "&amp;"
   '"' -> "&quot;"
   _   -> [c]
encodeHTML' c = case c of
   '<' -> "&lt;"
   '>' -> "&gt;"
   '&' -> "&amp;"
   '"' -> "&quot;"
   _   -> if isControl c then
         "&#" ++ show (fromEnum c) ++ ";"
      else
         [c]
\end{code}

\noindent \highlighttt{makeHTMLSafe}~$\mathit{cs}$ encodes
all of the special characters in $\mathit{cs}$. It would be
counter productive to put text containing tags
through this filter.
\highlighttt{makeHTMLSafe}~$\mathit{cs}$ encodes all of
the special characters in $\mathit{cs}$ including all
control characters.

\begin{code}
makeHTMLSafe, makeHTMLSafe' :: String -> String
makeHTMLSafe  = concatMap encodeHTML
makeHTMLSafe' = concatMap encodeHTML'
\end{code}

\submodule{Making text safe for LaTeX} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\highlighttt{makeLatexSafe}~$\mathit{cs}$ makes $\mathit{cs}$
safe for inclusion in a \LaTeX\ document as plain text, by 
encoding some special characters. 

\begin{code}
makeLatexSafe :: String -> String
makeLatexSafe = concatMap safe . unwords . words
   where
   safe :: Char -> String
   safe c = case c of
      '$' -> "\\$"  
      '%' -> "\\%"  
      '_' -> "\\_"  
      '}' -> "\\}"  
      '&' -> "\\&"  
      '#' -> "\\#"  
      '{' -> "\\{"  
      '^' -> "\\textasciicircum "  
      '~' -> "\\textasciitilde "  
      '*' -> "\\textasteriskcentered "  
      '\\' -> "\\textbackslash "  
      '|' -> "\\textbar "  
      '>' -> "\\textgreater "  
      '<' -> "\\textless "  
      _   -> [c]  
\end{code}

\submodule{Converting LaTeX to HTML} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\noindent \highlighttt{latex2html}~$\mathit{cs}$ converts \LaTeX\ string
$\mathit{cs}$ to HTML. This is not meant for whole documents. It has some
basics for writing comments just like this one. This is used by
{\tt mashdoc}.

\begin{code} 
latex2html :: String -> String
latex2html = t2h [""]
\end{code} 


\noindent {\tt t2h}~$\mathit{ends}~\mathit{cs}$ converts \LaTeX\ 
string $\mathit{cs}$ to HTML. $\mathit{ends}$ is a list of 
element closes awaiting the next \verb+}+. We are currently 
in text mode.

\begin{code}
t2h :: [String] -> String -> String
t2h ends source = case source of
   '{' : cs -> 
      t2h ("" : ends) cs
   '}' : cs -> 
      case ends of
         []          -> t2h [] cs
         end : ends' -> end ++ t2h ends' cs
   '$' : cs ->
      m2h ends cs
   '~' : cs ->
      "&nbsp;" ++ t2h ends cs
   '`' : '`' : cs ->
      "&ldquo;" ++ t2h ends cs
   '\'' : '\'' : cs ->
      "&rdquo;" ++ t2h ends cs
   '"' : cs ->
      "&rdquo;" ++ t2h ends cs
   '\\' : cs -> 
      case cs of
         '\\' : cs' ->
            "<br />" ++ t2h ends cs'
         '$' : cs' ->
            '$' : t2h ends cs'
         '{' : cs' ->
            '{' : t2h ends cs'
         ' ' : cs' ->
            ' ' : t2h ends cs'
         _ ->
            let (command, rest) = getCommand cs
            in case command of
               "textbar" -> "|" ++ t2h ends rest
               "tt"   -> "<tt>" ++ t2h' "</tt>" ends rest
               "it"   -> "<i>" ++ t2h' "</i>" ends rest
               "bf"   -> "<b>" ++ t2h' "</b>" ends rest
               "verb" -> case rest of
                  ""     -> t2h ends ""
                  [r]    -> t2h ends [r]
                  r : rs -> 
                     let (vs, rs') = break (== r) rs
                     in "<tt>" ++ makeHTMLSafe vs ++
                        "</tt>" ++ t2h ends (drop 1 rs')
               _    -> t2h ends rest
   -- MORE CASES GO HERE
   c : cs -> 
      c : t2h ends cs
   [] -> 
      concat ends
\end{code}

\noindent {\tt t2h'}~$\mathit{newEnd}~\mathit{ends}~\mathit{cs}$
converts \LaTeX\ string $\mathit{cs}$ to HTML. $\mathit{ends}$ 
is a list of element closes awaiting the next \verb+}+. We 
are currently in text mode. $\mathit{newEnd}$ is a new closing tag to 
add to the innermost ending.

\begin{code}
t2h' :: String -> [String] -> String -> String
t2h' newEnd ends cs = case ends of
    []     -> t2h [newEnd] cs
    e : es -> t2h ((newEnd ++ e) : es) cs 
\end{code}

\noindent {\tt m2h}~$\mathit{ends}~\mathit{cs}$ converts \LaTeX\ 
string $\mathit{cs}$ to HTML. $\mathit{ends}$ is a list of 
element closes awaiting the next \verb+}+. We are currently 
in math mode.

\begin{code}
m2h :: [String] -> String -> String
m2h ends source = case source of
   '{' : cs -> 
      m2h ("" : ends) cs
   '}' : cs -> 
      case ends of
         []          -> m2h [] cs
         end : ends' -> end ++ m2h ends' cs
   '$' : cs ->
      t2h ends cs
   '<' : cs ->
      " &lt; " ++ m2h ends cs
   '>' : cs ->
      " &gt; " ++ m2h ends cs
   '-' : cs ->
      " &minus; " ++ m2h ends cs
   '^' : cs -> 
      case cs of
         '{' : cs' -> 
            "<sup>" ++ m2h' "</sup>" ends cs'
         c : cs' ->
            "<sup>" ++ m2h ends [c] ++ "</sup>"
            ++ m2h ends cs'
         [] ->
             []
   '_' : cs -> 
      case cs of
         '{' : cs' -> 
            "<sub>" ++ m2h' "</sub>" ends cs'
         c : cs' ->
            "<sub>" ++ m2h ends [c] ++ "</sub>"
            ++ m2h ends cs'
         [] ->
             []
   '\\' : cs -> 
      case cs of
         ',' : cs' ->
            ' ' : m2h ends cs'
         '$' : cs' ->
            '$' : m2h ends cs'
         '{' : cs' ->
            '{' : m2h ends cs'
         ' ' : cs' ->
            ' ' : m2h ends cs'
         _ ->
            let (command, rest) = getCommand cs
            in case command of
               "alpha"   -> "<i>&alpha;</i>" ++ 
                            m2h ends rest
               "beta"    -> "<i>&beta;</i>" ++ 
                            m2h ends rest
               "gamma"   -> "<i>&gamma;</i>" ++ 
                            m2h ends rest
               "delta"   -> "<i>&delta;</i>" ++ 
                            m2h ends rest
               "epsilon" -> "<i>&epsilon;</i>" ++ 
                            m2h ends rest
               "zeta"    -> "<i>&zeta;</i>" ++ 
                            m2h ends rest
               "eta"     -> "<i>&eta;</i>" ++ 
                            m2h ends rest
               "theta"   -> "<i>&theta;</i>" ++ 
                            m2h ends rest
               "iota"    -> "<i>&iota;</i>" ++ 
                            m2h ends rest
               "kappa"   -> "<i>&kappa;</i>" ++ 
                            m2h ends rest
               "lambda"  -> "<i>&lambda;</i>" ++ 
                            m2h ends rest
               "mu"      -> "<i>&mu;</i>" ++ 
                            m2h ends rest
               "nu"      -> "<i>&nu;</i>" ++ 
                            m2h ends rest
               "xi"      -> "<i>&xi;</i>" ++ 
                            m2h ends rest
               "omicron" -> "<i>&omicron;</i>" ++ 
                            m2h ends rest
               "pi"      -> "<i>&pi;</i>" ++ 
                            m2h ends rest
               "rho"     -> "<i>&rho;</i>" ++ 
                            m2h ends rest
               "sigma"   -> "<i>&sigma;</i>" ++ 
                            m2h ends rest
               "tau"     -> "<i>&tau;</i>" ++ 
                            m2h ends rest
               "upsilon" -> "<i>&upsilon;</i>" ++ 
                            m2h ends rest
               "phi"     -> "<i>&phi;</i>" ++ 
                            m2h ends rest
               "chi"     -> "<i>&chi;</i>" ++ 
                            m2h ends rest
               "psi"     -> "<i>&psi;</i>" ++ 
                            m2h ends rest
               "omega"   -> "<i>&omega;</i>" ++ 
                            m2h ends rest
               "Alpha"   -> "&Alpha;" ++ 
                            m2h ends rest
               "Beta"    -> "&Beta;" ++ 
                            m2h ends rest
               "Gamma"   -> "&Gamma;" ++ 
                            m2h ends rest
               "Delta"   -> "&Delta;" ++ 
                            m2h ends rest
               "Epsilon" -> "&Epsilon;" ++ 
                            m2h ends rest
               "Zeta"    -> "&Zeta;" ++ 
                            m2h ends rest
               "Eta"     -> "&Eta;" ++ 
                            m2h ends rest
               "Theta"   -> "&Theta;" ++ 
                            m2h ends rest
               "Iota"    -> "&Iota;" ++ 
                            m2h ends rest
               "Kappa"   -> "&Kappa;" ++ 
                            m2h ends rest
               "Lambda"  -> "&Lambda;" ++ 
                            m2h ends rest
               "Mu"      -> "&Mu;" ++ 
                            m2h ends rest
               "Nu"      -> "&Nu;" ++ 
                            m2h ends rest
               "Xi"      -> "&Xi;" ++ 
                            m2h ends rest
               "Omicron" -> "&Omicron;" ++ 
                            m2h ends rest
               "Pi"      -> "&Pi;" ++ 
                            m2h ends rest
               "Rho"     -> "&Rho;" ++ 
                            m2h ends rest
               "Sigma"   -> "&Sigma;" ++ 
                            m2h ends rest
               "Tau"     -> "&Tau;" ++ 
                            m2h ends rest
               "Upsilon" -> "&Upsilon;" ++ 
                            m2h ends rest
               "Phi"     -> "&Phi;" ++ 
                            m2h ends rest
               "Chi"     -> "&Chi;" ++ 
                            m2h ends rest
               "Psi"     -> "&Psi;" ++ 
                            m2h ends rest
               "Omega"   -> "&Omega;" ++ 
                            m2h ends rest
               "le"      -> " &le; " ++ 
                            m2h ends rest
               "ge"      -> " &ge; " ++ 
                            m2h ends rest
               "times"   -> " &times; " ++ 
                            m2h ends rest
               "pm"      -> " &plusmn; " ++ 
                            m2h ends rest
               "mathit"  -> m2h ends rest
               _    -> m2h ends rest
   -- MORE CASES GO HERE
   c : cs 
      | isAlpha c -> 
         "<i>" ++ c : "</i>" ++ m2h ends cs
      | otherwise -> 
         c : m2h ends cs
   [] -> 
      []
\end{code}

\noindent {\tt m2h'}~$\mathit{newEnd}~\mathit{ends}~\mathit{cs}$
converts \LaTeX\ string $\mathit{cs}$ to HTML. $\mathit{ends}$ 
is a list of element closes awaiting the next \verb+}+. We 
are currently in mayj mode. $\mathit{newEnd}$ is a new closing tag to 
add to the innermost ending.

\begin{code}
m2h' :: String -> [String] -> String -> String
m2h' newEnd ends cs = case ends of
    []     -> m2h [newEnd] cs
    e : es -> m2h ((newEnd ++ e) : es) cs 
\end{code}

\noindent {\tt getCommand}~$\mathit{cs}$ splits a string
into the first run of letters and the rest. A space
at the end of the run of letters is discarded (as per
\LaTeX).

\begin{code}
getCommand :: String -> (String, String)
getCommand cs = case cs of
   "" ->
      ("", "")
   ' ' : cs' ->
      ("", cs')
   c : cs' 
      | isAlpha c ->
         let (command, rest) = getCommand cs'
         in (c : command, rest)
      | otherwise ->
         ("", c : cs')
\end{code}

