% Args.#ext
% This file was produced from Args.lit

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

\module{Util.Args} %%%%%%%%%%%%%%%%%%%%%%%%%%

Module \highlighttt{ABR.Util.Args} provides a way to pick apart the meanings of
command line arguments.

\begin{code}
{-# LANGUAGE CPP #-}
\end{code}

\begin{code}
module ABR.Util.Args (
      ShortName, LongName, OptName, ParamValue,
      OptSpec(..), OptVal(..), Options, findOpts,
      assertFlagPlus, assertFlagMinus, deleteFlag,
      insertParam, deleteParam, emptyOptions,
      lookupFlag, lookupParam, lookupQueue
# ifdef NO_GLOB
-- legacy for hobbit
# else 
      , glob
# endif
   ) where
\end{code}

\begin{code}
import qualified Data.List as L
import qualified Data.Sequence as S
import qualified Data.Foldable as F
import qualified Data.Map as M
\end{code}

\noindent Hackage package {\tt Glob}:

\begin{code}
# ifdef NO_GLOB
-- legacy for hobbit
# else 
import qualified System.FilePath.Glob as G
# endif
\end{code}

\submodule{Maintenance notes} %%%%%%%%%%%%%%%%%%%%%%

Reviewed 2019-12-28: Added long forms, maintaining backwards compatibility.\\
Reviewed 2017-12-28: Updated {\tt glob} call.\\
Reviewed 2016-05-28: Added {\tt glob} dependency note.\\
Reviewed 2015-02-12: Made globbing conditional.\\
Reviewed 2015-02-12: Added filename globbing.\\
Reviewed 2015-02-02: Passed {\tt hlint}.\\
Reviewed 2014-05-29: Made all the {\tt -Wall}s go away.\\
Reviewed 2013-11-06.\\
Reviewed 2012-11-24: Moved into {\tt ABR.Util}. \\
Reviewed 2012-11-01: Keep this module but make it use the 
data types in the CHC libs instead of my own data types -- done.
   
\submodule{Dependencies} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

{\tt ABR.Util.Args} uses the {\tt Glob} package. Install
that on your system with {\tt cabal}:

\begin{Verbatim}
cabal install Glob
\end{Verbatim}

\noindent To compile where {\tt Glob} is not available use
the {\tt ghc} compiler option \verb"-DNO_GLOB".

\submodule{Language tweaks} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\begin{code}
undefined' :: String -> a
undefined' label = error $
   "Intentional undefined at module ABR.Util.Args, " ++
   " with label \"" ++ label ++ "\"."
\end{code}

\submodule{Data types} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Options have either short forms with
\highlighttt{ShortName}s, long forms with
\highlighttt{LongName}s, or both. After options are
parsed it doesn't matter whether an \highlighttt{OptName}
is long or short. All are just strings.

\begin{code}
type ShortName = String
type LongName = String
type OptName = String
type ParamValue = String
\end{code}

\noindent An \highlighttt{OptSpec} is used to specify the
types of options expected. 

\begin{code}
data OptSpec = 
     FlagS   ShortName 
   | ParamS  ShortName 
   | QueueS  ShortName
   | FlagL   LongName 
   | ParamL  LongName 
   | QueueL  LongName
   | FlagSL  ShortName LongName
   | ParamSL ShortName LongName
   | QueueSL ShortName LongName
   deriving (Eq, Show)
\end{code}

\noindent An option is one of:

\begin{itemize}
   \item a flag to be set or unset. 
   
      Specify with 
      \highlighttt{FlagS}~$\mathit{shortName}$,
      \highlighttt{FlagL}~$\mathit{longName}$, or
      \highlighttt{FlagSL}~$\mathit{shortName}~\mathit{longName}$.
      
      Users set with 
      \verb"+"$\mathit{shortName}$ or
      \verb"--"$\mathit{longName}$, and unset with
      \verb"-"$\mathit{shortName}$ or
      \verb"--no-"$\mathit{longName}$.
      
   \item a parameter with a value. 
   
      Specify with
      \highlighttt{ParamS}~$\mathit{shortName}$,
      \highlighttt{ParamL}~$\mathit{longName}$, or
      \highlighttt{ParamS}~$\mathit{shortName}$~$\mathit{longName}$. 
      
      Users provide values with
      \verb"-"$\mathit{shortName}~\mathit{value}$ or
      \verb"--"$\mathit{longName}~\mathit{value}$.
      
   \item a parameter that can have multiple values.
      The order of the multiple values might be
      significant. In this case a queue of strings
      is returned. 
      
      Specify with
      \highlighttt{QueueS}~$\mathit{shortName}$,
      \highlighttt{QueueL}~$\mathit{longName}$, or
      \highlighttt{QueueS}~$\mathit{shortName}$~$\mathit{longName}$. 
      
      Users provide values with
      \verb"-"$\mathit{shortName}~\mathit{value}$ or
      \verb"--"$\mathit{longName}~\mathit{value}$.
\end{itemize}   

\noindent An \highlighttt{OptVal} is used to
indicate presence of a command line option. Flags
might be \highlighttt{FlagPlus} or
\highlighttt{FlagMinus}. Parameters will either
return the
\highlighttt{ParamValue}~$\mathit{value}$ or an
indication that the value was missing,
\highlighttt{ParamMissingValue}. Queue parameters
return \highlighttt{ParamQueue}~$\mathit{queue}$.
Missing values for queue parameters might yield an
empty queue.

\begin{code}
data OptVal = 
      FlagPlus | FlagMinus | 
      ParamValue String | ParamMissingValue |
      ParamQueue (S.Seq String)
   deriving Show
\end{code}

\noindent An \highlighttt{Options} is a map
from option names to their values. For clarity if there exists 
both a short name and a long name for an option, the map
will \emph{always} be keyed by the long name, no matter which
form the user used. This will make
code using this module clearer.

\begin{code}
type Options = M.Map OptName OptVal
\end{code}

\submodule{Empty options} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\highlighttt{emptyOptions} is an empty {\tt Options}.

\begin{code}
emptyOptions :: Options
emptyOptions = M.empty
\end{code}

\submodule{Specs} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

{\tt findShortSpec}~$\mathit{specs}~\mathit{name}$ returns the first
spec in $\mathit{specs}$ that matches $\mathit{name}$ with its short name.

\begin{code}
findShortSpec :: [OptSpec] -> ShortName -> Maybe OptSpec
findShortSpec specs name = L.find match specs
   where
   match :: OptSpec -> Bool
   match spec = case spec of
      FlagS   sn    -> name == sn
      ParamS  sn    -> name == sn
      QueueS  sn    -> name == sn
      FlagL   _     -> False
      ParamL  _     -> False 
      QueueL  _     -> False
      FlagSL  sn _  -> name == sn
      ParamSL sn _  -> name == sn
      QueueSL sn _  -> name == sn
\end{code}

\noindent {\tt findLongSpec}~$\mathit{specs}~\mathit{name}$ returns the first
spec in $\mathit{specs}$ that matches $\mathit{name}$ with its long name.

\begin{code}
findLongSpec :: [OptSpec] -> LongName -> Maybe OptSpec
findLongSpec specs name = L.find match specs
   where
   match :: OptSpec -> Bool
   match spec = case spec of
      FlagS   _     -> False
      ParamS  _     -> False
      QueueS  _     -> False
      FlagL   ln    -> name == ln
      ParamL  ln    -> name == ln 
      QueueL  ln    -> name == ln
      FlagSL  _  ln -> name == ln
      ParamSL _  ln -> name == ln
      QueueSL _  ln -> name == ln
\end{code}

\noindent {\tt isFlag}~$\mathit{spec}$ returns {\tt True} if this $\mathit{spec}$
is a flag type.

\begin{code}
isFlag :: OptSpec -> Bool
isFlag spec = case spec of
   FlagS   _    -> True
   FlagL   _    -> True
   FlagSL  _  _ -> True
   _            -> False
\end{code}

\noindent {\tt isParam}~$\mathit{spec}$ returns {\tt True} if this $\mathit{spec}$
is a param type.

\begin{code}
isParam :: OptSpec -> Bool
isParam spec = case spec of
   ParamS  _    -> True
   ParamL  _    -> True 
   ParamSL _  _ -> True
   _            -> False
\end{code}

\noindent {\tt isQueue~$\mathit{spec}$ returns {\tt True} if this $\mathit{spec}$
is a queue type.

\begin{code}
isQueue :: OptSpec -> Bool
isQueue spec = case spec of
   QueueS  _    -> True
   QueueL  _    -> True
   QueueSL _  _ -> True
   _            -> False
\end{code}

\noindent {\tt prefName}~$\mathit{spec}$ returns the preferred (longest)
name for $\mathit{spec}$.

\begin{code}
prefName :: OptSpec -> OptName
prefName spec = case spec of
   FlagS   sn    -> sn
   ParamS  sn    -> sn
   QueueS  sn    -> sn
   FlagL   ln    -> ln
   ParamL  ln    -> ln 
   QueueL  ln    -> ln
   FlagSL  _  ln -> ln
   ParamSL _  ln -> ln
   QueueSL _  ln -> ln
\end{code}

\submodule{Option detection} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\highlighttt{findOpts}~$\mathit{optSpecs}~\mathit{
args}$ returns $(\mathit{options},
\mathit{leftovers})$, where:\\
$\mathit{optSpecs}$ is
a list of option specifications; $\mathit{args}$ is
a list of command line arguments;
$\mathit{options}$ is a mapping from the option
names to the values found; and $\mathit{leftovers}$
is a list of any unconsumed arguments, typically
file paths.
      
\begin{code}
findOpts :: [OptSpec] -> [String] -> (Options, [FilePath])
findOpts specs = fo
   where
   -- find a spec, with short or long names
   fss, fls :: OptName -> Maybe OptSpec
   fss = findShortSpec specs
   fls = findLongSpec specs
   -- insert/replace an option
   rep, enq :: Options -> String -> OptVal -> Options
   rep t cs v = M.insertWith const cs v t
   -- enqueue a parameter
   enq t cs v = M.insertWith enq' cs v t
   enq' :: OptVal -> OptVal -> OptVal
   enq' (ParamQueue q) (ParamQueue q') = 
      ParamQueue (q' S.>< q)
   enq' _ _ = undefined' "enq'"
   -- loop through command line arguments
   fo :: [String] -> (Options, [FilePath])
   fo args = 
      let -- no matching spec, add cs as FilePath
          fp cs css = 
             let (t, css') = fo css
             in (t, cs : css')
          -- matching specs
          minus spec css = 
            let (t, css') = fo css
            in (rep t (prefName spec) FlagMinus, css')
          plus spec css = 
            let (t, css') = fo css
            in (rep t (prefName spec) FlagPlus, css')
          param spec css = case css of
             []->    
                (rep M.empty (prefName spec) 
                 ParamMissingValue, [])
             cs' : css' ->
                let (t, css'') = fo css'
                in (rep t (prefName spec) 
                    (ParamValue cs'), css'')
          queue spec css = case css of
             [] ->
                (enq M.empty (prefName spec) 
                 (ParamQueue S.empty), [])
             cs' : css' ->
                let (t, css'') = fo css'
                in (enq t (prefName spec)
                    (ParamQueue (S.singleton cs')), css'')
      in case args of
         []       -> (M.empty, [])
         cs : css
            | "--no-" `L.isPrefixOf` cs ->
               case fls (drop 5 cs) of
                  Nothing           -> fp cs css
                  Just spec 
                     | isFlag spec  -> minus spec css
                     | otherwise    -> fp cs css
            | "--" `L.isPrefixOf` cs ->
               case fls (drop 2 cs) of
                  Nothing           -> fp cs css
                  Just spec 
                     | isFlag  spec -> plus  spec css
                     | isParam spec -> param spec css
                     | isQueue spec -> queue spec css
                     | otherwise    -> fp cs css
            | "-" `L.isPrefixOf` cs ->
               case fss (tail cs) of
                  Nothing           -> fp cs css
                  Just spec 
                     | isFlag  spec -> minus spec css
                     | isParam spec -> param spec css
                     | isQueue spec -> queue spec css
                     | otherwise    -> fp cs css
            | "+" `L.isPrefixOf` cs ->
               case fss (tail cs) of
                  Nothing           -> fp cs css
                  Just spec 
                     | isFlag spec  -> plus spec css
                     | otherwise    -> fp cs css
            | otherwise ->
               fp cs css
\end{code}

\submodule{Adding and deleting options} %%%%%%%%%%%%%%%%%%%

Flags can be asserted positive or negative or deleted, with
\highlighttt{assertFlagPlus}, \highlighttt{assertFlagMinus},
and \highlighttt{deleteFlag}, respectively.

\begin{code}
assertFlagPlus, assertFlagMinus, deleteFlag ::
   OptName -> Options -> Options
assertFlagPlus name  = M.insert name FlagPlus 
assertFlagMinus name = M.insert name FlagMinus 
deleteFlag           = M.delete  
\end{code}

\noindent Params can be inserted or deleted, with
\highlighttt{insertParam} and \highlighttt{deleteParam},
respectively.

\begin{code}
insertParam :: OptName -> ParamValue -> Options -> Options
insertParam name value = M.insert name (ParamValue value)
\end{code}

\begin{code}
deleteParam :: OptName -> Options -> Options
deleteParam = M.delete 
\end{code}

\submodule{Looking up options} %%%%%%%%%%%%%%%%%%%%%%%%%%%%

\highlighttt{lookupFlag}~$\mathit{name}~\mathit{options}~\mathit{def}$
returns the value ({\tt FlagPlus} means {\tt True}) stored
for the $\mathit{name}$ed flag in $\mathit{options}$ or
$\mathit{def}$ if it has not been properly specified.
      
\begin{code}
lookupFlag :: OptName -> Options -> Bool -> Bool
lookupFlag name options def = 
   case M.lookup name options of
      Just FlagPlus  -> True
      Just FlagMinus -> False
      _              -> def
\end{code}

\noindent
\highlighttt{lookupParam}~$\mathit{name}~\mathit{options}~\mathit{def}$
returns the value stored for the $\mathit{name}$ed
parameter in $\mathit{options}$ or $\mathit{def}$ if it
has not been properly specified.
      
\begin{code}
lookupParam :: 
   OptName -> Options -> ParamValue -> ParamValue
lookupParam name options def = 
   case M.lookup name options of
      Just (ParamValue v) -> v
      _                   -> def
\end{code}

\noindent
\highlighttt{lookupQueue}~$\mathit{name}~\mathit{options}$
returns the list stored for the $\mathit{name}$ed
queue parameter in $\mathit{options}$ or {\tt []} if it
has not been properly specified.
      
\begin{code}
lookupQueue :: OptName -> Options -> [ParamValue]
lookupQueue name options = 
   case M.lookup name options of
      Just (ParamQueue q) -> F.toList q
      _                   -> []
\end{code}

\submodule{Filename globbing} %%%%%%%%%%%%%%%%%%%%%%%%%%%%

\highlighttt{glob}~$\mathit{paths}$ returns the really existing
file names that match the $\mathit{paths}$ which may contain
wildcards.

\begin{code}
# ifdef NO_GLOB
-- legacy for hobbit
# else 
glob :: [FilePath] -> IO [FilePath]
glob paths = do
   pathss <- G.globDir (map G.compile paths) []
   return $ L.nub $ concat pathss
# endif
\end{code}

