% List.#ext
% This file was produced from List.lit

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

\module{Data.List} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Module \highlighttt{ABR.Data.List} is a collection of functions
that operate on lists.

\begin{code}
module ABR.Data.List (
      merge, msort, split, cartProd, interleave, separate,
      fragments, fragments', dropEach, permutations,
      permutations', combinations, subBag, bagElem,
      powSet, powSet_ge1, powSet', powSet_ge1',
      properSublists, pPlus, meet, disjoint, allUnique,
      duplicates, snub, (+:), mnub, isSubset, findSubset,
      noSuperSets, isSubSequence, notSubSequence, chop,
      chops, subsSuffix, odiff, osect, ounion,
      sortByLength, trimN, trim2, replace1, replaceAll
   ) where
\end{code}

\begin{code}
import qualified Data.List as L
\end{code}

\submodule{Maintenance notes} %%%%%%%%%%%%%%%%%%%%%%

Reviewed 2015-02-02. Passed {\tt hlint}.\\
Reviewed 2014-05-29: Made all the {\tt -Wall}s go away.\\
Reviewed 2013-11-10.
   
\submodule{Language tweaks} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\begin{code}
undefined' :: String -> a
undefined' label = error $
   "Intentional undefined at module ABR.Data.List, \
   \ with label \"" ++ label ++ "\"."
\end{code}

\submodule{Sorting} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\highlighttt{msort}~$\mathit{lt}~\mathit{xs}$ sorts
$\mathit{xs}$ using $\mathit{lt}$ as the less-than operator.

\begin{code}
msort :: (a -> a -> Bool) -> [a] -> [a]
{-# DEPRECATED msort "Use Data.List.sort" #-}
msort _  []  = []
msort _  [x] = [x]
msort lt xs  = let (ys,zs) = split xs
               in merge lt (msort lt ys) (msort lt zs)
\end{code}

\noindent \highlighttt{sortByLength}~$\mathit{xss}$ sorts a 
list of lists into non-descending order of length.

\begin{code}
sortByLength :: [[a]] -> [[a]]
sortByLength = map snd . 
   L.sortBy (\(a,_) (b,_) -> compare a b) .
   map (\xs -> (length xs, xs))
\end{code}

\submodule{Combinatorics} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\highlighttt{merge}~$\mathit{lt}~\mathit{xs}~\mathit{ys}$ merges lists $\mathit{xs}$ and $\mathit{ys}$
preserving the non-descending order in $\mathit{xs}$ and
$\mathit{ys}$ using $\mathit{lt}$ to decide what is less that what.

\begin{code}
merge :: (a -> a -> Bool) -> [a] -> [a] -> [a]
merge _ [] ys = ys
merge _ xs [] = xs
merge lt (x:xs) (y:ys) 
   | x `lt` y  = x : merge lt xs (y:ys)
   | otherwise = y : merge lt (x:xs) ys
\end{code}


\noindent \highlighttt{split}~$\mathit{xs}$ splits $\mathit{xs}$ into two lists of the
alternate elements of $\mathit{xs}$.

\begin{code}
split :: [a] -> ([a],[a])
split xs = case xs of
   []          -> ([],[])
   [x]         -> ([x],[])
   (x1:x2:xs') -> let (xs1,xs2) = split xs'
                  in (x1:xs1, x2:xs2)
\end{code}


\noindent \highlighttt{cartProd} produces the cartesian 
product of an arbitrary number of lists. That is,
{\tt cartProd~[}$\mathit{xs}_{1}, \, \mathit{xs}_{2}, \, \ldots${\tt]}
returns $\mathit{xs}_{1} \times \mathit{xs}_{2} \times \ldots$
Note: {\tt Prelude.sequence} can be used to do the same job.

\begin{code}
cartProd :: [[a]] -> [[a]]
cartProd xs = case xs of
   []      -> [[]]
   xs':xss -> [x : ys | x <- xs', ys <- cartProd xss]
\end{code}

\noindent \highlighttt{interleave}~$x~\mathit{xs}$ returns the list of lists formed by
inserting $x$ in each possible place in $\mathit{xs}$.

\begin{code}
interleave :: a -> [a] -> [[a]]
interleave x ys = case ys of
   []    -> [[x]]
   y:ys' -> (x:y:ys') : map (y:) (interleave x ys')
\end{code}

\noindent \highlighttt{separate}~$\mathit{xs}~\mathit{yss}$ 
concats $\mathit{yss}$, with $\mathit{xs}$ interspersed.

\begin{code}
separate :: [a] -> [[a]] -> [a]
{-# DEPRECATED separate "Use Data.List.intercalate" #-}
separate = L.intercalate
\end{code}

\noindent \highlighttt{fragments}~$\mathit{xs}$ returns the list of fragments\\
$(\mathit{beforeElems}, \mathit{elem}, \mathit{afterElems})$ for each element of $\mathit{xs}$.
The elements in $\mathit{beforeElems}$ are in reverse order with
respect to $\mathit{xs}$.

\begin{code}
fragments :: [a] -> [([a],a,[a])]
fragments = f []
   where
   f _ []     = []
   f b (e:a)  = (b,e,a) : f (e:b) a
\end{code}

\noindent \highlighttt{fragments'}~$\mathit{xs}$ returns the list of fragments
$(\mathit{elem}, \mathit{otherElems})$ for each elements of $\mathit{xs}$. The elements
in $\mathit{otherElems}$ are in no particular order.

\begin{code}
fragments' :: [a] -> [(a,[a])]
fragments' = map (\(xs,y,zs) -> (y, xs ++ zs)) . fragments
\end{code}

\noindent \highlighttt{dropEach}~$\mathit{xs}$ returns the list of lists obtained by
deleting each element of $\mathit{xs}$.

\begin{code}
dropEach :: [a] -> [[a]]
dropEach = map (\(xs,_,zs) -> xs ++ zs) . fragments
\end{code}

\noindent \highlighttt{permutations}~$k~\mathit{xs}$ returns all the permutations
of $k$ elements selected from $\mathit{xs}$.
Precondition: $0 <= k <=$ {\tt length} $\mathit{xs}$.

\begin{code}
permutations :: Int -> [a] -> [[a]]
permutations _ [] = [[]]
permutations 0 _  = [[]]
permutations k xs = [x : xs' | (bs,x,as) <- fragments xs,
                     xs' <- permutations (k-1) (bs++as)]
\end{code}

\noindent \highlighttt{permutations'}~$\mathit{xs}$ returns all the permutations
of the elements of $\mathit{xs}$.

\begin{code}
permutations' :: [a] -> [[a]]
{-# DEPRECATED permutations' "Use Data.List.permutations" #-}
permutations' [] = [[]]
permutations' (x:xs) = [zs | ys <- permutations' xs,
                             zs <- interleave x ys]
\end{code}

\noindent \highlighttt{combinations}~$k~\mathit{xs}$ returns all the combinations
of $k$ elements drawn from $\mathit{xs}$. Precondition:
$0 <= k <=$ {\tt length} $\mathit{xs}$.

\begin{code}
combinations :: Int -> [a] -> [[a]]
combinations k xs
   = comb k (length xs) xs
     where
     comb :: Int -> Int -> [a] -> [[a]]
     comb k' l (x:xs')
        | k' == 0    = [[]]
        | k' == l    = [x:xs']
        | otherwise = map (x:) (comb (k'-1) (l-1) xs')
                      ++ comb k' (l-1) xs'
     comb _ _ [] = undefined' "comb"
\end{code}


\noindent \highlighttt{powSet}~$\mathit{xs}$ returns the list of sub-lists of $\mathit{xs}$.
This version does not return them in an order that
monotonically increases in length.

\begin{code}
powSet :: [a] -> [[a]]
powSet []      = [[]]
powSet (x:xs)  = let pss = powSet xs
                 in pss ++ map (x:) pss
\end{code}

\noindent \highlighttt{powSet\_ge1}~$\mathit{xs}$ returns the list of sub-lists of
$\mathit{xs}$ with at least 1 element. This version does
not return them in an order that monotonically increases
in length.

\begin{code}
powSet_ge1 :: [a] -> [[a]]
powSet_ge1 = tail . powSet
\end{code}

\noindent \highlighttt{powSet'}~$\mathit{xs}$ returns the list of sub-lists of
$\mathit{xs}$. This version \emph{does} return them in an
order that monotonically increases in length.

\begin{code}
powSet' :: [a] -> [[a]]
powSet' xs = [] : powSet_ge1' xs
\end{code}

\noindent \highlighttt{powSet\_ge1'}~$\mathit{xs}$ returns the list of sub-lists of
$\mathit{xs}$ with at least 1 element. This version \emph{does}
return them in an order that monotonically increases
in length.

\begin{code}
powSet_ge1' :: [a] -> [[a]]
powSet_ge1' xs
   = concat [combinations k xs | k <- [1..length xs]]
\end{code}

\noindent \highlighttt{properSublists}~$\mathit{xs}$ returns the list of
strict sub-lists of $\mathit{xs}$ with at least 1 element, in an
order that monotonically increases in length.

\begin{code}
properSublists :: [a] -> [[a]]
properSublists xs = case xs of
   [] -> []
   _  -> [] : concat [combinations k xs 
                     | k <- [1..length xs - 1]]
\end{code}

\submodule{Bag-like operations} %%%%%%%%%%%%%%%%%%%%%%%%%

\noindent $\mathit{xs}$~\highlightttInfix{subBag}~$\mathit{ys}$ returns {\tt True} iff every
element of $\mathit{xs}$ occurs at least as frequently in
$\mathit{ys}$ as it does in $\mathit{xs}$.

\begin{code}
subBag :: Eq a => [a] -> [a] -> Bool
subBag [] _       = True
subBag (x:xs) ys  = case del x ys of
                       (False, _)  -> False
                       (True, ys') -> subBag xs ys'
   where
   del _ [] = (False,[])
   del x' (y:ys')
        | x' == y    = (True, ys')
        | otherwise  = case del x' ys' of
           (False, _) -> (False, [])
           (_, ys'')   -> (True, y : ys'')
\end{code}

\noindent \highlighttt{bagElem}~$\mathit{xs}~\mathit{xss}$ returns {\tt True} iff $\mathit{xs}$ or 
some permutation of $\mathit{xs}$ is an element of $\mathit{xss}$.

\begin{code}
bagElem :: Eq a => [a] -> [[a]] -> Bool
bagElem _ []
   = False
bagElem xs (xs':xss)
   | null (xs L.\\ xs') && null (xs' L.\\ xs) = True
   | otherwise                                = bagElem xs xss
\end{code}

\submodule{Set-like operations} %%%%%%%%%%%%%%%%%%%%%%%%%

\noindent \highlighttt{allUnique}~$\mathit{xs}$ returns {\tt True} iff all elements of
$\mathit{xs}$ are unique.

\begin{code}
allUnique :: Eq a => [a] -> Bool
allUnique [] = True
allUnique (x:xs)
   | x `elem` xs = False
   | otherwise   = allUnique xs
\end{code}

\noindent \highlighttt{duplicates}~$\mathit{xs}$ returns all of the elements of $\mathit{xs}$ that
are duplicated.

\begin{code}
duplicates :: Eq a => [a] -> [a]
duplicates [] = []
duplicates (x:xs)
   | x `elem` xs = x : duplicates xs
   | otherwise   = duplicates xs
\end{code}

\noindent \highlighttt{snub}~$\mathit{xs}$ returns the unique elements of $\mathit{xs}$ in
non-descending order, and does it in $O(N \log N)$ time. 

\begin{code}
snub :: Ord a => [a] -> [a]
snub xs
   = case xs of
        (_:_:_) -> mnub (snub ys) (snub zs)
        _       -> xs
     where
     (ys, zs) = split xs
\end{code}

\noindent $X$~\highlighttt{+:}~$x$ returns $X \cup \{x\}$. 

\begin{code}
infixl 5 +:
(+:) :: Ord a => [a] -> a -> [a] 
xs +: x = snub (x : xs)
\end{code}

\noindent \highlighttt{mnub}~$\mathit{xs}~\mathit{ys}$ merges lists $\mathit{xs}$ and
$\mathit{ys}$ which must be in strictly ascending order. Any elements
that occur in $\mathit{xs}$ and $\mathit{ys}$ occur only once in the result.

\begin{code}
mnub :: Ord a => [a] -> [a] -> [a]
mnub xs ys = case xs of
   []      -> ys
   x : xs' -> case ys of
      []                  -> xs
      y : ys' | x < y     -> x : mnub xs' ys
              | x == y    -> x : mnub xs' ys'
              | otherwise -> y : mnub xs ys'
\end{code}

\noindent \highlighttt{isSubset}~$\mathit{xs}~\mathit{ys}$ returns {\tt True} iff
$\mathit{xs} \subseteq \mathit{ys}$. Precondition:
$\mathit{xs}$ and $\mathit{ys}$ are in strictly ascending order.

\begin{code}
isSubset :: (Ord a) => [a] -> [a] -> Bool
isSubset []     _                  = True
isSubset _      []                 = False
isSubset (x:xs) (y:ys) | x == y    = isSubset xs ys
                       | x <  y    = False
                       | otherwise = isSubset (x:xs) ys
\end{code}

\noindent \highlighttt{isProperSubset}~$\mathit{xs}~\mathit{ys}$ returns {\tt True} iff
$\mathit{xs} \subset \mathit{ys}$. Precondition:
$\mathit{xs}$ and $\mathit{ys}$ are in strictly ascending order.

\begin{code}
isProperSubset :: (Ord a) => [a] -> [a] -> Bool
isProperSubset xs ys = xs /= ys && xs `isSubset` ys
\end{code}

\noindent \highlighttt{odiff}~$\mathit{xs}~\mathit{ys}$
returns the set difference $\mathit{xs} - \mathit{ys}$.
Precondition: $\mathit{xs}$ and $\mathit{ys}$ are in
strictly ascending order.

\begin{code}
odiff :: (Eq a, Ord a) => [a] -> [a] -> [a]
odiff xs ys = case xs of
   []    -> []
   x:xs' -> case ys of
      []                -> x : xs'
      y:ys' | x < y     -> x : odiff xs (y:ys')
            | x == y    -> odiff xs' ys'
            | otherwise -> odiff (x:xs') ys'
\end{code}

\noindent \highlighttt{osect}~$\mathit{xs}~\mathit{ys}$
returns the set intersection $\mathit{xs} \cap \mathit{ys}$.
Precondition: $\mathit{xs}$ and $\mathit{ys}$ are in
strictly ascending order.

\begin{code}
osect :: (Eq a, Ord a) => [a] -> [a] -> [a]
osect xs ys = case xs of
   []    -> []
   x:xs' -> case ys of
      []                -> []
      y:ys' | x < y     -> osect xs' (y:ys')
            | x == y    -> x : osect xs' ys'
            | otherwise -> osect (x:xs') ys'
\end{code}

\noindent \highlighttt{ounion}~$\mathit{xs}~\mathit{ys}$
returns the set union $\mathit{xs} \cup \mathit{ys}$.
Precondition: $\mathit{xs}$ and $\mathit{ys}$ are in
strictly ascending order.

\begin{code}
ounion :: (Eq a, Ord a) => [a] -> [a] -> [a]
ounion xs ys = case xs of
   []   -> ys
   x:xs' -> case ys of
      []                -> x : xs'
      y:ys' | x < y     -> x : ounion xs' (y:ys')
            | x == y    -> x : ounion xs' ys'
            | otherwise -> y : ounion (x:xs') ys'
\end{code}

\noindent
\highlighttt{findSubset}~$\mathit{xs}~\mathit{ys}$ returns
{\tt Just}~$\mathit{xs}$ if $\mathit{xs} \subseteq
\mathit{ys}$ or {\tt Just}~$\mathit{ys}$ if $\mathit{ys}
\subseteq \mathit{xs}$, otherwise {\tt Nothing}.
Precondition: $\mathit{xs}$ and $\mathit{ys}$ are in
strictly ascending order.

\begin{code}
findSubset :: Ord a => [a] -> [a] -> Maybe [a]
findSubset lefts rights = fss lefts rights
   where 
   fss  []     []                 = Just lefts
   fss  []     _                  = Just lefts
   fss  _      []                 = Just rights
   fss  (x:xs) (y:ys) | x == y    = fss xs ys
                      | x < y     = fssR xs (y:ys)
                      | otherwise = fssL (x:xs) ys
   fssR _      []                 = Just rights
   fssR []     _                  = Nothing
   fssR (x:xs) (y:ys) | x == y    = fssR xs ys
                      | x < y     = fssR xs (y:ys)
                      | otherwise = Nothing
   fssL []     _                  = Just lefts
   fssL _      []                 = Nothing
   fssL (x:xs) (y:ys) | x == y    = fssL xs ys
                      | y < x     = fssL (x:xs) ys
                      | otherwise = Nothing
\end{code}

\noindent \highlighttt{noSuperSets}~$\mathit{xss}$ reduces $\mathit{xss}$ by removing
all elements of $\mathit{xss}$ that are supersets of any
other elements of $\mathit{xss}$. Preconditions: All
elements of $\mathit{xss}$ are in strictly ascending order; and
all elements of $\mathit{xss}$ are unique.

\begin{code}
noSuperSets :: Ord a => [[a]] -> [[a]]
noSuperSets xss = [xs | xs <- xss, 
   not (any (`isProperSubset` xs) xss)]
\end{code}

\noindent \highlighttt{disjoint}~$A~B$ returns {\tt True} iff $A
\cap B = \{\}$.

\begin{code}
disjoint :: Eq a => [a] -> [a] -> Bool
disjoint as bs = and [a /= b | a <- as, b <- bs]
\end{code}

\noindent \highlighttt{meet}~$A~B$ returns {\tt True}
iff $A \cap B \ne \{\}$.

\begin{code}
meet :: Eq a => [a] -> [a] -> Bool
meet as bs = or [a == b | a <- as, b <- bs]
\end{code}

\noindent \highlighttt{pPlus}~$L~H$ returns
$\mathcal{P}^{+}(L,H) = \{K
: K \subseteq L$ and $K \ne \{\}$ and $H \cap K =
\{\}\}$.

\begin{code}
pPlus :: Eq a => [a] -> [a] -> [[a]]
pPlus l h = filter (disjoint h) (powSet_ge1' l)
\end{code}


\submodule{Subsequence operations} %%%%%%%%%%%%%%%%%%%%%%%%%%%%

\noindent \highlighttt{isSubSequence}~$\mathit{ps}~\mathit{cs}$ returns {\tt True} iff
$\mathit{ps}$ is a sequence that occurs in $\mathit{cs}$.
This implementation uses only a brute force
algorithm, $O(m \times n)$, for $m = $ {\tt length}~$\mathit{ps}$
and $n = $ {\tt length}~$\mathit{cs}$.

\begin{code}
isSubSequence :: Eq a => [a] -> [a] -> Bool
isSubSequence ps cs = case cs of
   []    -> False
   _:cs' -> L.isPrefixOf ps cs || isSubSequence ps cs'
\end{code}

\noindent \highlighttt{notSubSequence}~$\mathit{ps}~\mathit{cs}$ returns {\tt True} iff
$\mathit{ps}$ is not a sequence that occurs in $\mathit{cs}$.

\begin{code}
notSubSequence :: Eq a => [a] -> [a] -> Bool
notSubSequence p w = not $ isSubSequence p w
\end{code}

\noindent \highlighttt{chop}~$x~\mathit{xs}$ returns the 
sublists in $\mathit{xs}$ that are separated by elements
equal to $x$.

\begin{code}
chop :: Eq a => a -> [a] -> [[a]]
chop x xs = case xs of
   []  -> []
   xs' -> let (xs'',xs''') = break (== x) xs'
          in xs'' : case xs''' of
             []         -> []
             _ : xs'''' -> chop x xs''''
\end{code}

\noindent \highlighttt{chops}~$\mathit{bs}~\mathit{xs}$ returns the 
sublists in $\mathit{xs}$ that are separated by sequences
equal to $\mathit{bs}$.

\begin{code}
chops :: Eq a => [a] -> [a] -> [[a]]
chops bs xs
   | null bs   = error "chops null"
   | otherwise = chp xs
   where
   n = length bs
   chp xs'
      | null xs'            = []
      | bs `L.isPrefixOf` xs' = chp (drop n xs')
      | otherwise           = 
         let (ys,zs) = brk xs'
         in ys : chp zs
   brk xs'
      | bs `L.isPrefixOf` xs' = ([], drop n xs')
      | otherwise            = case xs' of
         [] -> ([],[])
         x : xs'' -> 
            let (ys,zs) = brk xs''
            in (x : ys, zs)
\end{code}

\noindent \highlighttt{subsSuffix}~$\mathit{sep}~\mathit{suf}~\mathit{xs}$
replaces anything after the rightmost occurrence of $\mathit{sep}$
in $\mathit{xs}$ with $\mathit{suf}$. If $\mathit{suf}$ does not
occur in $\mathit{xs}$, then $\mathit{sep}$ and $\mathit{suf}$
are appended. Use this to create otput file names from input
file names.

\begin{code}
subsSuffix :: Eq a => a -> [a] -> [a] -> [a]
subsSuffix sep suf xs =
   let rs = reverse xs
       (ys, zs) = break (== sep) rs
       rs' = if null zs then
             reverse suf ++ sep : ys
          else
             reverse suf ++ zs
   in reverse rs'
\end{code}

\noindent \highlighttt{trimN}~$n~\mathit{xs}$
drops $n$ elements from both ends of a list.

\begin{code}
trimN :: Int -> [a] -> [a]
trimN n = reverse . drop n . reverse . drop n
\end{code}

\noindent \highlighttt{trim2}~$\mathit{xs}$
drops $2$ elements from both ends of a list.
Useful for comments and the like.

\begin{code}
trim2 :: [a] -> [a]
trim2 = trimN 2
\end{code}

\noindent \highlighttt{replace1}~$\mathit{as}~\mathit{bs}~\mathit{xs}$
replaces the first occurence of $\mathit{as}$ in $\mathit{xs}$ with 
$\mathit{bs}$.

\begin{code}
replace1 :: Eq a => [a] -> [a] -> [a] -> [a]
replace1 as bs xs = case xs of
   [] -> []
   x : xs' | as `L.isPrefixOf` xs -> 
              bs ++ drop (length as) xs
           | otherwise ->
              x : replace1 as bs xs'
\end{code}

\noindent \highlighttt{replaceAll}~$\mathit{as}~\mathit{bs}~\mathit{xs}$
replaces all occurences of $\mathit{as}$ in $\mathit{xs}$ with 
$\mathit{bs}$.

\begin{code}
replaceAll :: Eq a => [a] -> [a] -> [a] -> [a]
replaceAll as bs xs = case xs of
   [] -> []
   x : xs' | as `L.isPrefixOf` xs -> 
              bs ++ replaceAll as bs (drop (length as) xs)
           | otherwise ->
              x : replaceAll as bs xs'
\end{code}

