module Main (main) where

import System.IO
import System.Environment
import qualified Data.Map as M
import Data.Char

import ABR.Util.Pos
import ABR.Parser
import ABR.Parser.Lexers


---------------- Types & Data ----------------
type Object_name = String
type Var_name = String

data Json = JValue [Value]

data Value = Val Value
           | Num JNumbers
           | Str String
           | Obj [(Value, Value)] -- This should be changed to [(String, Value)]
           | Bol Bool
           | Arr [Value]
      deriving (Show, Read)


data JNumbers = JDouble Double | JInteger Integer
 deriving (Show, Read)


---------------- Variable table ----------------
--type VarTable = M.Map Var_name Values


---------------- Lexer ----------------

-- this is basically the exact same as stringL (in ABR)
-- but it does not include " chars.
myStringL :: Lexer
myStringL =
   literalL '"'
   &> (many (  tokenL "\"\""
           <|> satisfyL (/= '"') "") &%> "")
   <& literalL '"'
   %> "string"


boolL :: Lexer
boolL =
      satisfyL isAlpha ""
      <&&> (many ( satisfyL isAlpha "" ) &%> "")
      %> "bool"


symbolL :: Lexer
symbolL =  literalL '{'
       <|> literalL '}'
       <|> literalL ','
       <|> literalL '='
       <|> literalL '-'
       <|> literalL ':'
       <|> literalL '['
       <|> literalL ']'

jsonL :: Lexer
jsonL = dropWhite $ nofail $ total $ listL
   [ whitespaceL
   , numberL
   , symbolL
   , myStringL
   , boolL]


-- struggling with this.
-- just seems to make every number fixedL.
numberL :: Lexer
numberL =  fixedL  %> "double"
       <|> cardinalL %> "integer"
       <|> floatL %> "double"
       <|> signedFloatL %> "double"
       <|> signedFixedL %> "double"
       <|> signedCardinalL %> "integer"


---------------- Parsers ----------------

jsonP :: Parser [Value]
jsonP =
   nofail' "statement expected" $ total $ many valueP


valueP :: Parser Value
valueP =
   objectP @> Val <|>
   arrayP  @> Val <|>
   stringP @> Val <|>
   numberP @> Val <|>
   boolP   @> Val


stringP :: Parser Value
stringP = tagP "string" @> (\(_,n,_) -> Str n)


numberP :: Parser Value
numberP = tagP "integer" @> (\(_,n,_) -> Num $ JInteger $ read n)
      <|> tagP "double" @> (\(_,n,_) -> Num $ JDouble $ read n)


boolP :: Parser Value
boolP = tagP "bool"
        @> (\(_,n,_) -> Bol $ stob n)


objectP :: Parser Value
objectP =
      literalP "'{'" "{"
   &> memberP
  <&  literalP "'}'" "}"
   @> Val


memberP :: Parser Value
memberP =
   many (stringP
    <&> literalP "':'" ":"
     &> valueP
    <&  optional (literalP "','" ","))
     @> (\t -> Obj  t)


arrayP :: Parser Value
arrayP =
      literalP "'['" "["
   &> many(valueP
  <&  optional (literalP "','" ","))
  <&  literalP "']'" "]"
   @> Arr


---------------- driving functions ----------------

main :: IO ()
main = do
   args <- getArgs
   case args of
      [path] -> interpret path
      _      -> error "wrong number of arguments"

run :: IO ()
run = interpret "input.JSON"

interpret :: FilePath -> IO ()
interpret path = do
   source <- readFile path
   putStrLn "----- source code ------"
   putStr source
   let cps = preLex source
   putStrLn "----- Pairs ------"
   print cps
   case jsonL cps of
      Error pos msg -> putStr $ errMsg pos msg source
      OK (tlps,_) -> do
         putStrLn "----- Lexical Level ------"
         print tlps
         case jsonP tlps of
                    Error pos msg -> putStr $ errMsg pos msg source
                    OK (prog,_)   -> do
                       putStrLn "----- Parse Level ------"
                       print prog

----------------- functions ----------------
stob :: String -> Bool
stob s
    | s == "true"  = True
    | s == "false" = False




