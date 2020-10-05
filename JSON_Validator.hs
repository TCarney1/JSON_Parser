{------------------------------------------------
 takes 2 arguments. 1. a json file,
 2. a schema file that decribes the format of
 a desired json file. The program outputs true if
 the json file matches the specified schema file.
-------------------------------------------------}


module Main (main) where

import System.IO
import System.Environment
import qualified Data.Map as M
import Data.Char

import ABR.Util.Pos
import ABR.Parser
import ABR.Parser.Lexers


---------------- Types & Data ----------------
data Value = Val Value
           | Num JNumbers
           | Str String
           | Obj [(Value, Value)] -- This should be changed to [(String, Value)]
           | Bol Bool
           | Arr [Value]
      deriving (Show, Read, Eq)


data JNumbers = JDouble Double | JInteger Integer
 deriving (Show, Read, Eq)


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


-- same as ABR fixedL, but the decimal place is not optional
myFixedL :: Lexer
myFixedL =
   cardinalL
   <&&> literalL '.' <&&> soft (optional cardinalL)
   %> "fixed"


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
       <|> literalL ':'
       <|> literalL '['
       <|> literalL ']'


mySignedFixedL :: Lexer
mySignedFixedL =
   soft (optional (literalL '-')) <&&> myFixedL
   %> "signedFixed"

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
numberL =
       myFixedL  %> "float" <|>
       cardinalL %> "integer" <|>
       floatL %> "float" <|>
       mySignedFixedL %> "float" <|>
       signedCardinalL %> "integer" <|>
       signedFloatL %> "float"


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
      <|> tagP "float" @> (\(_,n,_) -> Num $ JDouble $ read n)


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
   (p1: p2:_) <- getArgs
   interpret p1 p2

{-
run :: IO ()
run = interpret "data/myArray.json" "schema/intArray-schema.json"-}


interpret :: FilePath -> FilePath -> IO ()
interpret p1 p2 = do
   source <- readFile p1
   putStrLn "----- source code ------"
   putStr source
   schema <- readFile p2
   putStrLn "----- schema code ------"
   putStr schema
   let cps = preLex source
   let cps' = preLex schema
   putStrLn "----- Pairs ------"
   print cps
   case jsonL cps' of
      Error pos msg -> putStr $ errMsg pos msg source
      OK (tlps',_) -> do
         putStrLn "----- Schema Lexical Level ------"
         print tlps'
         case jsonP tlps' of
            Error pos msg -> putStr $ errMsg pos msg source
            OK (prog',_)   -> do
                case jsonL cps of
                  Error pos msg -> putStr $ errMsg pos msg source
                  OK (tlps,_) -> do
                     putStrLn "----- Values Lexical Level ------"
                     print tlps
                     case jsonP tlps of
                        Error pos msg -> putStr $ errMsg pos msg source
                        OK (prog,_)   -> do
                          putStrLn "----- Schema ------"
                          print $ prog'
                          putStrLn "----- Value ------"
                          print $ prog
                          print $ eval prog' prog


----------------- functions ----------------

-- string to bool.
stob :: String -> Bool
stob s
    | s == "true"  = True
    | s == "false" = False


-- checks schema
eval :: [Value] -> [Value] -> Bool
eval (s:ss) (v:vs) = case s of
  Val(Val(Obj [])) -> True -- empty schema is always true
  Val(Val(Obj((Str _, Val(Str s')): _))) ->
      case s' of
        "float" -> case v of
           Val(Num(JDouble d)) -> True
           _                   -> False
        "int" -> case v of
           Val(Num(JInteger d)) -> True
           _                    -> False
        "bool" -> case v of
           Val(Bol _)           -> True
           _                    -> False
        "string" -> case v of
           Val(Str _)           -> True
           _                    -> False
        -- array works almost completely.
        -- small bug where the first number of the array has to match the schema.
        -- e.g. int array schema will accept [1, 2.0] but reject [2.0, 1]
        "array" -> case v of
             Val(Arr val) ->
                case s of
                  Val(Val(Obj(_:(Str _,sch):_))) -> eval [sch] val --this is 'strict' array schema.
                  _ -> True -- this is 'lazy' array schema.
             _  -> False
        _ -> False
  _ -> False


