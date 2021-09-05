---------------------------------------------------------------------------
--   PropLogicParser.hs
--
--   Version 1.0   (2 Mar 2011)
--   written by sueo@ecs.syr.edu and royer@ecs.syr.edu
--
--   This module provides a data structure for a simple language
--   of propositional logic,  along with parsing and unparsing 
--   routines for it.
--
--      parseForm   :: String -> Form
--      unparseForm :: Form -> String
---------------------------------------------------------------------------
module PropLogicParser where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language (emptyDef)

---------------------------------------------------------------------------
--   The Data Structure  (abstract syntax)
--
--       The corresponding concrete syntax is given via comments.
---------------------------------------------------------------------------

type Variable = String

data Form = PVar Variable           --   <variable>
          | Disj Form Form          --   ( <form> v <form> )
          | Conj Form Form          --   ( <form> & <form> )
          | Imp Form Form           --   ( <form> => <form> )
          | Neg Form                --   ~ <form>
            deriving (Eq,Show)
 
--
-- Truth assignments are simply lists of variables.  A variable is 
--   considered to be true in a given truth assignment if and only if
--   it appears in the list.
--
--  For example, in the truth assignment ["X", "Y", "Z"], the 
--  propositional variables "X", "Y", "Z" are true, whereas
--  "W" (among others) is false.
--
type TruthAssign = [Variable]


---------------------------------------------------------------------------
--   Code for unparsing   (nice and simple)
---------------------------------------------------------------------------

unparseForm :: Form -> String
unparseForm (PVar v) = v
unparseForm (Neg e) = "~"++ unparseForm e
unparseForm (Disj e1 e2) =
    "("++ unparseForm e1 ++ " v " ++ unparseForm e2 ++ ")"
unparseForm (Conj e1 e2) =
    "("++ unparseForm e1 ++ " & " ++ unparseForm e2 ++ ")"
unparseForm (Imp e1 e2) = 
    "("++ unparseForm e1 ++ " => " ++ unparseForm e2 ++ ")"

---------------------------------------------------------------------------
-- Parsing routines, built on top of the Parsec parse library
--
-- CIS 252 students need not concern themselves with this code.
---------------------------------------------------------------------------

---------------------------------------------------------------------------
-- The lexer
---------------------------------------------------------------------------
lexer  = P.makeTokenParser funDef
funDef = emptyDef 
         { P.reservedOpNames = [ "&","v","~","=>"]
         }

---------------------------------------------------------------------------
-- The parser
---------------------------------------------------------------------------
-- direct imports from Parsec's token parser library
parens     = P.parens     lexer    
reservedOp = P.reservedOp lexer
variable    = P.identifier    lexer


-- arithmetic expressions
form   = varForm <|>  negForm <|> parens(opForm)

varForm  = do v <- variable
              return (PVar v)

negForm  = do reservedOp "~"
              exp <- form
              return (Neg exp)

opForm = do lft <- form
            disjForm lft <|> conjForm lft <|> impForm lft 
            
disjForm lft = do reservedOp "v"
                  rght <- form
                  return (Disj lft rght)

conjForm lft = do reservedOp "&"
                  rght <- form
                  return (Conj lft rght)

impForm lft = do reservedOp "=>"
                 rght <- form
                 return (Imp lft rght)
                   
---------------------------------------------------------------------------
-- top level functions
---------------------------------------------------------------------------

-- (aparse inp) = the parse of an arithmetic expression 
parseForm :: String -> Form
parseForm inp = case (parse form "Form" inp) of
                  Left  m   -> error (show m)
                  Right inp -> inp

