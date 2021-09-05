-------------------------------------------------------------
--------------------Programming Task 6-----------------------
-------------------------------------------------------------
--  Name:  RYAN MAY                                        --
--  Email: rmay03@syr.edu                                  --
-------------------------------------------------------------
import PropLogicParser
import Data.List

-------------------------------------------------------------
-- PROBLEM #1
-- Purpose:
--    varLookup pv ta
--       This function takes a Variable and a TruthAssign (list of Variables) and determines if the variable
--       pv is in the TruthAssign list ta.  The function returns true if pv is in ta, else returns false.

--Definition:
varLookup :: Variable -> TruthAssign -> Bool
varLookup pv [] = False
varLookup pv (t:ta) = (t==pv) || varLookup pv ta


-- Tests:
--   Tests include the empty TruthAssign list, 4 tests where the TruthAssign remains the same but different
--        variables are tested against the TruthAssign list.  Tests also contain a test with longer string than a char.

t1a = varLookup "Y" []                               -- Should return False (TruthAssign is empty)
t1b = varLookup "Y" ["A","B","C"]                    -- Should return False (No matching variable in TruthAssign)
t1c = varLookup "C" ["A","B","C"]                    -- Should return True (Same TruthAssign as t1b but Variable matches)
t1d = varLookup "A" ["A","B","C"]                    -- Should return True (Same TruthAssign as t1c different Variable matches)
t1e = varLookup "a" ["A","B","C"]                    -- Should return False (Variables not the same case)
t1f = varLookup  "hello" ["hello","world","!"]       -- Should return True (TruthAssign with longer strings)


-------------------------------------------------------------
-- PROBLEM #2
-- Purpose:
--    countImps form
--       This function takes a Form type and returns how many implication logical operators
--          are in form(where form represents a logical formula)

--Definition:
countImps :: Form -> Integer
countImps (PVar var) = 0
countImps (Imp f1 f2) = 1 + countImps f1 + countImps f2
countImps (Neg f1) = countImps f1
countImps (Disj f1 f2) = countImps f1 + countImps f2
countImps (Conj f1 f2) = countImps f1 + countImps f2


-- Tests:
--   Tests include the Logical forumulas with just 1 implication, multiple implications, and no implications. All logical operators
--      are used throughout the tests to ensure proper functionality.

t2a = countImps (parseForm "(A => B)")                     -- Should return 1 (Single statement with 1 implication)
t2b = countImps (parseForm "(~(B v ~C) & (A & B))")        -- Should return 0 (TruthAssign is empty)
t2c = countImps (parseForm "(~(B => ~A) => (Z => A))")     -- Should return 3 (TruthAssign is empty)


-------------------------------------------------------------
-- PROBLEM #3
-- Purpose:
--    variables form
--       This function takes a logical formula and returns a list of type Variable, that contains
--          all the propositional variables in logical formula passed in(form).

--Definition:
variables :: Form -> [Variable]
variables (PVar var) = [var]
variables (Imp f1 f2) = union (variables f1) (variables f2)
variables (Neg f1) = (variables f1)
variables (Disj f1 f2) = union (variables f1) (variables f2)
variables (Conj f1 f2) = union (variables f1) (variables f2)

-- Tests:
--   Tests include the Logical forumulas with just 1 variable, multiple variables, and repeated variables.

t3a = variables (parseForm "A")                            -- Should return ["A"] (Single variable)
t3b = variables (parseForm "(~(B v ~C) & (A & D))")        -- Should return ["B","C","A","D"] (Multiple variables)
t3c = variables (parseForm "(~(B => ~A) => (Z => A))")     -- Should return ["B","A","Z"] (Multiple variables, w/ variable occuring more than once)

-------------------------------------------------------------
-- PROBLEM #4
-- Purpose:
--    variables form
--       This function takes a logical formula and a list of variables that have the value true (all other vars
--          assumed to be false), and determines/returns the truth value of the logical formula. (ie. evaluates
--          the formula using the rules of logical operators, where the truth values of each variable is given
--          in a list)

--Definition:
meaning :: Form -> TruthAssign -> Bool
meaning (PVar v) ta = (varLookup v ta)
meaning (Neg e) ta = not (meaning e ta)
meaning (Disj e1 e2) ta = (meaning e1 ta) || (meaning e2 ta)
meaning (Conj e1 e2) ta  = (meaning e1 ta) && (meaning e2 ta)
meaning (Imp e1 e2) ta = (not (meaning e1 ta)) || (meaning e2 ta)

-- Tests:
--   Tests include the Logical forumulas with just 1 variable, multiple variables, and repeated variables.

t4a = meaning (parseForm "A") ["A"]                             -- Should return True (Single variable statement, where var is in TruthAssign)
t4b = meaning (parseForm "A") ["B"]                             -- Should return False (Single variable statement, where var is not TruthAssign)
t4c = meaning (parseForm "~A") ["A"]                            -- Should return False (Same TruthAssign as t4a, use of not operator)
t4d = meaning (parseForm "(~B v ~C)") ["B"]                     -- Should return True (Use of disjunction)
t4e = meaning (parseForm "(B & C)") ["A","B"]                   -- Should return False (Use of conjunction)
t4f = meaning (parseForm "(B => C)") ["A","B"]                  -- Should return False (Use of implication)
t4g = meaning (parseForm "((B => C) v (A & ~C))") ["A","B"]     -- Should return True (Use of all logical operators)
t4h = meaning (parseForm "((B => C) v (A & ~C))") ["B"]         -- Should return False (same form as t4g, but now only B is true)

