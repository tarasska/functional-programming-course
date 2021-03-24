{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}

module Task45
  ( -- * Types
    HalyavaScript(..)
  , HalyavaScriptEx(..)
    -- * Functions
  , runHS
  , transToJS
  ) where

import Control.Monad.ST (ST, runST)
import Control.Monad.State (State, evalState, get, gets, put, runState)
import Control.Monad.Trans.Writer.Strict
import Data.List (intercalate)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)


-- | Class for restricting the types allowed by the implementation of 'HalyavaScript'.
class (Eq a, Show a) => HSType a

instance HSType Int
instance HSType Double
instance HSType String
instance HSType Bool

infixr 4 @=, @@=, @>, @@>, @>=, @@>=, @==, @@==
infixr 3 @+@, @+, @*@, @*, @-@, @-
infixr 1 #

-- | The type stores the result as a string (JS-code)
-- and counters for the depth and number of variables.
type JSTranslator = WriterT String (State JSState)

-- | State is the number of declared variables and the nesting depth.
data JSState = JSState
  { varCnt :: Int
  , depth  :: Int
  }

-- | Variable type for translation, the main purpose of which is to store the name.
data JSRef a = JSRef { varName :: String, varVal :: a }

-- | The family of types describing a variable in 'HalyavaScript'.
type family Var (v :: * -> *) where
  Var (ST s) = STRef s
  Var JSTranslator = JSRef

-- | The class describing the functions and operations allowed by our dsl.
-- Their functionality is obvious from the name.
class HalyavaScript (handler :: * -> *) where
  (@=)  :: HSType a => Var handler a -> a -> handler ()
  (@@=) :: HSType a => Var handler a -> handler a -> handler ()
  (#)   :: handler a -> handler b -> handler b

  (@>)   :: (HSType a, Ord a) => Var handler a -> a -> handler Bool
  (@>=)  :: (HSType a, Ord a) => Var handler a -> a -> handler Bool
  (@==)  :: (HSType a, Eq a)  => Var handler a -> a -> handler Bool
  (@@>)  :: (HSType a, Ord a) => Var handler a -> Var handler a -> handler Bool
  (@@>=) :: (HSType a, Ord a) => Var handler a -> Var handler a -> handler Bool
  (@@==) :: (HSType a, Eq a)  => Var handler a -> Var handler a -> handler Bool

  (@+@) :: (HSType a, Num a) => Var handler a -> Var handler a -> handler a
  (@+)  :: (HSType a, Num a) => Var handler a -> a -> handler a
  (@*@) :: (HSType a, Num a) => Var handler a -> Var handler a -> handler a
  (@*)  :: (HSType a, Num a) => Var handler a -> a -> handler a
  (@-@) :: (HSType a, Num a) => Var handler a -> Var handler a -> handler a
  (@-)  :: (HSType a, Num a) => Var handler a -> a -> handler a

  vLift :: HSType a => a -> handler a

  sWithVar :: HSType a => a -> (Var handler a -> handler b) -> handler b
  sFun1
    :: (HSType a, HSType b)
    => b
    -> (Var handler a -> Var handler b -> handler c)
    -> a
    -> handler b
  sFun2
    :: (HSType a, HSType b, HSType c)
    => c
    -> (Var handler a -> Var handler b -> Var handler c -> handler d)
    -> a
    -> b
    -> handler c
  sWhile :: handler Bool -> handler a -> handler ()
  sIf :: handler Bool -> handler a -> handler a -> handler a

class HalyavaScript handler => HalyavaScriptEx (handler :: * -> *) where
  eRead  :: HSType a => Var handler a -> (a -> handler b) -> handler b
  eRead2
    :: HSType a
    => Var handler a
    -> Var handler a
    -> (a -> a -> handler b)
    -> handler b

-- | Interpret bool (var - val) operator into a string in JS
interpreteBoolOp' :: HSType a => STRef s a -> a -> (a -> a -> Bool) -> ST s Bool
interpreteBoolOp' var1 val2 op = do
  val1 <- readSTRef var1
  return $ op val1 val2

-- | Interpret bool (var - var) operator into a string in JS
interpreteBoolOp :: HSType a => STRef s a -> STRef s a -> (a -> a -> Bool) -> ST s Bool
interpreteBoolOp var1 var2 op = do
  val2 <- readSTRef var2
  interpreteBoolOp' var1 val2 op

-- | Interpret arithmetic (var - val) operator into a string in JS
interpreteArithmeticOp' :: HSType a => STRef s a -> a -> (a -> a -> a) -> ST s a
interpreteArithmeticOp' var1 val2 op = do
  val1 <- readSTRef var1
  return $ op val1 val2

-- | Interpret arithmetic (var - var) operator into a string in JS
interpreteArithmeticOp :: HSType a => STRef s a -> STRef s a -> (a -> a -> a) -> ST s a
interpreteArithmeticOp var1 var2 op = do
  val2 <- readSTRef var2
  interpreteArithmeticOp' var1 val2 op

-- | Implementing 'HalyavaScript' interpretation using the 'ST' monad.
instance HalyavaScript (ST s) where
  var @= val = writeSTRef var val

  var @@= val = val >>= \vv -> writeSTRef var vv

  actionL # actionR = actionL >> actionR

  var1 @>   val2 = interpreteBoolOp' var1 val2 (>)

  var1 @>=  val2 = interpreteBoolOp' var1 val2 (>=)

  var1 @==  val2 = interpreteBoolOp' var1 val2 (==)

  var1 @@>  var2 = interpreteBoolOp  var1 var2 (>)

  var1 @@>= var2 = interpreteBoolOp  var1 var2 (>=)

  var1 @@== var2 = interpreteBoolOp  var1 var2 (==)

  var1 @+@  var2 = interpreteArithmeticOp  var1 var2 (+)

  var1 @+   val2 = interpreteArithmeticOp' var1 val2 (+)

  var1 @*@  var2 = interpreteArithmeticOp  var1 var2 (*)

  var1 @*   val2 = interpreteArithmeticOp' var1 val2 (*)

  var1 @-@  var2 = interpreteArithmeticOp  var1 var2 (-)

  var1 @- val2   = interpreteArithmeticOp' var1 val2 (-)

  vLift val = pure val

  sWithVar val action = newSTRef val >>= \ref -> action ref

  sFun1 val2 f val1 = do
    var1 <- newSTRef val1
    var2 <- newSTRef val2
    f var1 var2 >> readSTRef var2

  sFun2 val3 f val1 val2 = do
    var1 <- newSTRef val1
    var2 <- newSTRef val2
    var3 <- newSTRef val3
    f var1 var2 var3 >> readSTRef var3

  sWhile condM action = condM >>= \cond ->
    if cond
    then action >> sWhile condM action
    else return ()

  sIf condM thenAction elseAction = condM >>= \cond ->
    if cond
    then thenAction
    else elseAction

-- | These functions are difficult to translate, but useful in interpretation.
instance HalyavaScriptEx (ST s) where
  eRead var action = readSTRef var >>= \val -> action val

  eRead2 var1 var2 action = do
    val1 <- readSTRef var1
    val2 <- readSTRef var2
    action val1 val2

-- | Creates a string from twice the number of spaces.
genIndent :: Int -> String
genIndent n = replicate (2 * n) ' '

-- | Creates a template variable name by number
mkName :: Int -> String
mkName i = "var_" ++ (show i)

-- | Converts bool (var - var) operator into a string in JS
transBoolOp :: JSRef a -> JSRef a -> String -> JSTranslator Bool
transBoolOp var1 var2 op = do
  ind <- gets depth
  tell $ intercalate " " [varName var1, op, varName var2]
  return True

-- | Converts bool (var - val) operator into a string in JS
transBoolOp' :: HSType a => JSRef a -> a -> String -> JSTranslator Bool
transBoolOp' var1 val2 op = do
  ind <- gets depth
  tell $ intercalate " " [varName var1, op, show val2]
  return True

-- | Converts arithmetic (var - var) operator into a string in JS
transArithmeticOp :: JSRef a -> JSRef a -> String -> (a -> a -> a) -> JSTranslator a
transArithmeticOp var1 var2 op f = do
  ind <- gets depth
  tell $ intercalate " " [varName var1, op, varName var2]
  return $ f (varVal var1) (varVal var2)

-- | Converts arithmetic (var - val) operator into a string in JS
transArithmeticOp' :: HSType a => JSRef a -> a -> String -> (a -> a -> a) -> JSTranslator a
transArithmeticOp' var1 val2 op f = do
  ind <- gets depth
  tell $ intercalate " " [varName var1, op, show val2]
  return $ f (varVal var1) val2

-- | Implementing 'HalyavaScript' interpretation
-- using the 'JSTranslator' for translation.
instance HalyavaScript JSTranslator where
  var @= val = do
    ind <- gets depth
    tell $ genIndent ind ++ varName var ++ " = " ++show val ++ ";\n"

  var @@= val = do
    ind <- gets depth
    tell $ genIndent ind ++ varName var ++ " = "
    val
    tell $ ";\n"

  actionL # actionR = actionL >> actionR

  var1 @>   val2 = transBoolOp' var1 val2 ">"

  var1 @>=  val2 = transBoolOp' var1 val2 ">="

  var1 @==  val2 = transBoolOp' var1 val2 "=="

  var1 @@>  var2 = transBoolOp var1 var2 ">"

  var1 @@>= var2 = transBoolOp var1 var2 ">="

  var1 @@== var2 = transBoolOp var1 var2 "=="

  var1 @+@  var2 = transArithmeticOp var1 var2 "+" (+)

  var1 @+   val2 = transArithmeticOp' var1 val2 "+" (-)

  var1 @*@  var2 = transArithmeticOp var1 var2 "*" (*)

  var1 @*   val2 = transArithmeticOp' var1 val2 "*" (*)

  var1 @-@  var2 = transArithmeticOp var1 var2 "-" (-)

  var1 @-   val2 = transArithmeticOp' var1 val2 "-" (-)

  vLift val = pure val

  sWithVar val action = do
    JSState cnt ind <- get
    let name = mkName cnt
    let def = intercalate " " ["var", name, "=", show val] ++ ";\n"
    tell $ genIndent ind ++ def
    res <- action (JSRef name val)
    return res

  sFun1 val2 f val1 = do
    JSState cnt ind <- get
    let name1 = mkName cnt
    let name2 = mkName (cnt + 1)
    let def = intercalate " " ["var", name2, "=", show val2] ++ ";\n"
    tell $ genIndent ind ++ "function(" ++ name1 ++ "){\n"
    tell $ genIndent (ind + 1) ++ def
    put (JSState (cnt + 2) (ind + 1))
    f (JSRef name1 val1) (JSRef name2 val2)
    tell $ genIndent ind ++ "}\n"
    return val2

  sFun2 val3 f val1 val2 = do
    JSState cnt ind <- get
    let name1 = mkName cnt
    let name2 = mkName (cnt + 1)
    let name3 = mkName (cnt + 2)
    let def = intercalate " " ["var", name3, "=", show val3] ++ ";\n"
    tell $ genIndent ind ++ "function(" ++ name1 ++ ", " ++ name2 ++ "){\n"
    tell $ genIndent (ind + 1) ++ def
    put (JSState (cnt + 3) (ind + 1))
    f (JSRef name1 val1) (JSRef name2 val2) (JSRef name3 val3)
    tell $ genIndent ind ++ "}\n"
    return val3

  sWhile condM action = do
    st@JSState{ depth = ind } <- get
    tell $ genIndent ind ++ "while ("
    condM
    tell $ ") {\n"
    put st{ depth = ind + 1 }
    action
    tell $ genIndent ind ++ "}\n"

  sIf condM thenAction elseAction = do
    st@JSState{ depth = ind } <- get
    tell $ genIndent ind ++ "if ("
    condM
    tell $ ") {\n"
    put st{ depth = ind + 1 }
    rThen <- thenAction
    tell $ genIndent ind ++ "} else {\n"
    rElse <- elseAction
    tell $ genIndent ind ++ "}\n"
    return rElse


-- | Function for running a script written in 'HalyavaScript'.
runHS :: (forall s. ST s a) -> a
runHS script = runST script

transToJS :: JSTranslator a -> String
transToJS script = evalState (execWriterT script) (JSState 0 0)
