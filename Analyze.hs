{-# LANGUAGE NamedFieldPuns #-}
module Analyze
-- (
--  evaluateModule,
--  evaluateCode,
--  FocusedContext,
--  LeftFocus,
--  RightFocus,
--  Scope,
-- )
where
import qualified Data.Map as Map

import Language.Python.Version2.Parser
import Language.Python.Common.AST
import Language.Python.Common.PrettyAST
import Language.Python.Common.Pretty
import System.Environment

import qualified Text.Show.Pretty as Pr

import Debug.Trace


main = do
  (filename:opts) <- getArgs
  code <- readFile filename
  putStrLn $ Pr.ppShow (runFunc filename code evaluateModule)
--  putStrLn $ runFunc filename code (id)


evaluateCode :: String -> String -> FocusedContext
evaluateCode filename code = runFunc filename code evaluateModule


evaluateModule :: ModuleSpan -> FocusedContext
evaluateModule (Module statements) = foldl evaluateStatement globalFocusedContext statements

parsePython :: String -> String -> ModuleSpan 
parsePython filename contents = case (parseModule contents filename) of
  Right (parseTree, comments) -> parseTree
  Left  err     ->   error "Parse failed"

runFunc :: (Show a) => String -> String -> (ModuleSpan -> a) -> a
runFunc filename contents func = 
    let code = parsePython filename contents in
    func code

evaluateStatement :: StateModifier FocusedContext StatementSpan
evaluateStatement context (Assign (target:[]) assign_expr _) =  --multiple assignment not supported
  let 
    assign_type = typeOf context assign_expr
  in procAssignTarget context target assign_type

evaluateStatement context (Assign lhs@(target:rest) assign_expr _) =  --Error case
  error "Unexpected parse result"

-- No support for nested classes
evaluateStatement context@(FocusedContext left right LeftFocus) (Class name args body _) =
  let (FocusedContext _ new_class_scp _) = foldl evaluateStatement (FocusedContext left emptyScope RightFocus) body
      class_without_extra = scopeToClass new_class_scp
      class_type = addClassMetadata class_without_extra
      
  in insert (ident_string name) class_type context

evaluateStatement context (Fun (Ident name _)  args result_annot body _) = 
  insert name (Function name args body) context

--evaluateStatement context (Call call_fun call_args _) = 
--  let call_target = find 
  

evaluateStatement context _ = trace "unregognized" context 

procAssignTarget :: FocusedContext -> ExprSpan -> PyType -> FocusedContext
procAssignTarget scope lhs rhs =
  let lhs_zipper = convertToZipper lhs
  in zippedInsert scope lhs_zipper rhs

typeOf :: (ScopeLike u) => u -> ExprSpan -> PyType
typeOf context expr = zippedTypeOf context zippedExpr
  where zippedExpr = convertToExprZipper expr

zippedTypeOf :: (ScopeLike u) => u -> [ExprSpan] -> PyType
zippedTypeOf context (single:[]) = singletonTypeOf context single
zippedTypeOf context (head:rest) = zippedTypeOf (singletonTypeOf context head) rest

singletonTypeOf :: (ScopeLike u) => u -> ExprSpan -> PyType
singletonTypeOf _ Int{} = PyInt
singletonTypeOf context (Var (Ident "True" _) _) = PyBool
singletonTypeOf context (Var (Ident "False" _) _) = PyBool
singletonTypeOf context (Var (Ident name _) _) = findWrapper name context
singletonTypeOf context (Call { call_fun } ) = 
  let 
    call_target = typeOf context call_fun
    callable = case (find "__call__" call_target) of 
        Just c -> c
        Nothing -> ObjectNotCallable (render (pretty call_fun))
  in case(callable) of
    Identity -> delete "__call__" call_target
    t -> t

singletonTypeOf context List { list_exprs } = 
  let el_types = map (typeOf context) list_exprs
  in  PyList (pyTypeUnion el_types)

singletonTypeOf context Subscript { subscriptee } =
  let subscriptee_type = typeOf context subscriptee
  in case (subscriptee_type) of 
    PyList t -> t
    notalist -> ObjectNotSubscriptable (render (pretty subscriptee))

singletonTypeOf context expr   = trace (show expr) (Unknown)

zippedLookup :: (ScopeLike u) => u -> [String] -> PyType 
zippedLookup u (single:[]) = findWrapper single u
zippedLookup u (ident:rest) = zippedLookup (findWrapper ident u) rest

zippedInsert :: FocusedContext -> [String] -> PyType -> FocusedContext
zippedInsert context (last_ident:[]) rhs =
  insert last_ident rhs context

zippedInsert context (ident:rest) rhs = 
  let topscope = findWrapper ident context
  in insert ident (zippedInsertPyType topscope rest rhs) context

zippedInsertPyType :: PyType -> [String] -> PyType -> PyType
zippedInsertPyType context (last_ident:[]) rhs =
  insert last_ident rhs context
zippedInsertPyType context (ident:rest) rhs =
  let topscope = findWrapper ident context
  in insert ident (zippedInsertPyType topscope rest rhs) context

convertToZipper :: ExprSpan -> [String]
convertToZipper Var { var_ident } = [ident_string var_ident]
convertToZipper (BinaryOp (Dot _) left right _) = (convertToZipper left) ++ (convertToZipper right)

convertToExprZipper :: ExprSpan -> [ExprSpan]
convertToExprZipper (BinaryOp (Dot _) left right _) = convertToExprZipper left ++ convertToExprZipper right
convertToExprZipper expr = [expr]

pyTypeUnion :: [PyType] -> PyType
pyTypeUnion types = 
  let underlying = (foldl mergeIntoUnion [] types)
  in case (underlying) of
    [x] -> x
    longer -> UnionType longer


mergeIntoUnion :: [PyType] -> PyType -> [PyType]
mergeIntoUnion soFar new 
  | new `elem` soFar = soFar
  | otherwise = new : soFar




scopeToClass :: Scope -> PyType
scopeToClass (Scope scope) = ComplexType scope []

-- Wraps find to add errors for missing types
findWrapper :: (ScopeLike u) => String -> u -> PyType 
findWrapper ident scope = 
  case (find ident scope) of 
    Just t -> t
    Nothing -> IdentifierNotFound ident
  

addClassMetadata :: PyType -> PyType
addClassMetadata scope =
  let existing_constructor = find "__init__" scope 
  in case (existing_constructor) of 
    Nothing -> insert "__call__" Identity scope -- TODO: create py-function
    Just cons -> insert "__call__" cons scope -- TODO: bind to type

data Scope = Scope ScopeMap deriving (Show, Eq)

class ScopeLike a where
    find :: String -> a -> Maybe PyType 
    insert :: String -> PyType -> a -> a
    delete :: String -> a -> a

type ScopeMap = Map.Map String PyType

type ScopeZipper = [String]

data PyType = 
    UnionType [PyType] | 
    ComplexType ScopeMap [ScopeZipper] | 
    PyInt | 
    PyBool | 
    PyList PyType | 
    PyDict PyType PyType | 
    Function { name :: String, args :: [ParameterSpan], body :: SuiteSpan } |
    Identity |
    Unknown |
    AttributeNotFound String |
    IdentifierNotFound String |
    ObjectNotSubscriptable String |
    ObjectNotCallable String deriving (Show, Eq) 


data FocusLocation = LeftFocus | RightFocus deriving (Eq, Show, Bounded)
data FocusedContext = FocusedContext { left :: Scope, right :: Scope, focus :: FocusLocation } deriving (Show, Eq)

globalFocusedContext = 
  let emptyScope = Scope Map.empty in 
    FocusedContext emptyScope emptyScope LeftFocus

instance ScopeLike PyType where
    find key (ComplexType scope _) = Map.lookup key scope
    find key _ = Nothing
    -- TODO: also insert into refs
    insert key pytype (ComplexType scope refs)  = ComplexType (Map.insert key pytype scope) refs
    delete key (ComplexType scope refs)  = ComplexType (Map.delete key scope) refs

instance ScopeLike Scope where
    find key (Scope scope) = Map.lookup key scope
    insert key pytype (Scope scope) = Scope (Map.insert key pytype scope)
    delete key (Scope scope) = Scope (Map.delete key scope)

instance ScopeLike FocusedContext where
    find key (FocusedContext left right LeftFocus) = case (find key left) of
      Just x -> Just x
      Nothing -> find key right
    
    find key (FocusedContext left right RightFocus) = case (find key right) of
      Just x -> Just x
      Nothing -> find key left

    insert key pytype (FocusedContext left right LeftFocus) = FocusedContext (insert key pytype left) right LeftFocus
    insert key pytype (FocusedContext left right RightFocus) = FocusedContext left (insert key pytype right) RightFocus
    
    delete key (FocusedContext left right LeftFocus) = FocusedContext (delete key left) right LeftFocus
    delete key (FocusedContext left right RightFocus) = FocusedContext left (delete key right) RightFocus
    
type StateModifier u t  = u -> t -> u

emptyComplexType = ComplexType Map.empty
emptyScope = Scope Map.empty
