{-# LANGUAGE NamedFieldPuns #-}
module Analyze
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
      
  in setAttr (ident_string name) class_type context

evaluateStatement context (Fun (Ident name _)  args result_annot body _) = 
  setAttr name (Function name args body) context


evaluateStatement context _ = trace "unregognized" context 

procAssignTarget :: FocusedContext -> ExprSpan -> PyType -> FocusedContext
procAssignTarget scope lhs rhs =
  let lhs_zipper = convertToExprZipper lhs
  in zippedInsert scope lhs_zipper rhs

identForSimpleExpr :: ExprSpan -> String
identForSimpleExpr Var { var_ident } = ident_string var_ident
identForSimpleExpr Call { call_fun  } = identForSimpleExpr call_fun
identForSimpleExpr Subscript { subscriptee } = identForSimpleExpr subscriptee

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
singletonTypeOf context (Var (Ident name _) _) = getAttrWrapper name context
singletonTypeOf context (Call { call_fun } ) = 
  let 
    call_target = typeOf context call_fun
    callable = case (getAttr "__call__" call_target) of 
        Just c -> c
        Nothing -> ObjectNotCallable (render (pretty call_fun))
  in case(callable) of
    Identity -> delete "__call__" call_target
    t -> t

singletonTypeOf context List { list_exprs } = 
  let el_types = map (typeOf context) list_exprs
  in  PyList (pyTypeUnion el_types)

singletonTypeOf context Subscript { subscriptee, subscript_expr } =
  let 
      subscriptee_type = typeOf context subscriptee
      expr_type = typeOf context subscript_expr
  in 
      subscriptGet expr_type subscriptee_type
--  in case (subscriptee_type) of 
--    PyList t -> t
--    notalist -> ObjectNotSubscriptable (render (pretty subscriptee))

singletonTypeOf context expr   = trace (show expr) (Unknown)

zippedInsert :: FocusedContext -> [ExprSpan] -> PyType -> FocusedContext
zippedInsert context (last_expr:[]) rhs =
  let last_ident = identForSimpleExpr last_expr
  in setAttr last_ident rhs context

zippedInsert context (first:rest) rhs = 
  let 
    ident = identForSimpleExpr first
    ident_type = getAttrWrapper ident context 
    topscope = trace (show ident_type) (typeOf context first)
  in setAttr ident (zippedInsertPyType topscope rest rhs) context

zippedInsertPyType :: PyType -> [ExprSpan] -> PyType -> PyType
zippedInsertPyType context (last_expr:[]) rhs =
  let last_ident = identForSimpleExpr last_expr
  in setAttr last_ident rhs context

zippedInsertPyType context (first:rest) rhs =
  let 
    ident = identForSimpleExpr first
    topscope = typeOf context first
  in setAttr ident (zippedInsertPyType topscope rest rhs) context

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
    []  -> Unknown
    longer -> UnionType longer

mergeIntoUnion :: [PyType] -> PyType -> [PyType]
mergeIntoUnion soFar new 
  | new `elem` soFar = soFar
  | otherwise = new : soFar

scopeToClass :: Scope -> PyType
scopeToClass (Scope scope) = ComplexType scope []

-- Wraps getAttr to add errors for missing types
getAttrWrapper :: (ScopeLike u) => String -> u -> PyType 
getAttrWrapper ident scope = 
  case (getAttr ident scope) of 
    Just t -> t
    Nothing -> IdentifierNotFound ident

addClassMetadata :: PyType -> PyType
addClassMetadata scope =
  let existing_constructor = getAttr "__init__" scope 
  in case (existing_constructor) of 
    Nothing -> setAttr "__call__" Identity scope -- TODO: create py-function
    Just cons -> setAttr "__call__" cons scope -- TODO: bind to type

data Scope = Scope ScopeMap deriving (Show, Eq)

class ScopeLike a where
    getAttr :: String -> a -> Maybe PyType 
    setAttr :: String -> PyType -> a -> a
    delete :: String -> a -> a
    subscriptGet :: PyType -> a -> PyType
    subscriptSet :: PyType -> PyType -> a -> PyType

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

getAttrResolveIdentity ident scope =
  case (getAttr ident scope) of
    Just Identity -> delete "__call__" scope
    Just x -> x
    Nothing -> IdentifierNotFound ident


instance ScopeLike PyType where
    getAttr key (ComplexType scope _) = Map.lookup key scope
    getAttr key (UnionType types) = Just (pyTypeUnion (map (getAttrResolveIdentity key) types))
    getAttr key _ = Nothing
    -- TODO: also setAttr into refs
    setAttr key pytype (ComplexType scope refs)  = ComplexType (Map.insert key pytype scope) refs
    setAttr key pytype (UnionType types) = pyTypeUnion (map (setAttr key pytype) types)
    delete key (ComplexType scope refs)  = ComplexType (Map.delete key scope) refs

    -- TODO: validate indices
    subscriptGet index (PyList t) = t
    subscriptGet index othertype = ObjectNotSubscriptable "TODO"
    
    subscriptSet index value (PyList t) = PyList $ pyTypeUnion (value : [t])
    subscriptSet index value othertype = ObjectNotSubscriptable "TODO"

instance ScopeLike Scope where
    getAttr key (Scope scope) = Map.lookup key scope
    setAttr key pytype (Scope scope) = Scope (Map.insert key pytype scope)
    delete key (Scope scope) = Scope (Map.delete key scope)

instance ScopeLike FocusedContext where
    getAttr key (FocusedContext left right LeftFocus) = case (getAttr key left) of
      Just x -> Just x
      Nothing -> getAttr key right
    
    getAttr key (FocusedContext left right RightFocus) = case (getAttr key right) of
      Just x -> Just x
      Nothing -> getAttr key left

    setAttr key pytype (FocusedContext left right LeftFocus) = FocusedContext (setAttr key pytype left) right LeftFocus
    setAttr key pytype (FocusedContext left right RightFocus) = FocusedContext left (setAttr key pytype right) RightFocus
    
    delete key (FocusedContext left right LeftFocus) = FocusedContext (delete key left) right LeftFocus
    delete key (FocusedContext left right RightFocus) = FocusedContext left (delete key right) RightFocus
    
type StateModifier u t  = u -> t -> u

emptyComplexType = ComplexType Map.empty
emptyScope = Scope Map.empty
