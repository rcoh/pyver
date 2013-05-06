import Test.HUnit
import Data.Map
import Analyze hiding (main) 

makeTest name code expected = 
  TestCase (assertEqual name expected (evaluateCode name code))

getAST code =
  parsePython "name" code

makeFilebackedTest filename expected = 
  TestCase $
  do
    code <- readFile filename
    assertEqual filename expected (evaluateCode filename code)

test_basic = makeFilebackedTest "basic_test.py"
  FocusedContext {left = Scope (fromList [("x",PyInt)]), right = Scope (fromList []), focus = LeftFocus}

test_undefined = makeFilebackedTest "undefined_test.py" 
  FocusedContext {left = Scope (fromList [("x",IdentifierNotFound "a")]), right = Scope (fromList []), focus = LeftFocus}

test_lists = makeFilebackedTest "test_lists.py"
    FocusedContext
      { left = Scope (
                 fromList
                   [ ( "Foo"
                     , ComplexType (
                         fromList
                           [ ( "__call__"
                             , Identity
                             )
                           , ( "x"
                             , PyList PyInt
                             )
                           , ( "y"
                             , PyInt
                             )
                           ] )
                         []
                     )
                   , ( "a"
                     , PyList (
                         UnionType
                           [ PyBool
                           , PyInt
                           ] )
                     )
                   , ( "bad_access"
                     , ObjectNotSubscriptable "5"
                     )
                   , ( "empty"
                     , PyList Unknown
                     )
                   , ( "f"
                     , ComplexType (
                         fromList
                           [ ( "x"
                             , PyList PyInt
                             )
                           , ( "y"
                             , PyInt
                             )
                           ] )
                         []
                     )
                   , ( "int_list"
                     , PyList PyInt
                     )
                   , ( "should_be_int1"
                     , PyInt
                     )
                   , ( "should_be_int2"
                     , PyInt
                     )
                   , ( "should_be_union"
                     , UnionType
                         [ PyBool
                         , PyInt
                         ]
                     )
                   , ( "should_be_unsub"
                     , ObjectNotSubscriptable "f.y"
                     )
                   ] )
      , right = Scope ( fromList [] )
      , focus = LeftFocus
      }

test_nested_lists = makeFilebackedTest "test_nested_lists.py"
    FocusedContext
      { left = Scope (
                 fromList
                   [ ( "list_int_bool"
                     , PyList (
                         UnionType
                           [ PyList PyBool
                           , PyList PyInt
                           ] )
                     )
                   , ( "list_list_int"
                     , PyList ( PyList PyInt )
                     )
                   , ( "list_mixed_int_bool"
                     , PyList (
                         UnionType
                           [ PyList (
                               UnionType
                                 [ PyBool
                                 , PyInt
                                 ] )
                           , PyList PyInt
                           ] )
                     )
                   ] )
      , right = Scope ( fromList [] )
      , focus = LeftFocus
      }


tests = TestList [
    TestLabel "test_basic" test_basic, 
    TestLabel "test_undefined" test_undefined, 
    TestLabel "test_lists" test_lists,
    TestLabel "test_nested" test_nested_lists]

main = runTestTT tests
