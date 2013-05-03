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

test1 = makeTest "Simple Assignment" "x = 5\n" 
  FocusedContext {left = Scope (fromList [("x",PyInt)]), right = Scope (fromList []), focus = LeftFocus}

test2 = makeFilebackedTest "undefined_test.py" 
  FocusedContext {left = Scope (fromList [("x",PyInt)]), right = Scope (fromList []), focus = LeftFocus}



tests = TestList [TestLabel "test1" test1 , TestLabel "test2" test2]

main = runTestTT tests
