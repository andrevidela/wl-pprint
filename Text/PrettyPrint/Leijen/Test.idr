module Text.PrettyPrint.Leijen.Test

import System

import Text.PrettyPrint.Leijen

%default total

--%access private

data SExpr = Symbol String
           | Cons SExpr SExpr
           | Nil
           | IntLit Int

consp : SExpr -> Bool
consp (Cons _ _) = True
consp _ = False

mutual
  pprint : SExpr -> Doc
  pprint (Symbol str) = text str
  pprint (IntLit i)   = text (show i)
  pprint (Cons x y)   = enclose lparen rparen (align $ listContents x y)
  pprint Nil          = lparen |+| rparen

  listContents : SExpr -> SExpr -> Doc
  listContents car Nil = pprint car
  listContents car (Symbol str) = pprint car |++| dot |++| pprint (Symbol str)
  listContents car (IntLit i) = pprint car |++| dot |++| pprint (IntLit i)
  listContents car (Cons cadr cddr) = group $ pprint car |$| listContents cadr cddr

implicit
fromList : List SExpr -> SExpr
fromList = foldr Cons Nil

implicit
fromString : String -> SExpr
fromString x = Symbol x

omega : SExpr
omega = [["lambda", ["f"], ["f", "f"]], ["lambda", ["f"], ["f", "f"]]]

covering
test : Int -> SimpleDoc
test w = renderPretty 0.8 w (pprint omega)

covering
test1 : String
test1 = displayS (test 80) ""

covering
test2 : String
test2 = displayS (test 40) ""

covering
test3 : String
test3 = displayS (test 20) ""

covering
test4 : String
test4 = displayS (test 10) ""

doTest : String -> String -> IO ()
doTest output expected =
  if output == expected
    then putStrLn "ok"
    else do putStrLn $ "Expected: " ++ expected
            putStrLn $ "Got: " ++ output
            exit 1

covering
runTest : IO ()
runTest = traverse_ (uncurry doTest)
            [ (test1, "((lambda (f) (f f)) (lambda (f) (f f)))")
            , (test2, "((lambda (f) (f f))\n (lambda (f) (f f)))")
            , (test3, "((lambda\n  (f) (f f))\n (lambda\n  (f) (f f)))")
            , (test4, "((lambda\n  (f)\n  (f f))\n (lambda\n  (f)\n  (f f)))")
            ]

