module Main where

import qualified Data.Map as M
import qualified Closures
import Data.List
import Backend
import AST

import Parser

exampleDecls :: Decls
exampleDecls =
  Closures.compileDecls $ M.fromList
    [ (foo_name, (foo_args, foo_body))
    , (bar_name, (bar_args, bar_body))
    , (fibo_name, (fibo_args, fibo_body))
    , (wrapper_name, (wrapper_args, wrapper_body))
    , (caller_name, (caller_args, caller_body))]
  where
    foo_name = "foo"
    foo_args = ["a"]
    foo_body = Apply (Fun "bar" 2) [Const 8, Var "a"]

    bar_name = "bar"
    bar_args = ["a", "b"]
    bar_body = Ite (Var "a") (Const 3) (Var "b")

    fibo_name = "fibo"
    fibo_args = ["x"]
    fibo_body =
      Ite (Var "x")
        (Ite (call "int_sub" [Var "x", Const 1])
          (call "int_add"
            [ call "fibo_caller" [call "int_sub" [Var "x", Const 1]]
            , call "fibo_caller" [call "int_sub" [Var "x", Const 2]]])
          (Const 1))
        (Const 0)

    wrapper_name = "fibo_wrapper"
    wrapper_args = []
    wrapper_body = Fun "fibo" 1

    caller_name = "fibo_caller"
    caller_args = ["x"]
    caller_body = Apply (Fun "fibo_wrapper" 0) [Var "x"]

    -- call a function with all it's arguments
    call name args = Apply (Fun name (length args)) args

main_call :: String
main_call =
  "#include <stdio.h>\n" ++
  "#include <time.h>\n" ++
  "word_t allocation_buffer[100000000];" ++
  "word_t test_fibo(word_t x) { if (x < 2) return x; return test_fibo(x-1)+test_fibo(x-2); }\n" ++
  "int main() {\n" ++
  "  register word_t* base_sp asm(\"sp\");" ++
  "  alloc_init(base_sp, allocation_buffer, 100000);\n" ++
  "  word_t buf;\n" ++
  "  buf = int_to_word(32);\n" ++
  "  clock_t t0 = clock();\n" ++
  "  printf(\"fibo(32): %ld\\n\", test_fibo(32));" ++
  "  clock_t t1 = clock();\n" ++
  "  printf(\"fibo(32): %ld\\n\", word_to_int(fn_fibo_caller(&buf)));\n" ++
  "  clock_t t2 = clock();\n" ++
  "  double diff0 = (double)(t1-t0) / CLOCKS_PER_SEC;\n" ++
  "  double diff1 = (double)(t2-t1) / CLOCKS_PER_SEC;\n" ++
  "  printf(\"baseline: %f compiled: %f\\n\", diff0, diff1);\n" ++
  "  unsigned gc_calls;\n" ++
  "  double gc_time;\n" ++
  "  alloc_stats(&gc_calls, &gc_time);\n" ++
  "  printf(\"gc calls: %d gc time: %f\\n\", gc_calls, gc_time);\n" ++
  "}"

main :: IO ()
main = do
  content <- Closures.compileDecls <$> parseFile "test.lambda"
  print content

  let (strings, _, _) = runCGen (compileDecls content) [] 0

  let file = concat (intersperse "\n" (reverse strings))
  let file_with_header = "#include \"object.h\"\n\n" ++ file ++ main_call

  writeFile "runtime/program.c" file_with_header
