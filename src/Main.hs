module Main where

import qualified Data.Map as M
import qualified Closures
import Data.List
import Backend
import AST

import Parser

main_call :: String
main_call =
  "#include <stdio.h>\n" ++
  "#include <time.h>\n" ++
  "word_t allocation_buffer[100000000];\n" ++
  "word_t test_fibo(word_t x) { if (x < 2) return x; return test_fibo(x-1)+test_fibo(x-2); }\n" ++
  "int main() {\n" ++
  "  register word_t* base_sp asm(\"sp\");\n" ++
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
