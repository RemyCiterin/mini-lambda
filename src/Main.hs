module Main where

import qualified Data.Map as M
import Data.List
import Backend
import Lower
import AST

import Lexer

import Parser
import qualified Parse

test_program =
  unlines
    [ "test x = do"
    , "  let "
    , "    a = 0o17_111110"
    , "    b ="
    , "      300"
    , "  x = y"
    , "  x <- 4"
    , "where "
    , " { y=2 }"
    , "let x = do 42"
    , "  where "
    , "    y = 3"
    , ""
    , ""
    , ""
    , "" ]

-- 100000 / 100000 / 22
main_call :: String
main_call =
  "#include <stdio.h>\n" ++
  "#include <time.h>\n" ++
  "word_t allocation_buffer[100000000];\n" ++
  "word_t test_fibo(word_t x) { if (x < 2) return x; return test_fibo(x-1)+test_fibo(x-2); }\n" ++
  "int main() {\n" ++
  "  register word_t* base_sp asm(\"sp\");\n" ++
  "  alloc_init(base_sp, allocation_buffer, 1000000);\n" ++
  "  word_t buf;\n" ++
  "  buf = int_to_word(32);\n" ++
  "  clock_t t0 = clock();\n" ++
  "  printf(\"fibo(32): %ld\\n\", test_fibo(32));" ++
  "  clock_t t1 = clock();\n" ++
  "  printf(\"fibo(32): %ld\\n\", word_to_int(fn_fibo(&buf)));\n" ++
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
  print (lexer Parse.testString)
  print Parse.testParsed
  --print $ (insertBraces $ lexerNoLayout (SLoc 1 1) test_program)
  --print $ lexer test_program
  content <- lowerDecls <$> parseFile "test.lambda"
  --print content

  let (strings, _, _) = runCGen (compileDecls content) [] 0

  let file = concat (intersperse "\n" (reverse strings))
  let file_with_header = "#include \"object.h\"\n\n" ++ file ++ main_call

  writeFile "runtime/program.c" file_with_header
