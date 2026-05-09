This project is a small experimentation on writing a compiler with the goal of runing the resulting
code on my own processor.
As a consequence the generated code must be relatively simple and have few dependencies in the
standard library.


The compiler is strongly inspired by [Actora](https://github.com/blarney-lang/actora) (I partially copied the parser and IR from this project), an erlang compiler runing on a specialised CPU.
And the runtime is mostly inspired by [Lean4](https://github.com/leanprover/lean4).
