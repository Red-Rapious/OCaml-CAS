# OCaml-CAS
A basic Computer Algebra System (CAS) made in OCaml

## Features
- Various expressions
- Basic function: exponential, natural logarithm, sinus, cosinus, tangent, arctan, ...
- Expression validity checker
- Print expressions (soon in LaTeX format)
- Simplification function
- Derivative function supporting every expression implemented in the CAS

## Installation
Enable `opam` commands in command line with:
```
eval $(opam config env) 
```
(you can also add it in the PATH permanently)

Once in the `OCaml_CAS` folder, see if the program compiles with:
```
dune build
```

Run it with:
```
dune exec ./bin/main.exe
```

## Files
- `bin/expression.ml`: basic expression type and checker
- `bin/calculus.ml`: derivation and simplification
- `bin/main.ml`: tests
- `bin/output.ml`: function to print expressions

## License
This work is licensed under the [CC-BY-NC-SA 4.0](https://creativecommons.org/licenses/by-nc-sa/4.0/) license.