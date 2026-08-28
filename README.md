# Sloth

A small interpreted programming language written in Haskell.

I wrote Sloth in 2021 as an experiment in developing my own interpreted language using monadic parser combinators in Haskell. It imitates mainstream imperative languages with C-style syntax.

The project was primarily a learning exercise. The name "Sloth" reflects the interpreter's intentionally modest focus on performance.

## Features

- Dynamically typed values
- Variables and assignment
- Arithmetic, comparison and logical operators
- `if`/`elif`/`else` conditionals
-  `while` loops
- Functions and lambda expressions
- Higher-order functions
- Heterogeneous lists
- List indexing and mutation
- Built-in input/output and utility functions

## Examples

The `examples/` directory contains example Sloth programs, including:

- `game_of_life.sloth` — Conway's Game of Life
- `hangman.sloth` — interactive Hangman
- `lambdas.sloth` — lambdas and higher-order functions
- `lists.sloth` — list construction and mutation
- `words.sloth` — string processing
- `ProjectEuler/` — solutions to Project Euler problems

```text
// test.sloth

func map(f, xs) {
    var result = [];
    var i = 0;
    while (i < 5) {
        result = result + [f(xs[i])];
        i += 1;
    }
    return result;
}

func square(x) {
    return x * x;
}

var numbers = [1, 2, 3, 4, 5];

var squares = map(square, numbers);
var doubled = map(|x => x * 2|, numbers);

println(squares);
println(doubled);

squares[0] = "potato";

println(squares);
```

Running the program:
```
$ cabal run sloth test.sloth
[1,4,9,16,25]
[2,4,6,8,10]
[potato,4,9,16,25]
```

## Implementation

### Parsing

The source is parsed with [Megaparsec](https://github.com/mrkkrp/megaparsec) using monadic parser combinators. The parser handles literals, identifiers, comments, function definitions, conditionals, loops, assignments, function calls, indexing and lambda expressions, and constructs an abstract syntax tree.

Expressions are parsed with `makeExprParser`, with explicit precedence for arithmetic, comparison and logical operators. Function calls and indexing can be chained, allowing expressions such as `f(x)[i]`.

### Evaluation

Sloth evaluates the abstract syntax tree directly rather than compiling it to another representation. Evaluation runs in `StateT Store IO`, with a stack of local variable environments and a global environment. Environments are implemented using mutable hash tables.

Function calls evaluate their arguments and execute the function body in a new local environment containing the function parameters.

### First-class functions

Functions and lambdas are represented as values, allowing them to be assigned to variables, passed as arguments and returned from functions. This supports higher-order functions such as the `map` example above.

### Lists

Sloth lists are represented as `ListL (Vector Literal)`. The custom `Vector` type wraps `Data.Vector.Mutable.IOVector` and provides dynamically resizable mutable storage.

The language supports heterogeneous lists, indexing, concatenation and in-place mutation, including nested indexing and assignment.

### Built-ins

Built-in functions provide input/output and utility operations, including `print`, `println`, `getInt`, `getFloat`, `getLine`, `getWord`, `len`, `round`, `min`, `max` and `sleep`.

## Running

With GHC and Cabal installed:

```bash
cabal run sloth <filename>


