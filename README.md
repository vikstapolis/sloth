# Sloth

## About

Sloth is a dynamically typed, multi-paradigm interpreted language. It is currently in the development phase.

## Program Structure

In order write a program, you can just start typing at the head of the page – no main method required.

This is a complete program:

> `print("hello, world")`

<br>

## Data types, Literals and Variables

The data types in `Sloth`, along with their literals are:

- Int – Arbitary precision integers
- Float – Arbitrary precision floating point numbers
- Bool – True | False
- String - A collection of characters enclosed in quotes, eg. `"hello"`
- List - A heterogeneous variable length collection, eg. `[42, "fat cat", False]` 
- Object (Discussed further in the OOP section)


To declare a variable, use the `var` keyword:

> `var foo = "hello"`

Since `Sloth` is dynamically typed, the type of a variable can change.

> `var foo = "hello"`<br>
> `foo = True`

<br>

## Operators

`Sloth` supports all basic arithmetic operators:

- `+`
- `-`
- `*`
- `/`
- `%` (modulus operator)

Relational operators:

- `==`
- `!=`
- `<=`
- `>=`

Logical operators:

- `&&`
- `||`
- `! ` (logical negation)

Assigment operators:

- `=`
- `+=`
- `-=`
- `*=`
- `/=`
- `%=`

## Control Structures

### Conditional statements

If-else ladder

> `if (num > 5) {`<br>
> &nbsp;&nbsp;&nbsp;&nbsp;`num = 0`<br>
> `} elif (num == 5) {`<br>
> &nbsp;&nbsp;&nbsp;&nbsp;`num = 1`<br>
> `} else {`<br>
> &nbsp;&nbsp;&nbsp;&nbsp;`num += 1`<br>
> `}`<br>

Ternary statements

> `num = (num > 5) ? 0 : (num == 5) ? 1 : num + 1`

### Loops

For loop

> `for (var i = 0; i < num; i += 1) {`<br>
> &nbsp;&nbsp;&nbsp;&nbsp;`print(num)`<br>
> `}`

While loop

> `while (i < num) {`<br>
> &nbsp;&nbsp;&nbsp;&nbsp;`print(num)`<br>
> `}`

For-each loop

> `var myList = [3, 22, 9, 1]`<br>
> `foreach(elem : myList) {`<br>
> &nbsp;&nbsp;&nbsp;&nbsp;`print(elem * 5)`<br>
> `}`

## Functions

Functions are declared using the `func` keyword, and can take any number of arguments.

> `func bar()`<br>
> `func bar(count, names)`

The function body is enclosed in curly brackets, and values are (optionally) returned using the keyword `return`.

> `func bar(count, names) {`<br>
> &nbsp;&nbsp;&nbsp;&nbsp;`count = count + 1`<br>
> &nbsp;&nbsp;&nbsp;&nbsp;`names = names + ["John"]`<br>
> &nbsp;&nbsp;&nbsp;&nbsp;`print("hello from a function!")`<br>
> &nbsp;&nbsp;&nbsp;&nbsp;`return [count, names]`<br>
> `}`

### Lambda Functions

`Sloth` also supports lambda functions in the following format:

> `| (param1, param2) => expression_to_be_returned |`<br>
> `| (count, names) => [count + 1, names + ["John"]] |`

Lambdas with multiple statements:

> `| (count, names) =>`<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`print("hello from a function!")`<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`return [count + 1, names + ["John"]] |`

Lambdas can also be used to partially apply functions:

> `| names => foo(5, names) |`

Functions can also be passed as arguments to other functions.

## Object Oriented Programming

TODO