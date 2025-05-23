
# **Lexa** - A Concatenative, Stack-Based Interpreter

Lexa was chosen for its sleek, modern feel and subtle reference to _lexing_, the process of breaking down source code into tokens. This is fundemental to the core functionality of this concatenative, stack-based interpreter. Written in Haskell, the interpreter processes commands through a stack, offering both an interactive REPL mode and batch execution mode to run programs and manipulate data efficiently.

##  Features

- Concatenative, postfix syntax (Reverse Polish Notation)
- Immutable core types (Int, Float, Bool, String, List, Quotation, Symbol)
- User-defined word bindings (via mutable symbols)
- First-class quotations (for higher-order programming and control flow)
- Interactive REPL and script execution
- Basic IO via interrupt buffer
- Lightweight and extensible Haskell codebase
- Passing all core functionality in HSpec test suite.


## **How to Build and Run Lexa**

To build and run Lexa from the terminal, follow these steps:

### 1. **Clone the repository**

First, clone the Lexa project repository to your local machine:

```bash
git clone https://github.com/wilvang/lexa-stack-interpreter.git
cd lexa-stack-interpreter
```

### 2. **Install Stack (if not already installed)**

Make sure you have **Stack** installed. If not, you can install it using the following command:

- **On macOS:**

```bash
brew install haskell-stack
```

- **On Ubuntu/Linux:**

```bash
curl -sSL https://get.haskellstack.org/ | sh
```

### 3. **Build the project**

Use Stack to build the project:

```bash
stack build
```

This will download dependencies and build the project.

### 4. **Run Lexa (REPL mode)**

To run Lexa in **REPL mode** (interactive mode), use the following command:

```bash
stack run -- -r
```

The `-- -r` syntax passes the `-r` argument to the program, which starts the REPL mode.

### 5. **Run a Program (Batch mode)**

To run a program from a file in batch mode, you can create a file with the .lexa extension and then execute it like this:

```bash
stack run -- /path/to/your_program.lexa
```

Where `your_program.lexa` is the name of the file containing the program code.


# Design Decisions and Assumptions

Lexa’s design emphasizes **clarity**, **minimalism**, and staying true to **concatenative programming** principles.

### 1. Language Choice

Lexa is implemented in **Haskell** to take advantage of:

-   **Strong static typing** for type safety and clarity.
    
-   **Pattern matching** and **algebraic data types** for clear, expressive code.
    
-   **Monads** to handle interpreter state, IO, and error management.
    
-   Simplicity in building **domain-specific languages (DSLs)**.
    

### 2. State-Based Evaluation with Global Stack

Lexa uses a global `State` record to manage execution:

-   `stack :: [Value]` — Holds the working stack of values.
    
-   `program :: [Token]` — The remaining instructions to execute.
    
-   `dictionary :: Map String Value` — Stores symbol bindings for variables and functions.
    
-   `buffer :: [Interrupt]` — Holds deferred IO operations (e.g., `InputIO`, `OutputIO`).
    

Execution is **pure** and **deterministic**: IO operations are deferred as `Interrupt`s, and execution halts when the `buffer` is non-empty. After interrupts are processed, execution resumes with updated input.

Lexa follows **postfix notation (RPN)** for simplicity and consistency, using **stack manipulation** for arithmetic, logic, function calls, and control flow.

### 3. Tokenization Assumptions

-   Input is **space-delimited** and parsed into a list of `Token`s, which can be:
    
    -   `TokVal Value` — Literal values, quotations, or symbols.
        
    -   `TokOp Op` — Built-in operations.
        
-   **Quotations** (`{ ... }`) and **lists** (`[ ... ]`) are parsed recursively. Strings require an extra space in between quotes (`" ... "`).
    
-   Programs must be **well-formed**; invalid syntax may lead to runtime errors or infinite loops.
    

### 4. Execution Modes

-   **REPL Mode**: For quick experimentation and debugging in an interactive environment.
    
-   **Batch Mode**: Executes `.lexa` script files for scripting and automation tasks.



# Core Types and Value Semantics

Lexa's `Value` type covers both **literal** and **composite** types, designed for flexibility and composability within the stack-based model.

## Numbers and Booleans

Lexa supports integers (`VInt`), floats (`VFloat`), and booleans (`VBool`) as basic literal types. Booleans are treated numerically (`True = 1`, `False = 0`), and numeric operations automatically promote to floats when necessary.
```lexa
True 2 +       # => 3
3 2.5 *        # => 7.5
5 False -      # => 5
```
## Lists and Strings

Strings (`VString`) are both literals and composite types. They behave like lists of characters and support all list operations, including concatenation, indexing, slicing, reversing, and mapping. In contrast, Lexa’s lists (`VList`) are fully heterogeneous: they can contain values of any type — integers, strings, booleans, quotations, and more.

```lexa
" Hello, " "  world! " append     # => "Hello, world!"
True [ 1 2 3 ] cons               # => [True,1,2,3]
```
## Quotations
Quotations are unevaluated blocks of code written with `{ ... }`. They are first-class values used for deferred execution, control flow, and higher-order functions. You evaluate a quotation with `exec`.
```lexa
{ 1 2 + print } exec                            # => prints 3
True if { "yes" print }  { "no" print }         # => prints "yes"
```
# Native functionallity

All operations in Lexa work on a global stack. Functions consume their inputs from the top of the stack and push their results back onto it. To describe this behavior, we use stack effect notation: `( inputs -- outputs )`, where items on the left are popped (with the rightmost being the top of the stack), and items on the right are pushed back in order.
For example, addition is written as `( x y -- sum )`, meaning it takes two values from the stack and pushes their sum.
### Stack Operations

| Feature | Description |
|--------|-------------|
| `dup`  | `( x -- x x )` duplicates the top element on the stack. |
| `swap` | `( x y -- y x )` swaps the two top elements on the stack. |
| `pop`  | `( x -- )` removes the top element from the stack. |

### Simple IO

| Feature | Description |
|--------|-------------|
| `print` | `( x -- )` prints the top element with a newline. |
| `read`  | `( -- x )` reads a line of input as a string. |

### String Parsing

| Feature        | Description |
|----------------|-------------|
| `parseInteger` | `( s -- i )` converts a string to an integer. |
| `parseFloat`   | `( s -- f )` converts a string to a float. |
| `words`        | `( s -- list )` splits a string into a list of whitespace-delimited tokens. |

### Arithmetic

| Feature | Description |
|--------|-------------|
| `+`    | `( x y -- x+y )` addition |
| `-`    | `( x y -- x−y )` subtraction |
| `*`    | `( x y -- x*y )` multiplication |
| `/`    | `( x y -- x/y )` floating point division |
| `div`  | `( x y -- int-div )` integer division |
| `<`    | `( x y -- bool )` true if x < y |
| `>`    | `( x y -- bool )` true if x > y |
| `==`   | `( x y -- bool )` true if x == y |

### Logical Operations

| Feature         | Description |
|----------------|-------------|
| `True`, `False` | Boolean literals. |
| `&&`            | `( x y -- bool )` logical AND. |
| `\|\|`            | `( x y -- bool )` logical OR. |
| `not`           | `( x -- bool )` logical NOT or numeric negation. |

### List Operations

| Feature  | Description |
|----------|-------------|
| `head`   | `( list -- item )` gets the first element. |
| `tail`   | `( list -- list )` gets the rest of the list. |
| `empty`  | `( list -- bool )` true if list is empty. |
| `length` | `( list -- len )` list length. |
| `cons`   | `( item list -- list )` prepend an item. |
| `append` | `( list1 list2 -- list3 )` concatenate lists. |
| `each`   | `( list quotation -- )` executes block on each item. |
| `map`    | `( list quotation -- newlist )` maps block over list. |
| `foldl`  | `( list acc quotation -- result )` folds list left. |

### Control Flow

| Feature | Description |
|--------|-------------|
| `if`    | `( bool -- )` executes then/else quotations based on condition. |
| `loop`  | `( -- )` repeats block until break quotation returns True. |
| `times` | `( num quotation -- )` repeats block num times. |

### Symbols and Assignment

| Feature | Description |
|--------|-------------|
| `:=`   | `( value name -- )` assigns a value to a symbol. |
| `fun`  | `( quotation name -- )` defines a named function. |

## Examples from the `stdlib`
This is the fibonacci sequence implemented in Lexa.
```lexa
fibonacci {
    dup 0 <= if
    { pop 0 }
    { dup 1 == if
      { pop 1 }
      { dup 1 - fibonacci swap 2 - fibonacci + }
    }
} fun
```

# Further development
### Tuples
- Add `VTuple` type (fixed-size, immutable).
- Syntax examples: `( 1 2 "a" )` or `[1 2 "a"] tuple`.
- Support tuple creation and unpacking.

### Comments
- Single-line comments: start with `--` and extend to end of line.
- Optional multi-line comments: `-- comment --`.
- Comments are stripped during lexing before parsing.

### Variable Rebinding
- Allow `:=` operator to overwrite existing symbol bindings.
- Optionally add `const` keyword for immutable variables.
- Support variable shadowing if scoped environments are added.

### Extended IO
- File operations: `openFile`, `readFile`, `writeFile`, `closeFile`.
- Networking: HTTP requests like `httpGet` and `httpPost`.
- IO operations handled as interrupts to preserve pure evaluation.
- Utilize Haskell libraries such as `network` and `http-client`.

