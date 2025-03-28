# **Lexa** - A Concatenative, Stack-Based Interpreter

Lexa was chosen for its sleek, modern feel and subtle reference to _lexing_, the process of breaking down source code into tokens. This is fundemental to the core functionality of this concatenative, stack-based interpreter. Written in Haskell, the interpreter processes commands through a stack, offering both an interactive REPL mode and batch execution mode to run programs and manipulate data efficiently.

## **How to Build and Run Lexa**

To build and run Lexa from the terminal, follow these steps:

#### 1. **Clone the repository**

First, clone the Lexa project repository to your local machine.

#### 2. **Install Stack (if not already installed)**

Make sure you have **Stack** installed. If not, you can install it using the following command:

- **On macOS:**

```bash
brew install haskell-stack
```

- **On Ubuntu/Linux:**

```bash
curl -sSL https://get.haskellstack.org/ | sh
```

#### 3. **Build the project**

Use Stack to build the project:

```bash
stack build
```

This will download dependencies and build the project.

#### 4. **Run Lexa (REPL mode)**

To run Lexa in **REPL mode** (interactive mode), use the following command:

```bash
stack run -- -r
```

The `-- -r` syntax passes the `-r` argument to the program, which starts the REPL mode.
