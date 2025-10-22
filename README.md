# Assessment Solution: Cryptography and Logic Puzzles
-----

## Project Overview

This repository contains the complete solution for the 7017SCN module assessment. The project is divided into two distinct parts, demonstrating skills in logic programming (using Z3) and functional programming (using Haskell) for solving computational problems.

1.  **Cryptarithm Solver:** A Python script utilizing the Z3 Satisfiability Modulo Theories (SMT) solver to find unique solutions for cryptarithmetic puzzles.
2.  **Simple Cryptography:** A Haskell implementation of two classical ciphers: the Caesar Cipher and the Vigenère Cipher.

-----

## Part 1: Cryptarithm Solver (Python & Z3) 🐍🧩

This solution uses the Z3 theorem prover to transform a cryptarithmetic puzzle (an equation where letters represent digits) into a set of constraints and find a unique numerical assignment that satisfies them.

### Prerequisites

  * **Python 3.x:** Installed on your system.
  * **Z3 Solver:** The Python bindings for Z3 must be installed.

### Setup and Installation

1.  **Clone the repository:**
    ```bash
    git clone [Your Repository URL]
    cd [repository-name]
    ```
2.  **Install the Z3 library:**
    ```bash
    pip install z3-solver
    ```

### How to Run

The main script is `cryptarithm_solver.py` (or similar, adjust the name). It is designed to solve a hardcoded puzzle (e.g., `SEND + MORE = MONEY`) or read from an input file/argument.

**To run the solver:**

```bash
python cryptarithm_solver.py [OPTIONAL: PUZZLE_STRING, e.g., 'A+B=C']
```

*(If the script uses a hardcoded example, you can omit the argument.)*

### Example Output

When run, the script should output the solved equation and the unique mapping of letters to digits.

```
Solving: SEND + MORE = MONEY
Solution Found:
S = 9, E = 5, N = 6, D = 7, M = 1, O = 0, R = 8, Y = 2
Check: 9567 + 1085 = 10652 (Correct)
```

-----

## Part 2: Simple Cryptography (Haskell)  functional

This section implements core cryptographic concepts using Haskell, focusing on the principles of functional purity and immutability.

### Prerequisites

  * **GHC (The Glasgow Haskell Compiler):** Installed on your system.
  * **Cabal or Stack:** A build tool for Haskell projects (e.g., `stack`).

### Setup and Installation

1.  **Navigate to the Haskell directory:**
    ```bash
    cd haskell-cipher
    ```
    *(Adjust the directory name as necessary.)*
2.  **Build the project (using Stack as an example):**
    ```bash
    stack setup
    stack build
    ```

### How to Run

The Haskell program can be run either as an interpreted script or a compiled executable.

#### 1\. Running via GHCi (Interpreter)

```bash
ghci [Cipher File Name, e.g., Cipher.hs]
```

Once in the GHCi prompt, you can call the functions directly:

```haskell
-- Encrypt "HELLO" with a Caesar shift of 3
*Main> caesarEncrypt 3 "HELLO"
"KHOOR"

-- Encrypt "ATTACKATDAWN" with Vigenère key "LEMON"
*Main> vigenereEncrypt "LEMON" "ATTACKATDAWN"
"LXFOPVEFRNHR"
```

#### 2\. Running as an Executable (if configured)

If your project is set up to compile to an executable:

```bash
stack exec simple-cipher -- encrypt caesar 3 "HELLO"
```

*(Adjust the command structure based on your specific executable interface.)*

### Implemented Ciphers

| Cipher | Functionality |
| :--- | :--- |
| **Caesar Cipher** | Implements both **encryption** and **decryption** by shifting letters a fixed number of positions (e.g., $\text{ciphertext} = (\text{plaintext} + \text{key}) \bmod 26$). |
| **Vigenère Cipher** | Implements both **encryption** and **decryption** using a polyalphabetic substitution based on a keyword. |

-----

## Academic Notes

### Part 1: Cryptarithm Solver (Z3)

This approach leverages the power of **SMT Solvers** to perform complex constraint satisfaction. It demonstrates:

  * **Modelling:** Translating a real-world puzzle into a system of algebraic and logical constraints.
  * **Efficiency:** Using an industrial-grade solver instead of manual brute-force enumeration.

### Part 2: Simple Cryptography (Haskell)

The Haskell implementation focuses on core **Functional Programming** concepts:

  * **Purity:** Functions are side-effect free, making them easy to test and reason about.
  * **Composition:** Breaking down complex tasks (like Vigenère) into smaller, composable functions.
  * **Type Safety:** Utilizing Haskell's strong type system to prevent common errors.

-----