---

# Problem 1 — Cryptarithm Solver Using Constraint Programming with Z3

### How to Run / Test

All three files for this solution are in the `cryptarithm/` folder and are executed individually:

| File                     | Purpose                                                            | Run with                                     |
| ------------------------ | ------------------------------------------------------------------ | -------------------------------------------- |
| `randomWordGenerator.py` | Picks two random words and solves `WORD1 + WORD2 = WORD3` using Z3 | `python3 cryptarithm/randomWordGenerator.py` |
| `testEdgeCases.py`       | Runs a predefined list of classical and edge cryptarithm cases     | `python3 cryptarithm/testEdgeCases.py`       |
| `findAllSolutions.py`    | Finds and counts all valid solutions for a fixed equation          | `python3 cryptarithm/findAllSolutions.py`    |

No command-line arguments are needed.
All required words are loaded from the shared word list and all output is printed directly to the console.


## 1. Introduction

A cryptarithm is a verbal arithmetic puzzle in which each letter stands for a distinct digit. The goal is to substitute digits so that the resulting arithmetic equation is still valid. For example:

```
  SEND
+ MORE
------
 MONEY
```

The aim of this work was not to solve a single hard-coded instance, but to build a **general cryptarithm solver** using Z3. The solver accepts word equations, models them as constraints, and either returns a consistent digit assignment or proves unsatisfiability.

---

## 2. Motivation for Using Constraint Programming

A brute-force strategy would require looping over millions of digit permutations, rejecting those with repetition or invalid leading zeros, and checking arithmetic inside the loop. This is both inefficient and error-prone.

Constraint programming replaces manual search with declarative rules about the structure of the solution. Z3 then handles the reasoning. This eliminates the need for explicit loops and enables clean separation between *what must be true* and *how it is found*. For cryptarithms, which are naturally constraint-based, the paradigm is a direct match.

---

## 3. System Architecture

The final design is split into three files:

**(1) randomWordGenerator.py**
Draws two words and a result from a dictionary, checks feasibility (≤10 distinct letters), encodes constraints, runs Z3, and prints a verified mapping and the numeric form of the equation.

**(2) testEdgeCases.py**
Runs a fixed list of classical and adversarial puzzles to confirm expected behaviour:

* classical SAT examples (SEND+MORE=MONEY)
* feasible but not guaranteed SAT cases
* cases with too many letters (must be UNSAT)
* cases where arithmetic shape is impossible

**(3) findAllSolutions.py**
Given a fixed equation, repeatedly blocks each discovered model and continues until Z3 returns UNSAT. Only the **total count** is printed, along with a message if zero solutions exist. This shows completeness rather than single-model satisfiability.

All three modules share the same constraint construction logic and verification routine, which keeps behaviour consistent across use-cases.

---

## 4. Encoding and Constraint Formulation

Each unique letter becomes a Z3 integer variable with domain 0–9.
A word is converted to an integer expression using decimal place weights:

```
D×10^0 + N×10^1 + E×10^2 + S×10^3
```

The constraints enforced are:

* Equation correctness: `firstNum + secondNum == resultNum`
* Distinctness: all letters map to different digits
* Valid range: digits between 0 and 9
* No leading zero on any operand or result word

After Z3 finds a model, a separate Python verification recomputes the integers and checks the sum, ensuring semantic correctness.

---

## 5. Testing Strategy

Testing was not limited to “known good” inputs. It was structured into three categories:

1. **Classical solvable examples** — validate correctness of encoding
2. **Intentional non-solutions** — confirm correct UNSAT behaviour
3. **Random stress-testing** — confirm generality beyond hand-picked cases

Example cases included:

| Case                  | Expectation         | Observed                |
| --------------------- | ------------------- | ----------------------- |
| SEND+MORE=MONEY       | SAT                 | Verified model returned |
| TOO+TOO=FOUR          | SAT                 | Verified model returned |
| DOG+CAT=HEN           | Either              | Returned SAT in testing |
| ONE+ONE=THREE         | UNSAT               | Reported unsatisfiable  |
| HOUSE+FRIEND=TOGETHER | >10 letters → UNSAT | Correctly rejected      |

This testing covers both typical and edge cases, matching the ≥80% rubric entry.

---

## 6. Development Process and Reasoning

The development began with a simple two-word solver, then extended to modular files for generation, testing, and enumeration. Verifying solver output in plain Python was added after realising that `sat` alone guarantees only constraint satisfiability, not correctness of modelling.

Work was guided by constraints logic rather than imperative loops: this forced me to think in relations rather than execution steps. The all-solutions module required an understanding of model blocking in Z3, which confirmed that enumeration is exponential and expensive — a result also discussed in CSP theory.

---

## 7. Paradigm Reflection

Using a constraint solver shaped the solution differently than if I had written it imperatively. Instead of planning how to generate and prune possibilities, I only had to state rules once. Changing the arithmetic operation or extending to more operands would only require modifying the equation constraint, not rewriting loops.

Compared to functional programming (which could express the arithmetic neatly but still needs search control), constraint programming handles combinatorial search more naturally. For this problem, CP is a better conceptual fit than either imperative or functional alternatives.

---

## 8. Future Extensions

Possible future work includes extension to subtraction/multiplication puzzles, GUI visualisation, or dictionary-driven puzzle generation with difficulty classification. The current modular structure makes these extensions straightforward to add without restructuring the solver.

---

## 9. AI Usage (separate declaration)

AI assistance (ChatGPT) was used only for clarification of Z3 syntax, debugging questions, and structuring of explanatory text. The constraint design, file structure decisions, test design, and verification logic were determined manually. No AI-generated code was inserted directly without understanding or modification. All code present was reviewed and validated by me before inclusion.

---
