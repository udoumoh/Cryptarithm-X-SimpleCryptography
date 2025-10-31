from z3 import *
import random

with open("../words_max5.txt", "r", encoding="utf-8") as f:
    WORDS = [line.strip() for line in f if line.strip()]


def isSolvable(lhsWords, resultWord):
    letters = set("".join(lhsWords) + resultWord)
    return len(letters) <= 10


def pickRandomEquation():
    while True:
        # randomly choose how many addends will be on the left-hand side
        numAddends = random.randint(2, 4) 

        pickedWords = random.sample(WORDS, numAddends + 1)

        lhsWords   = pickedWords[:-1]
        resultWord = pickedWords[-1]

        if isSolvable(lhsWords, resultWord):
            return lhsWords, resultWord


def wordToNum(word, dlDict):
    return Sum([
        dlDict[letter] * (10 ** i)
        for i, letter in enumerate(reversed(word))
    ])


def verifySolution(solutionMap, lhsWords, resultWord):
    def evalWordConcrete(w):
        return int("".join(str(solutionMap[ch]) for ch in w))

    lhsVals   = [evalWordConcrete(w) for w in lhsWords]
    resultVal = evalWordConcrete(resultWord)

    return sum(lhsVals) == resultVal


def cryptarithmSolver():
    # Step 1: pick a random feasible equation (words, not digits yet)
    lhsWords, resultWord = pickRandomEquation()

    # Collect all distinct letters across all chosen words
    distinctLetters = set("".join(lhsWords) + resultWord)

    # Map each letter to a fresh Z3 Int variable
    dlDict = {letter: Int(letter) for letter in distinctLetters}

    # Create the Z3 solver
    solver = Solver()

    # Build numeric form (Z3 expressions) of each LHS word and the RHS word
    lhsNums   = [wordToNum(w, dlDict) for w in lhsWords]
    resultNum = wordToNum(resultWord, dlDict)

    # The first letter of any word cannot map to 0 (no leading zeros allowed)
    leadingLetters = {w[0] for w in lhsWords + [resultWord]}

    # Add the constraints to the solver


    solver.add(Sum(lhsNums) == resultNum)
    solver.add(*[
        And(dlDict[ch] >= 0, dlDict[ch] <= 9)
        for ch in distinctLetters
    ])
    solver.add(*[
        dlDict[ch] != 0
        for ch in leadingLetters
    ])
    solver.add(Distinct([dlDict[ch] for ch in distinctLetters]))

    # Try to solve this instance
    print(f"Finding a solution for {' + '.join(lhsWords)} = {resultWord}")
    if solver.check() != sat:
        print("No solution found for that selection, retrying...\n")
        return cryptarithmSolver()

    # Extract the model
    model = solver.model()
    solutionMap = {
        ch: model[dlDict[ch]].as_long()
        for ch in distinctLetters
    }

    print("Verifying solution...")
    if not verifySolution(solutionMap, lhsWords, resultWord):
        print("Solution did not verify arithmetically, retrying...\n")
        return cryptarithmSolver()

    print("\n✅ Solution Verified Successfully!")
    print("=" * 40)

    # Show mapping letter -> digit
    print("Letter-to-Digit Mapping:")
    for letter in sorted(solutionMap.keys()):
        print(f"  {letter} = {solutionMap[letter]}")

    def evalWordNum(w):
        return int("".join(str(solutionMap[ch]) for ch in w))

    lhsVals   = [evalWordNum(w) for w in lhsWords]
    resultVal = evalWordNum(resultWord)

    maxLenWord = max(len(w) for w in lhsWords + [resultWord])
    maxLenVal  = max(len(str(v)) for v in lhsVals + [resultVal])

    print("\nCryptarithm Equation:")
    for i, (w, v) in enumerate(zip(lhsWords, lhsVals)):
        prefix = "+ " if i > 0 else "  "
        print(f"{prefix}{w.rjust(maxLenWord)}  ({str(v).rjust(maxLenVal)})")

    print("-" * (maxLenWord + 6 + maxLenVal))

    # Print the result word and its numeric value
    print(f"  {resultWord.rjust(maxLenWord)}  ({str(resultVal).rjust(maxLenVal)})")
    print("=" * 40)


cryptarithmSolver()
