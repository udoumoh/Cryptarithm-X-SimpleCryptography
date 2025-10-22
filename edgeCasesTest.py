from z3 import *
import time
import random

WORDS = []
with open("../words_max5.txt", "r", encoding="utf-8") as f:
    WORDS = [line.strip() for line in f if line.strip()]

#Function to verify that the mapping z3 found actually satisfies the arithmetic equation
def verifySolution(dl_dict, firstWord, secondWord, resultWord):
  def wordToNum(word):
    return int("".join(str(dl_dict[letter]) for letter in word))

  firstNum = wordToNum(firstWord)
  secondNum = wordToNum(secondWord)
  resultNum = wordToNum(resultWord)
  return firstNum + secondNum == resultNum


def cryptarithmSolver(firstWord, secondWord, resultWord):

  #Get the distinct letters from the users input to be used in creating the z3 Integer variables
  distinctLetters = set(firstWord + secondWord + resultWord)

  #create a dictionary and map each letter to a z3 interger variable
  dlDict = {letter: Int(letter) for letter in distinctLetters}

  #instantiate a Solver
  solver = Solver()

  #Function to convert each word to number
  def wordToNum(word, dl_dict):
    return sum(dl_dict[letter] * (10 ** i) for i, letter in enumerate(reversed(word)))

  firstNum = wordToNum(firstWord, dlDict)
  secondNum = wordToNum(secondWord, dlDict)
  resultNum = wordToNum(resultWord, dlDict)

  firstLetters = {firstWord[0], secondWord[0], resultWord[0]}

  #Add the constraints to the solver
  solver.add(firstNum + secondNum == resultNum)
  solver.add(*[And(dlDict[l] >= 0, dlDict[l] <= 9) for l in distinctLetters])
  solver.add(*[dlDict[letter] != 0 for letter in firstLetters])
  solver.add(Distinct([dlDict[l] for l in distinctLetters]))

  #Find a solution using z3 solver
  print(f"Finding a solution for {firstWord} + {secondWord} = {resultWord} ")
  if solver.check() == sat:
    model = solver.model()
    solution = {letter: model[dlDict[letter]].as_long() for letter in distinctLetters}
    print("Verifying Solution")
      
    if verifySolution(solution, firstWord, secondWord, resultWord):
      return solution
    else:
      return None
  else:
    return None

edgeCasesTests = [
    # solvable
    ("SEND", "MORE", "MONEY"),
    # unsatisfiable mathematically
    ("DOG", "CAT", "HEN"),
    # too many distinct letters
    ("HOUSE", "FRIEND", "TOGETHER"),
    # leading zero test case
    ("TEN", "TEN", "TWENTY"),
    # structured but impossible
    ("ONE", "ONE", "THREE"),
    # possibly solvable depending on mapping
    ("TOO", "TOO", "FOUR"),
    # trivial short case
    ("A", "B", "C"),
]

def runEdgeCaseTests(testCases):
    print("\n=== Running Edge Case Tests ===\n")
    for firstWord, secondWord, resultWord in testCases:
        print(f"Testing: {firstWord} + {secondWord} = {resultWord}")
        result = cryptarithmSolver(firstWord, secondWord, resultWord)
        if result is None:
            print("No valid solution\n")
        else:
          print("\n✅ Solution Verified Successfully!")
          print("=" * 40)
          print("Letter-to-Digit Mapping:")
            
          for letter in sorted(result.keys()):
            print(f"  {letter} = {result[letter]}")

          # Evaluate numeric values
          def evalWord(word):
            return int("".join(str(result[letter]) for letter in word))

          firstNum = evalWord(firstWord)
          secondNum = evalWord(secondWord)
          resultNum = evalWord(resultWord)

          print("\nCryptarithm Equation:")
          max_len = max(len(firstWord), len(secondWord), len(resultWord))
          print(f"  {firstWord.rjust(max_len)}  ({firstNum})")
          print(f"+ {secondWord.rjust(max_len-1)}  ({secondNum})")
          print("-" * (max_len + 6))
          print(f"  {resultWord.rjust(max_len)}  ({resultNum})")
          print("=" * 40)
          print("SAT — solution printed above\n")
    print("=== End of Edge Case Tests ===\n")

runEdgeCaseTests(edgeCasesTests)