from z3 import *
import time
import random

WORDS = []
with open("../words_max5.txt", "r", encoding="utf-8") as f:
    WORDS = [line.strip() for line in f if line.strip()]

def isSolvable(word1, word2, word3):
    letters = set(word1 + word2 + word3)
    return len(letters) <= 10

def getFeasibleWords():
  while True:
    word1, word2, word3 = random.sample(WORDS, 3)
    if(isSolvable(word1, word2, word3)):
      return word1, word2, word3

#Function to verify that the mapping z3 found actually satisfies the arithmetic equation
def verifySolution(dl_dict, firstWord, secondWord, resultWord):
  def wordToNum(word):
    return int("".join(str(dl_dict[letter]) for letter in word))

  firstNum = wordToNum(firstWord)
  secondNum = wordToNum(secondWord)
  resultNum = wordToNum(resultWord)
  return firstNum + secondNum == resultNum

#Find all possible solutions
def findAllCryptarithmSolutions(maxSolutions = 500000):
  firstWord, secondWord, resultWord = getFeasibleWords()

  #Get the distinct letters from the users input to be used in creating the z3 Integer variables
  distinctLetters = set(firstWord + secondWord + resultWord)

  #create a dictionary and map each letter to a z3 interger variable
  dlDict = {letter: Int(letter) for letter in distinctLetters}

  #instantiate a Solver
  solver = Solver()

  #Convert word to numeric expression
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
  solutions = []
  startTime = time.time()

  print(f"Finding all possible solutions (limited to {maxSolutions})")
  while solver.check() == sat:
    model = solver.model()
    solution = {letter: model[dlDict[letter]].as_long() for letter in distinctLetters}

    if verifySolution(solution, firstWord, secondWord, resultWord):
      solutions.append(solution)
    else:
      return

    solver.add(Or([dlDict[l] != model[dlDict[l]] for l in distinctLetters]))

    if len(solutions) >= maxSolutions:
      print(f"\nStopping after {maxSolutions} solutions to avoid long computation time.")
      break

  elapsed = time.time() - startTime
  print(f"\nTotal time: {elapsed:.2f}s")

  if solutions:
      print(f"\n{len(solutions)} total solutions were found (limited to {maxSolutions})")
  else:
      print("No solutions found.")

  return solutions


#Test edge cases to further prove that the constraints are accurate

findAllCryptarithmSolutions()