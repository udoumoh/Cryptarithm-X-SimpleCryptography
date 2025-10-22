from z3 import *
import time

#Funtion to find all possible solutions
def findAllCryptarithmSolutions(firstWord, secondWord, resultWord, maxSolutions = 5):
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

  while solver.check() == sat:
    model = solver.model()
    solution = {letter: model[dlDict[letter]].as_long() for letter in distinctLetters}

    if verifySolution(solution, firstWord, secondWord, resultWord):
      solutions.append(solution)
      print(f"Found solution {len(solutions)}: {solution}")
    else:
      print("Found invalid mapping — skipping")

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
  if solver.check() == sat:
      model = solver.model()
      solution = {letter: model[dlDict[letter]].as_long() for letter in distinctLetters}
      print("Verifying Solution")
      if verifySolution(solution, firstWord, secondWord, resultWord):
        print("Solution has been verified")
        print("-" * 25)
        for letter in sorted(solution.keys()):
            print(f"{letter} = {solution[letter]}")
        print("-" * 25)

        # Evaluate the numeric values for the equation
        def evalWord(word):
            return int("".join(str(solution[letter]) for letter in word))

        firstNum = evalWord(firstWord)
        secondNum = evalWord(secondWord)
        resultNum = evalWord(resultWord)

        # Display the full equation
        print(f"\n{firstWord} + {secondWord} = {resultWord}")
        print(f"{firstNum} + {secondNum} = {resultNum}")
      else:
        print("Unable to verify: ", solution)
  else:
    print("No solution found.")


#Function to verify that the mapping z3 found actually satisfies the arithmetic equation
def verifySolution(dl_dict, firstWord, secondWord, resultWord):
  def wordToNum(word):
    return int("".join(str(dl_dict[letter]) for letter in word))

  firstNum = wordToNum(firstWord)
  secondNum = wordToNum(secondWord)
  resultNum = wordToNum(resultWord)
  return firstNum + secondNum == resultNum


#Test edge cases to further prove that the constraints are accurate
testCases = [
    ("SEND", "MORE", "MONEY"),        
    ("TOO", "TOO", "FOUR"),         
    ("BASE", "BALL", "GAMES"),  
    ("DOG", "CAT", "HEN"),            
    ("HOUSE", "FRIEND", "TOGETHER"),  
    ("SUN", "MOON", "STARS"),       
]

print("\n=== Testing Edge Cases ===\n")
for firstWord, secondWord, resultWord in testCases:
    print(f"Testing: {firstWord} + {secondWord} = {resultWord}")
    result = cryptarithmSolver(firstWord, secondWord, resultWord)
    print(result)
    print("-" * 40)



#Custom list of valid english words for the user to choose from
validWords = [
      "SEND", "MORE", "MONEY",
      "CATS", "DOGS", "BIRD", "FISH",
      "TREE", "HOUSE", "RAIN", "SNOW",
      "STAR", "MOON", "SUN", "SKY",
      "BOOK", "READ", "WORD", "NOTE",
      "GAME", "PLAY", "CARD", "DICE"
  ]

allowedWordsStr = ", ".join(sorted(validWords))

#Get the users input for the words
firstWord = input(f"Kindly Input the first word from one of these allowed words: {allowedWordsStr}\n").upper()
secondWord = input(f"Kindly Input the second word from one of these allowed words: {allowedWordsStr}\n").upper()
resultWord = input(f"Kindly Input the result word from one of these allowed words: {allowedWordsStr}\n").upper()

#Check for duplicate words and throw an error if any is found
for word in (firstWord, secondWord, resultWord):
  if word not in validWords:
    print(f"Error! '{word}' is not an allowed word.")
    exit()

cryptarithmSolver(firstWord, secondWord, resultWord)

findAllCryptarithmSolutions(firstWord, secondWord, resultWord,  maxSolutions=20)