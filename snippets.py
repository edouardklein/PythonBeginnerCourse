########################
# "Happy path" version #
########################
import random

secret_number = random.randint(1, 100)

guesses = 0

while True:
    guess = int(input("Take a guess: "))
    guesses = guesses + 1
    if guess == secret_number:
        print(f"Correct! The number was {secret_number}!")
        print(f"You took {guesses} guesses.")
        break
    elif guess < secret_number:
        print("Too low!")
    else:
        print("Too high!")

print('Game finished')


#################
# Terse version #
#################
import random

secret_number = random.randint(1, 100)

guesses = 0
# BUG HERE
while guess != secret_number:
    guess = int(input("Take a guess: "))
    guesses = guesses + 1
    if guess < secret_number:
        print("Too low!")
    elif guess > secret_number:
        print("Too high!")
print(f"Correct! The number was {secret_number}!")
print(f"You took {guesses} guesses.")

