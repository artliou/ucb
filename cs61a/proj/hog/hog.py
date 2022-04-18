"""The Game of Hog."""

from dice import four_sided, six_sided, make_test_dice
from ucb import main, trace, log_current_line, interact

GOAL_SCORE = 100  # The goal of Hog is to score 100 points.
#Code may be similar to when I did this project in Fall 2013.
#All Tests except Fuzz Test and 77 Win Rate passes (as of 1151pm on night of submission)
######################
# Phase 1: Simulator #
######################


def roll_dice(num_rolls, dice=six_sided):
    """Simulate rolling the DICE exactly NUM_ROLLS times. Return the sum of
    the outcomes unless any of the outcomes is 1. In that case, return 0.
    """
    # These assert statements ensure that num_rolls is a positive integer.
    assert type(num_rolls) == int, 'num_rolls must be an integer.'
    assert num_rolls >= 0, 'Must roll at least once or choose not to.'
    # BEGIN Question 1
    assert num_rolls < 11
    rolls = 0
    total = 0
    term = True

    while rolls < num_rolls:
        outcome = dice()
        if outcome == 1:
            term = False
        total = total + outcome
        rolls += 1

    if term == True:
        return total
    else:
        return 0
    # END Question 1


def is_prime(x):
    if x < 2:
        return False
    if x == 2:
        return True
    i = 2
    while i < x:
        if x % i == 0:
            return False
        i += 1
    else:
        return True

def next_prime(x):
    if is_prime(x+1) == True:
        return x + 1
    else:
        return next_prime(x+1)

def take_turn(num_rolls, opponent_score, dice=six_sided):
    """Simulate a turn rolling NUM_ROLLS dice, which may be 0 (Free bacon).

    num_rolls:       The number of dice rolls that will be made.
    opponent_score:  The total score of the opponent.
    dice:            A function of no args that returns an integer outcome.
    """
    assert type(num_rolls) == int, 'num_rolls must be an integer.'
    assert num_rolls >= 0, 'Cannot roll a negative number of dice.'
    assert num_rolls <= 10, 'Cannot roll more than 10 dice.'
    assert opponent_score < 100, 'The game should be over.'
    # BEGIN Question 2
    n = roll_dice(num_rolls, dice)

    if num_rolls == 0:
        first = opponent_score % 10
        second = (opponent_score // 10) % 10
        score = 1 + max(first, second)
        if is_prime(score) == True:
            return next_prime(score)
        return score
    if is_prime(n) == True:
        return next_prime(n)
    else:
        return n
    # END Question 2

#score = 1 + max(opponent_score//10, opponent_score%10)

def select_dice(score, opponent_score):
    """Select six-sided dice unless the sum of SCORE and OPPONENT_SCORE is a
    multiple of 7, in which case select four-sided dice (Hog wild).
    """
    # BEGIN Question 3
    #Coded originally
    if (score + opponent_score) % 7 == 0:
        return four_sided
    else:
        return six_sided
    # END Question 3


def is_swap(score0, score1):
    """Returns whether the last two digits of SCORE0 and SCORE1 are reversed
    versions of each other, such as 19 and 91.
    """
    # BEGIN Question 4
    first = score0 % 10
    second = (score0 // 10) % 10
    third = score1 % 10
    fourth = (score1 // 10) % 10
    if first == fourth and second == third:
        return True
    return False

    """
    if int(str(score1)[::-1]) == score0:
        return True
    elif int(str(score0)[::-1]) == score1:
        return True
    if score0 > 100:
        score0 = score0 - 100
        if int(str(score1)[::-1]) == score0:
            return True
    if score1 > 100:
        score1 = score1 - 100
        if int(str(score0)[::-1]) == score1:
            return True
    return False"""
    # END Question 4


def other(who):
    """Return the other player, for a player WHO numbered 0 or 1.

    >>> other(0)
    1
    >>> other(1)
    0
    """
    if who == 0:
        return 1
    return 0


def play(strategy0, strategy1, score0=0, score1=0, goal=GOAL_SCORE):
    """Simulate a game and return the final scores of both players, with
    Player 0's score first, and Player 1's score second.

    A strategy is a function that takes two total scores as arguments
    (the current player's score, and the opponent's score), and returns a
    number of dice that the current player will roll this turn.

    strategy0:  The strategy function for Player 0, who plays first
    strategy1:  The strategy function for Player 1, who plays second
    score0   :  The starting score for Player 0
    score1   :  The starting score for Player 1
    """
    who = 0  # Which player is about to take a turn, 0 (first) or 1 (second)
    zeros = roll_dice(strategy0(score0, score1), select_dice(score0, score1))
    zero = roll_dice(strategy0(score1, score0), select_dice(score1, score0))
    
    #this HOF is for Piggy Back
    def roll_dice_other(num_rolls, dice=six_sided):
        assert type(num_rolls) == int, 'num_rolls must be an integer.'
        assert num_rolls >= 0, 'Must roll at least once or choose not to.'
        assert num_rolls < 11
        rolls = 0
        total = 0

        while rolls < num_rolls:
            outcome = dice()
            total = total + outcome
            rolls += 1
        return total


    # BEGIN Question 5
    while (score0 < goal) and (score1 < goal):

        if who == 0:
            if take_turn(strategy0(score0, score1), score1, select_dice(score0, score1)) == 0:
                score1 += roll_dice_other(strategy0(score0, score1), select_dice(score0, score1))
            else:
                score0 += take_turn(strategy0(score0, score1), score1, select_dice(score0, score1))
        else:
            if take_turn(strategy1(score1, score0), score0, select_dice(score1, score0)) == 0:
                score0 += roll_dice_other(strategy1(score1, score0), select_dice(score1, score0))
            else:
                score1 += take_turn(strategy1(score1, score0), score0, select_dice(score1, score0))

        if is_swap(score0, score1) == True:
            score1, score0 = score0, score1
        
        who = other(who)
    return score0, score1

# END Question 5

#######################
# Phase 2: Strategies #
#######################


def always_roll(n):
    """Return a strategy that always rolls N dice.

    A strategy is a function that takes two total scores as arguments
    (the current player's score, and the opponent's score), and returns a
    number of dice that the current player will roll this turn.

    >>> strategy = always_roll(5)
    >>> strategy(0, 0)
    5
    >>> strategy(99, 99)
    5
    """
    def strategy(score, opponent_score):
        return n
    return strategy


# Experiments

def make_averaged(fn, num_samples=1000):
    """Return a function that returns the average_value of FN when called.

    To implement this function, you will have to use *args syntax, a new Python
    feature introduced in this project.  See the project description.

    >>> dice = make_test_dice(3, 1, 5, 6)
    >>> averaged_dice = make_averaged(dice, 1000)
    >>> averaged_dice()
    3.75
    >>> make_averaged(roll_dice, 1000)(2, dice)
    5.5

    In this last example, two different turn scenarios are averaged.
    - In the first, the player rolls a 3 then a 1, receiving a score of 0.
    - In the other, the player rolls a 5 and 6, scoring 11.
    Thus, the average value is 5.5.
    Note that the last example uses roll_dice so the hogtimus prime rule does
    not apply.
    """
    # BEGIN Question 6
    def average_function(*args):
        # k is a counter to keep track of how many times fn has been run,
        # total represents the total sum of the return values of fn
        total = 0
        k = 0
        while k < num_samples:
            total += fn(*args)
            k += 1
        return total / num_samples
    return average_function    
    # END Question 6


def max_scoring_num_rolls(dice=six_sided, num_samples=1000):
    """Return the number of dice (1 to 10) that gives the highest average turn
    score by calling roll_dice with the provided DICE over NUM_SAMPLES times.
    Assume that dice always return positive outcomes.

    >>> dice = make_test_dice(3)
    >>> max_scoring_num_rolls(dice)
    10
    """
    # BEGIN Question 7
    k = 1
    avg1 = 0
    avg2 = 0
    highest = 1
    while k < 11:
        avg2 = make_averaged(roll_dice, 1000)(k, dice)
        if avg2 > avg1:
            highest = k
            avg1 = avg2
        #print (k, "dice scores", avg2 , "on average")
        k+=1
    return highest

    # END Question 7


def winner(strategy0, strategy1):
    """Return 0 if strategy0 wins against strategy1, and 1 otherwise."""
    score0, score1 = play(strategy0, strategy1)
    if score0 > score1:
        return 0
    else:
        return 1


def average_win_rate(strategy, baseline=always_roll(5)):
    """Return the average win rate of STRATEGY against BASELINE. Averages the
    winrate when starting the game as player 0 and as player 1.
    """
    win_rate_as_player_0 = 1 - make_averaged(winner)(strategy, baseline)
    win_rate_as_player_1 = make_averaged(winner)(baseline, strategy)

    return (win_rate_as_player_0 + win_rate_as_player_1) / 2


def run_experiments():
    """Run a series of strategy experiments and report results."""
    if True:  # Change to False when done finding max_scoring_num_rolls
        six_sided_max = max_scoring_num_rolls(six_sided)
        print('Max scoring num rolls for six-sided dice:', six_sided_max)
        four_sided_max = max_scoring_num_rolls(four_sided)
        print('Max scoring num rolls for four-sided dice:', four_sided_max)

    if False:  # Change to True to test always_roll(8)
        print('always_roll(8) win rate:', average_win_rate(always_roll(8)))

    if False:  # Change to True to test bacon_strategy
        print('bacon_strategy win rate:', average_win_rate(bacon_strategy))

    if False:  # Change to True to test swap_strategy
        print('swap_strategy win rate:', average_win_rate(swap_strategy))

    "*** You may add additional experiments as you wish ***"


# Strategies

def bacon_strategy(score, opponent_score, margin=8, num_rolls=5):
    """This strategy rolls 0 dice if that gives at least MARGIN points,
    and rolls NUM_ROLLS otherwise.
    """
    # BEGIN Question 8
    if take_turn(0, opponent_score) >= margin:
        return 0
    else:
        return num_rolls # Replace this statement
    # END Question 8


def swap_strategy(score, opponent_score, num_rolls=5):
    """This strategy rolls 0 dice when it results in a beneficial swap and
    rolls NUM_ROLLS otherwise.
    """
    # BEGIN Question 9
    "*** REPLACE THIS LINE ***"
    if is_swap(score + take_turn(0, opponent_score), opponent_score) == True:
        n = score + take_turn(0, opponent_score)
        while opponent_score > n:
            return 0
        return num_rolls
    return num_rolls
    # Replace this statement
    # END Question 9

def final_strategy(score, opponent_score):
    """Write a brief description of your final strategy.

    *** YOUR DESCRIPTION HERE ***
    """
    # BEGIN Question 10
    diff = score - opponent_score
    ndiff = opponent_score - score
    num_rolls = 6
    margin = 6

    if diff > 15: #if we are winning by a lot
        return 0
    if diff < 40: #if we aren't winning by much, use bacon!
        return bacon_strategy(score, opponent_score, margin, num_rolls)
    if ndiff >= 10: #if we are losing, then wait for swap strategy
        return swap_strategy(score, opponent_score, num_rolls)
    #if near 100 points, try not to go over
    if 100 - opponent_score <= 20:
        return 4

    if 100 - score <= 10:
        return 2
    if 100 - score <= 15:
        return 3
    return num_rolls
 # Replace this statement
    # END Question 10


##########################
# Command Line Interface #
##########################


# Note: Functions in this section do not need to be changed. They use features
#       of Python not yet covered in the course.


@main
def run(*args):
    """Read in the command-line argument and calls corresponding functions.

    This function uses Python syntax/techniques not yet covered in this course.
    """
    import argparse
    parser = argparse.ArgumentParser(description="Play Hog")
    parser.add_argument('--run_experiments', '-r', action='store_true',
                        help='Runs strategy experiments')

    args = parser.parse_args()

    if args.run_experiments:
        run_experiments()
