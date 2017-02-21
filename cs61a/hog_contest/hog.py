"""The Game of Hog"""

from dice import four_sided_dice, six_sided_dice, make_test_dice
from ucb import main, trace, log_current_line, interact

goal = 100          # The goal of Hog is to score 100 points.

# Taking turns

def roll_dice(num_rolls, dice=six_sided_dice, who='Boss Hogg'):
    """Calculate WHO's turn score after rolling DICE for NUM_ROLLS times.

    num_rolls:  The number of dice rolls that will be made; at least 1.
    dice:       A function of no args and returns an integer outcome.
    who:        Name of the current player, for commentary.
    """
    assert type(num_rolls) == int, 'num_rolls must be an integer.'
    assert num_rolls > 0, 'Must roll at least once.'
    "*** YOUR CODE HERE ***"
    t, total=1, 0
    one_rolled=False
    while t<=num_rolls:
        roll_value=dice()
        if roll_value == 1:
            return 1
        total, t = total+roll_value, t+1
    return total

def take_turn(num_rolls, opponent_score, dice=six_sided_dice, who='Boss Hogg'):
    """Simulate a turn in which WHO chooses to roll NUM_ROLLS, perhaps 0.

    num_rolls:       The number of dice rolls that will be made.
    opponent_score:  The total score of the opponent.
    dice:            A function of no args and returns an integer outcome.
    who:             Name of the current player, for commentary.
    """
    assert type(num_rolls) == int, 'num_rolls must be an integer.'
    assert num_rolls >= 0, 'Cannot roll a negative number of dice.'
    "*** YOUR CODE HERE ***"
    if num_rolls == 0:
        return opponent_score//10 +1
    #else
    return roll_dice(num_rolls, dice, who)

# Game simulator

def num_allowed_dice(score, opponent_score):
    """Return the maximum number of dice allowed this turn. The maximum
    number of dice allowed is 10 unless the sum of SCORE and
    OPPONENT_SCORE has a 7 as its ones digit.

    >>> num_allowed_dice(1, 0)
    10
    >>> num_allowed_dice(5, 7)
    10
    >>> num_allowed_dice(7, 10)
    1
    >>> num_allowed_dice(3, 24)
    1
    """
    "*** YOUR CODE HERE ***"
    if (score + opponent_score) % 10 == 7:
        return 1
    return 10

def select_dice(score, opponent_score):
    """Select 6-sided dice unless the sum of scores is a multiple of 7.

    >>> select_dice(4, 24) == four_sided_dice
    True
    >>> select_dice(16, 64) == six_sided_dice
    True
    """
    "*** YOUR CODE HERE ***"
    if (score + opponent_score) % 7 == 0:
        return four_sided_dice
    else:
        return six_sided_dice

def other(who):
    """Return the other player, for players numbered 0 or 1.

    >>> other(0)
    1
    >>> other(1)
    0
    """
    return (who + 1) % 2

def name(who):
    """Return the name of player WHO, for player numbered 0 or 1."""
    if who == 0:
        return 'Player 0'
    elif who == 1:
        return 'Player 1'
    else:
        return 'An unknown player'

def play(strategy0, strategy1):
    """Simulate a game and return 0 if the first player wins and 1 otherwise.

    A strategy function takes two scores for the current and opposing players.
    It returns the number of dice that the current player will roll this turn.

    If a strategy returns more than the maximum allowed dice for a turn, then
    the maximum allowed is rolled instead.

    strategy0:  The strategy function for player 0, who plays first.
    strategy1:  The strategy function for player 1, who plays second.
    """
    who = 0 # Which player is about to take a turn, 0 (first) or 1 (second)
    "*** YOUR CODE HERE ***"
    score0 = score1 = 0

    # A suite of helper functions to get and update variables
    #   specific to each player, such as score and strategy
    def get_score(who):
        return score0 if who == 0 else score1
    def get_other_score(who):
        return get_score(other(who))
    def get_strategy(who):
        return strategy0 if who == 0 else strategy1
    def add_score(who, score):
        nonlocal score0, score1
        if who == 0:
            score0 += score
        else:
            score1 += score

    # Main Game Execution
    while True:
        # initializes variables to start a round
        my_score = get_score(who)
        opponent_score = get_other_score(who)
        max_dice = num_allowed_dice(my_score, opponent_score)
        type_dice = select_dice(my_score, opponent_score)
        dice_to_roll = get_strategy(who)(my_score, opponent_score)

        # prevent cheaters (the stupid ones) from rolling excessive dice
        if dice_to_roll > max_dice:
            dice_to_roll = max_dice

        # take a turn and add the resulting score to the current player
        score = take_turn(dice_to_roll, opponent_score, type_dice, name(who))
        add_score(who, score)

        #check if the current player has won
        if get_score(who) >= goal:
            return who

        #switch players
        who = other(who)


# Experiments (Phase 2)

def make_average(fn, num_samples=100000):
    """Return a function that returns the average_value of FN when called.

    To implement this function, you will have to use *args syntax, a new Python
    feature introduced in this project.  See the project description.

    >>> dice = make_test_dice(3, 1, 5, 6)
    >>> avg_dice = make_average(dice)
    >>> avg_dice()
    3.75
    >>> avg_score = make_average(roll_dice)
    >>> avg_score(2, dice, False)
    6.0

    In this last example, two different turn scenarios are averaged.
    - In the first, the player rolls a 3 then a 1, receiving a score of 1.
    - In the other, the player rolls a 5 and 6, scoring 11.
    Thus, the average value is 6.0.
    """
    "*** YOUR CODE HERE ***"
    def average_function(*args):
        # k is a counter to keep track of how many times fn has been run,
        # total represents the total sum of the return values of fn
        total = 0
        k=1
        while k <= num_samples:
            total += fn(*args)
            #if k%10000 == 0:
               # print(total/(k))
            k += 1
        return total / num_samples
    return average_function

def compare_strategies(strategy, baseline):
    """Return the average win rate (out of 1) of STRATEGY against BASELINE."""
    as_first = 1 - make_average(play)(strategy, baseline)
    as_second = make_average(play)(baseline, strategy)
    return (as_first + as_second) / 2  # Average the two results

def eval_strategy_range(make_strategy, lower_bound, upper_bound):
    """Return the best integer argument value for MAKE_STRATEGY to use against
    the always-roll-5 baseline, between LOWER_BOUND and UPPER_BOUND (inclusive).

    make_strategy -- A one-argument function that returns a strategy.
    lower_bound -- lower bound of the evaluation range.
    upper_bound -- upper bound of the evaluation range.
    """
    best_value, best_win_rate = 0, 0
    value = lower_bound
    while value <= upper_bound:
        strategy = make_strategy(value)
        win_rate = compare_strategies(strategy)
        if win_rate > best_win_rate:
            best_win_rate, best_value = win_rate, value
        value += 1
    return best_value

def run_experiments():
    """Run a series of strategy experiments and report results."""
    result = eval_strategy_range(always_roll, 1, 10)
    print('Best always_roll strategy:', result)

    if True: # Change to True when ready to test make_comeback_strategy
        result = eval_strategy_range(make_comeback_strategy, 5, 15)
        print('Best comeback strategy:', result)

    if True: # Change to True when ready to test make_mean_strategy
        result = eval_strategy_range(make_mean_strategy, 1, 10)
        print('Best mean strategy:', result)

    "*** You may add additional experiments here if you wish ***"


# Strategies

def final_strategy(score, opponent_score):
    bacon = opponent_score//10 + 1
    points_after_bacon = bacon + score + opponent_score
    min_points = 4

    score_difference = score - opponent_score
    sum_score = score + opponent_score

    gdistance = goal-score
    lead_margin = 10
    major_lead_margin = 15
    lose_margin = -15
    major_lose_margin = -20
    
    #auto-win condition
    if goal - score <= bacon:
    	return 0
    
    #if we are being hog-tied or hog-wilded
    elif sum_score % 10 == 7:
        if bacon > 3 or (points_after_bacon % 7 == 0 or points_after_bacon % 10 == 7):
            return 0
        return 1
    elif sum_score % 7 == 0:
        if gdistance == 2:
            return 1
        elif gdistance < 6:
            return 2
        elif gdistance < 9:
            return 3
        #screw opponent back case
        elif bacon >= min_points and (points_after_bacon % 7 == 0 or points_after_bacon % 10 == 7):
            return 0
        return 4

    #check if we are close to goal
    elif gdistance == 2:
        return 1
    elif gdistance <7:
        return 2
    elif gdistance <11:
        return 3
    elif gdistance <14:
        return 4
    #screw opponenet case
    elif bacon >= min_points and (points_after_bacon % 7 == 0 or points_after_bacon % 10 == 7):
        return 0
    elif gdistance < 18:
        return 5

    #score margins
    elif score_difference < major_lose_margin:
    	return 8
    elif score_difference < lose_margin:
    	return 7
    elif score_difference >= major_lead_margin:
    	return 4
    elif score_difference >= lead_margin:
    	return 5    
    else:
        return 6

def old_strategy(score, opponent_score):
  
    #Calculating Bacon
    bacon = opponent_score//10 + 1
    points_after_bacon = bacon + score + opponent_score

    min_points = 5
    # margins represents the difference in score between our opponent and us. think of them
    #   like point stratas. Based on how much we are winning/losing, we will adjust our rolling
    score_difference = score - opponent_score

    lead_margin = 10
    major_lead_margin = 15
    lose_margin = -10
    major_lose_margin = -15

    #Now run a bunch of tests outlined above to determine how many dice to roll
    # Bacon Tests
    if goal - score <= bacon:
        return 0
    elif bacon >= min_points and (points_after_bacon % 7 == 0 or points_after_bacon % 10 == 7):
        return 0
    #goal margin test
    elif goal-score <=10:
        return 3
    elif goal-score <=15:
        return 4


    # Margin Tests
    elif score_difference < major_lose_margin:
        return 8
    elif score_difference < lose_margin:
        return 7
    elif score_difference >= major_lead_margin:
        return 4
    elif score_difference >= lead_margin:
        return 5
    else:
        return 6

def final_strategy_v_0_5(score, opponent_score):
    bacon = opponent_score//10 + 1
    points_after_bacon = bacon + score + opponent_score
    min_points = 4

    score_difference = score - opponent_score
    sum_score = score + opponent_score

    gdistance = goal-score
    lead_margin = 10
    major_lead_margin = 15
    lose_margin = -15
    major_lose_margin = -20
    
    if goal - score <= bacon:
        return 0
    elif bacon >= min_points and (points_after_bacon % 7 == 0 or points_after_bacon % 10 == 7):
        return 0
    elif sum_score % 10 == 7:
        if bacon > 3:
            return 0
        return 1
    elif sum_score % 7 == 0:
        if gdistance == 2:
            return 1
        elif gdistance < 6:
            return 2
        elif gdistance < 9:
            return 3
        return 4
    elif gdistance == 2:
        return 1
    elif gdistance <7:
        return 2
    elif gdistance <11:
        return 3
    elif gdistance <14:
        return 4
    elif gdistance < 18:
        return 5
    elif score_difference < major_lose_margin:
        return 8
    elif score_difference < lose_margin:
        return 7
    elif score_difference >= major_lead_margin:
        return 4
    elif score_difference >= lead_margin:
        return 5    
    else:
        return 6

def final_strategy_test():
    """Compares final strategy to the baseline strategy."""
    #print('-- Testing final_strategy --')
    ans = compare_strategies(final_strategy, old_strategy)
    print('Win rate:', ans)
    return ans

# def loop_test():
#     total ,k= 0, 1
#     while 2:
#         total += final_strategy_test()
#         avg = total/k
#         k +=1
#         if k%50 == 0:        
#             print('overall avg = ', avg)

# t=1
# a=[]
# x=1
# while t<x:
#     a+= [t]
#     t+=1
    
'''
def goal_test(num_from_goal, num_tests = 100000, dice = six_sided_dice):
    best_dice_roll = 0
    success_rate = 0
    t=1
    while t<=6:
        k, total=1, 0
        while k<num_tests:
            c= (roll_dice(t, dice)>=num_from_goal)
            total +=c
            k+=1
        avg_success, num_dice =  total/num_tests, t
        if avg_success > success_rate:
            success_rate, best_dice_roll = avg_success, t
        t+=1
    return [success_rate, best_dice_roll]

def test_all_goals(min_distance, max_distance, num_tests = 100000, dice = six_sided_dice):
    k= min_distance
    while k<=max_distance:
        c=goal_test(k, num_tests, dice)
        print(k, c)
        k +=1

def test_4_sided_dice(min_val, max_val, num_tests = 1000000):
    k = min_val
    while k <= max_val:
        t, total = 1, 0
        while t<= num_tests:
            total += roll_dice(k,four_sided_dice)
            t+=1
        avg_roll, num_dice = total/num_tests, k
        k+=1
        print('rolling ', num_dice, ', avg= ', avg_roll)
        

def test_6_sided_dice(min_val, max_val, num_tests = 1000000):
    k = min_val
    while k <= max_val:
        t, total = 1, 0
        while t<= num_tests:
            total += roll_dice(k)
            t+=1
        avg_roll, num_dice = total/num_tests, k
        k+=1
        print('rolling ', num_dice, ', avg= ', avg_roll)
'''

@main
def run(*args):
    """Read in the command-line argument and calls corresponding functions.

    This function uses Python syntax/techniques not yet covered in this course.
    """
    import argparse
    parser = argparse.ArgumentParser(description="Play Hog")
    # parser.add_argument('--take_turn_test', '-t', action='store_true')
    # parser.add_argument('--play_interactively', '-p', action='store_true')
    # parser.add_argument('--play_basic', '-b', action='store_true')
    parser.add_argument('--run_experiments', '-r', action='store_true')
    parser.add_argument('--final_strategy_test', '-f', action='store_true')
    args = parser.parse_args()
    for name, execute in args.__dict__.items():
        if execute:
            globals()[name]()

