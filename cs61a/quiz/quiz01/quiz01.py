def diff(x, y, z):
    """Return whether one argument is the difference between the other two.

    x, y, and z are all integers.

    >>> diff(5, 3, 2) # 5 - 3 is 2
    True
    >>> diff(2, 3, 5) # 5 - 3 is 2
    True
    >>> diff(2, 5, 3) # 5 - 3 is 2
    True
    >>> diff(-2, 3, 5) # 3 - 5 is -2
    True
    >>> diff(-5, -3, -2) # -5 - -2 is -3
    True
    >>> diff(-2, 3, -5) # -2 - 3 is -5
    True
    >>> diff(2, 3, -5)
    False
    >>> diff(10, 6, 4)
    True
    >>> diff(10, 6, 3)
    False
    """
    "*** YOUR CODE HERE ***"
    if x - y == z:
        return True
    elif y - x == z:
        return True
    elif y - z == x:
        return True
    elif z - y == x:
        return True
    elif z - x == y:
        return True
    elif x - z == y:
        return True
    return False
#completed at 1010:23s on Wednesday. Mistake in the 2nd line

def abundant(n):
    """Print all ways of forming positive integer n by multiplying two positive
    integers together, ordered by the first term. Then, return whether the sum
    of the proper divisors of n is greater than n.

    A proper divisor of n evenly divides n but is less than n.

    >>> abundant(12) # 1 + 2 + 3 + 4 + 6 is 16, which is larger than 12
    1 * 12
    2 * 6
    3 * 4
    True
    >>> abundant(14) # 1 + 2 + 7 is 10, which is not larger than 14
    1 * 14
    2 * 7
    False
    >>> abundant(16)
    1 * 16
    2 * 8
    4 * 4
    False
    >>> abundant(20)
    1 * 20
    2 * 10
    4 * 5
    True
    >>> abundant(22)
    1 * 22
    2 * 11
    False
    >>> r = abundant(24)
    1 * 24
    2 * 12
    3 * 8
    4 * 6
    >>> r
    True
    """
    "*** YOUR CODE HERE ***"
    count = n
    total = 0
    b = 1
    original = n
    while count > 0:
        if n % b == 0 and b < n//b:
            print(b, '*', n//b)
            total = total + n//b + b
        elif n%b == 0 and b == n//b:
            print(b, '*', n//b)
            total = total + b
        count = count - 1
        b = b + 1
    if total - original > original:
        return True
    return False
#GOT IT! at 1035pm. All test cases passed :)

def amicable(n):
    """Return the smallest amicable number greater than positive integer n.

    Every amicable number x has a buddy y different from x, such that
    the sum of the proper divisors of x equals y, and
    the sum of the proper divisors of y equals x.

    For example, 220 and 284 are both amicable because
    1 + 2 + 4 + 5 + 10 + 11 + 20 + 22 + 44 + 55 + 110 is 284, and
    1 + 2 + 4 + 71 + 142 is 220

    >>> amicable(5)
    220
    >>> amicable(220)
    284
    >>> amicable(284)
    1184
    >>> r = amicable(5000)
    >>> r
    5020
    """
    "*** YOUR CODE HERE ***"
    #PLAN: Set Function to Sum Proper divisors. Then start a increasing count
    # from n until the sum n) > the original sum, then move on to
    #next positive integer (n+1) and test.
    n = n + 1
    a = sum_of_factors(n) #a is the sum
    while sum_of_factors(n) != a:
        n = n+1
    if sum_of_factors(n) == a and sum_of_factors(a) == n and n != a:
        return n
    else: #sum is greater than current
        return amicable(n)

def sum_of_factors(n):
    y = 1
    total = 0
    while n > y:
        if n % y == 0:
            total = total + y
        y = y + 1
    return total