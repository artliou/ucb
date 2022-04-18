from lab03 import *

# Q10
def summation(n, term):
    """Return the sum of the first n terms of a sequence.

    >>> summation(5, lambda x: pow(x, 3))
    225
    """
    total, k = 0, 1
    while k <= n:
        total, k = total + term(k), k + 1
    return total

def interleaved_sum(n, odd_term, even_term):
    """Compute the sum odd_term(1) + even_term(2) + odd_term(3) + ..., up
    to n.

    >>> # 1 + 2^2 + 3 + 4^2 + 5
    ... interleaved_sum(5, lambda x: x, lambda x: x*x)
    29
    """
    "*** YOUR CODE HERE ***"
    return interleaved_helper(n, odd_term, even_term, 1)

def interleaved_helper(n, term0, term1, k):
    if k == n:
        return term0(k)
    return term0(k) + interleaved_helper(n, term1, term0, k + 1)

    """def is_even(n):
        if n % 2 == 0:
            return True
        return False

    if is_even(n) == True:
        return n + interleaved_sum(n+1, odd_term, even_term)
    else:
        return n*n + interleaved_sum(n+1, odd_term, even_term)"""