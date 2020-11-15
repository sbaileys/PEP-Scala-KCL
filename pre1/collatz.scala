// Preliminary Part about the 3n+1 conjecture
//============================================

object CW6a {

//(1) Complete the collatz function below. It should
//    recursively calculate the number of steps needed 
//    until the collatz series reaches the number 1.
//    If needed, you can use an auxiliary function that
//    performs the recursion. The function should expect
//    arguments in the range of 1 to 1 Million.

def collatz(n: Long) : Long = {
    collatz_rec(n, 0)
}

def collatz_rec(n: Long, steps: Int) : Long = {
    if(n==1) steps
    else if((n%2)==0) collatz_rec(n/2, 1 + steps)
    else collatz_rec(3*n + 1, 1 + steps)
}

//(2) Complete the collatz_max function below. It should
//    calculate how many steps are needed for each number 
//    from 1 up to a bound and then calculate the maximum number of
//    steps and the corresponding number that needs that many 
//    steps. Again, you should expect bounds in the range of 1
//    up to 1 Million. The first component of the pair is
//    the maximum number of steps and the second is the 
//    corresponding number.

def collatz_max(bnd: Long) : (Long, Long) = {
    val lst = for (i <- 1L to bnd) yield collatz(i)
    val max_col = lst.max
    (max_col, lst.indexOf(max_col)+1)
}

//(3) Implement a function that calculates the last_odd
//    number in a collatz series.  For this implement an
//    is_pow_of_two function which tests whether a number 
//    is a power of two. The function is_hard calculates 
//    whether 3n + 1 is a power of two. Again you can
//    assume the input ranges between 1 and 1 Million,
//    and also assume that the input of last_odd will not 
//    be a power of 2.

def is_pow_of_two(n: Long) : Boolean = {
    (n & (n - 1)) == 0
}

def is_hard(n: Long) : Boolean = {
    is_pow_of_two((3*n)+1)
}

// def last_odd(n: Long) : Long = {
//     last_odd_rec(n, 1)
// }

// def last_odd_rec(n : Long, odd_num : Long) : Long = {
//     if (n==1) odd_num 
//     else if (n%2==0) last_odd_rec(n/2, odd_num)
//     else {
//         if(is_hard(n)) last_odd_rec((3*n)+1, n)
//         else last_odd_rec((3*n)+1, odd_num)
//     }
// }

def last_odd(n: Long) : Long = {
    def getLastOdd(n : Long, lastOdd : Long) : Long = {
        if (n == 1) lastOdd
        else if (n % 2 == 0) getLastOdd(n/2, lastOdd)
        else {
            if (is_hard(n)) getLastOdd(3 * n + 1, n)
            else getLastOdd(3*n+1, lastOdd)
        }
    } 
    getLastOdd(n, 1)
}
}