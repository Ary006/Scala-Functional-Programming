// Core Part 1 about the 3n+1 conjecture
//============================================

object C1 
{


  // ADD YOUR CODE BELOW
  //======================


  //(1) 
  def collatz(n: Long) : Long = 
  {
    @annotation.tailrec
    def helper(current: Long, steps: Long): Long = 
    {
      if (current == 1) steps
      else if (current % 2 == 0) helper(current / 2, steps + 1)
      else helper(3 * current + 1, steps + 1)
    }
    helper(n, 0)
  }

  //(2) 
  def collatz_max(bnd: Long) : (Long, Long) = 
  {
    (1L to bnd).foldLeft((0L, 0L)) { case ((maxSteps, maxNum), n) =>
      val steps = collatz(n)
      if (steps > maxSteps) (steps, n) else (maxSteps, maxNum)
    }
  }

  //(3)
  def is_pow_of_two(n: Long) : Boolean =
  {
    n > 0 && (n & (n - 1)) == 0
  }

  def is_hard(n: Long) : Boolean = 
  {
    is_pow_of_two(3 * n + 1)
  }

  def last_odd(n: Long) : Long = 
  {
    def helper(current: Long, lastOdd: Long): Long = 
    {
      if (is_pow_of_two(current)) lastOdd
      else if (current % 2 == 0) helper(current / 2, lastOdd)
      else helper(3 * current + 1, current)
    }
      
    helper(n, n)
  }

}