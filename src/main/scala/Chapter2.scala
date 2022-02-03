object Chapter2 {

  // exercise 2.1 Fibonacci tail recursive
  def fibonacci(n: Int) = {
    def loop(a: Int, b: Int, n: Int): Int = 
      if (n > 0) loop(b, a+b, n-1)
      else a
    loop(0, 1, n-1)    
  }

}