object Chapter2 {

  // exercise 2.1 Fibonacci tail recursive
  def fibonacci(n: Int) = {
    def loop(a: Int, b: Int, n: Int): Int = 
      if (n > 0) loop(b, a+b, n-1)
      else a
    loop(0, 1, n-1)    
  }


  // Listing 2.3 - Monomorphic function to find a String in an array
  // def findFirst(ss: Array[String], key: String): Int = {
  //   @annotation.tailrec
  //   def loop(n: Int): Int =
  //     if (n >= ss.length) -1
  //     else if (ss(n) == key) n
  //     else loop(n + 1)

  //   loop(0)
  // }
  // We can make the function above "polymorphic/generic" by generalising the type requirements. 
  // For example, the algorithm would still be the same as long as the container is a sequence
  // and the elements are of the same type as the key and they have an "equality feature"

  // Listing 2.4 - Polymorphic function to find an element in an array
  // def findFirst[A](as: Array[A], p: A => Boolean): Int = {
  //   @annotation.tailrec
  //   def loop(n: Int): Int =
  //     if (n >= as.length) -1
  //     else if (p(as(n))) n
  //     else loop(n + 1)
  //   loop(0)
  // }
  // [A] is called a type parameter like generics in Java, it can ve reference in the rest of the 
  // type signature the type variable A is reference in 2 places and that is inside `as` the array
  // and as `p`'s parameter This makes becuase finding values in a container where the key does 
  // not have the same type will always not work (edge cases are ignore for clarity)

  // Exercise 2.2 - implement an isSorted function that checks whether an Array[A] is sorted
  // according to a given comparison function
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean =
      if (n >= as.length-1) true
      else if (ordered(as(n), as(n+1))) false
      else loop(n + 1)
    loop(0)
  }

  // Exercise 2.3 - Currying
  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    a => b => f(a, b)

  // Exercise 2.4 - Uncurry
  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  // Exercise 2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C =
    a => f(g(a))

}