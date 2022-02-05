package fpinscala

object Chapter3 {
  
  trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {

    // Exercise 3.2
    def tail[A](list: List[A]) = list match {
      case Nil => Nil
      case Cons(_, t) => t
    }

    // Exercise 3.3
    def setHead[A](l: List[A], nh: A) = l match {
      case Cons(_, t) => Cons(nh, t)
      case Nil => sys.error("setting head on empty list")
    }

    // Exercise 3.4 - generalising tail
    def drop[A](l: List[A], n: Int): List[A] = {
      if (n <= 0) l
      else l match {
        case Nil => Nil
        case Cons(_, t) => drop(t, n-1)
      }
    }

    // Exercise 3.5 - dropWhile
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
      case Nil => Nil
      case Cons(head, tail) => 
        if (f(head)) dropWhile(tail, f)
        else l
    }

    // Exercise 3.6 - all elements except the last 
    def init[A](l: List[A]): List[A] = {
      def loop[A](l: List[A], acc: List[A]): List[A] = l match {
        case Nil => Nil
        case Cons(head, Nil) => acc
        case Cons(head, tail) => loop(tail, List.append(acc, List(head)))
      }
      loop(l, Nil)
    }

    // Exercise 3.7 - No not possible because all arguments are evaluated before function is called


    // Exercise 3.8 - 
    // List.foldRight(List(1, 2, 3), Nil:List[Int])(Cons(_, _))
    // We get back the same list because Cons(_, _) links the current head
    // and z

    // Exercise 3.9 - length using foldRight
    def length[A](l: List[A]): Int =
      foldRight(l, 0)((_, n) => n + 1)

    // Exercise 3.10 - foldLeft
    @annotation.tailrec
    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }

    // Exercise 3.11 - use folLeft to create sumLeft, productLeft, lengthLeft
    // sumLeft
    def sumLeft(l: List[Int]) = foldLeft(l, 0)(_ + _)

    // productLeft
    def productLeft(l: List[Double]) = foldLeft(l, 1.0)(_ * _)

    // lengthLeft
    def lengthLeft[A](l: List[A]) = foldLeft(l, 0)((n, _) => n + 1)

    // Exercise 3.12 - reverse using a fold
    def reverse[A](l: List[A]) = foldLeft(l, Nil: List[A])((x, y) => Cons(y, x))


    // Exercise 3.13 
    // foldLeft in terms of foldRight
    // needs notes
    def foldLeftViaFR[A, B](l: List[A], z: B)(f: (B, A) => B): B =
      foldRight(l, (b: B) => b)((x, g) => b => g(f(b, x)))(z)

    // foldRight in terms of foldLeft
    def foldRightViaFL[A, B](l: List[A], z: B)(f: (A, B) => B): B =
      foldLeft(l, (b: B) => b)((g, x) => b => g(f(x, b)))(z)


    // reduce
    def reduce[A](l: List[A])(f: (A, A) => A) = l match {
      case Cons(h, t) => foldLeft(t, h)(f)
      case Nil => sys.error("reduce on empty list")
    }

    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x,xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(h, t) => h * product(t)
    }

    def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

    def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

    def sum2(ns: List[Int]) =
      foldRight(ns, 0)((x,y) => x + y)

    def product2(ns: List[Double]) =
      foldRight(ns, 1.0)(_ * _)

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
  
  }
}

