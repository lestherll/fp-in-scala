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
        case Nil => acc
        case Cons(head, Nil) => acc
        case Cons(head, tail) => loop(tail, List.append(acc, List(head)))
      }
      loop(l, Nil)
    }

    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x,xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x,xs) => x * product(xs)
    }

    def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }


    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
  
  }
}

