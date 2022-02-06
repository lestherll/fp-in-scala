package fpinscala

import org.scalactic.Bool

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

    // Exercise 3.11 - use foldLeft to create sumLeft, productLeft, lengthLeft
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

    // Exercise 3.14 - append in foldLeft or foldRight
    def appendViaFL[A](l: List[A], r: List[A]) =
      foldLeft(List.reverse(l), r)((x, y) => Cons(y, x))

    def appendViaFR[A](l: List[A], r: List[A]) = 
      foldRightViaFL(l, r)(Cons(_, _))

    // Exercise 3.15 - concat a list
    def concat[A](l: List[List[A]]): List[A] =
      foldRightViaFL(l, Nil:List[A])(append)

    // Exercise 3.16 - add 1 to each elem
    def addOne(l: List[Int]) =
      foldRightViaFL(l, Nil: List[Int])((x, y) => Cons(x+1, y))

    // Exercise 3.17 - doubleToString
    def doubleToString(l: List[Double]): List[String] =
      foldRightViaFL(l, Nil: List[String])((x, y) => Cons(x.toString, y))

    // Exercise 3.18 - map function
    def map[A, B](l: List[A])(f: A => B): List[B] =
      foldRightViaFL(l, Nil: List[B])((x, y) => Cons(f(x), y))

    // Exercise 3.19 - filter
    def filter[A](l: List[A])(f: A => Boolean): List[A] =
      foldRightViaFL(l, Nil: List[A])((x, y) => if (f(x)) Cons(x, y) else y)

    // Exercise 3.20 - flatMap
    def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
      foldRightViaFL(l, Nil: List[B])((x, y) => append(f(x), y))

    // Exercise 3.21 - filter using flatMap
    def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
      flatMap(l)(x => if (f(x)) List(x) else Nil)

    // Exercise 3.22
    def zipAdd(l: List[Double], r: List[Double]): List[Double] = (l, r) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, zipAdd(t1, t2)) 
    }

    // Exercise 3.23
    def zipWith[A, B, C](l: List[A], r: List[B])(f: (A, B) => C): List[C] = (l, r) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    }

    // Exercise 3.24 - subsequence
    def hasSubseq[A](l: List[A], sub: List[A]): Boolean = {
      @annotation.tailrec
      def loop(l: List[A], curr: List[A]): Boolean = (l, curr) match {
        case (Nil, _) => curr == Nil
        case (_, Nil) => true
        case (Cons(h1, t1), Cons(h2, t2)) => if (h1 == h2) loop(t1, t2) else loop(t1, sub)
      }
      loop(l, sub)
    }

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


  // TREES
  sealed trait Tree[+A]
  case class Leaf[A](v: A) extends Tree[A]
  case class Branch[A](l: Tree[A], r: Tree[A]) extends Tree[A]

  object Tree {
    // Exercise 3.25 - size
    def size[A](t: Tree[A]): Int = t match {
      case Branch(l, r) => 1 + size(l) + size(r)
      case Leaf(v) => 1
    }

    // Exercise 3.26 - max
    def max(t: Tree[Int]): Int = t match {
      case Leaf(v) => v
      case Branch(l, r) => max(l) max max(r)
    }

    // Exercise 3.27 - depth
    def depth[A](t: Tree[A]): Int = t match {
      case Leaf(_) => 0
      case Branch(l,r) => 1 + (depth(l) max depth(r))
    }

    // Exercise 3.28 - map
    def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
      case Leaf(v) => Leaf(f(v))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }

    // Exercise 3.29 - fold and implementations of size, max, depth, map using fold
    def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
      case Leaf(v) => f(v)
      case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }

    def sizeViaFold[A](t: Tree[A]) =
      fold(t)(x => 1)(1 + _ + _)

    def maxViaFold(t: Tree[Int]) =
      fold(t)(x => x)(_ max _)

    def depthViaFold[A](t: Tree[A]) =
      fold(t)(x => 0)((x, y) => 1 + (x max y))

    def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] =
      fold(t)(x => Leaf(f(x)): Tree[B])(Branch(_, _))
  }
}

