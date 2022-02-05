import org.scalatest.flatspec.AnyFlatSpec

import fpinscala.Chapter3._

class Chapter3Test extends AnyFlatSpec {

  val l_string = List("a")
  val l_int = List(1)
  val l = List(1, 2, 3, 4, 5)

  "Exercise 3.1" should "result to 3" in {
    val x = l match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) =>x+y
      case Cons(h, t) => h + List.sum(t)
      case _ => 101
    }
    assert(x == 3)
  }

  "List" should "not be equal if type params are not the same" in {
    assert(l_string != l_int)
  }

  it should "contain the same end tail" in {
    assert(List.tail(l_string) == List.tail(l_int))
  }

  it should "contain the same end tail even when type params are different" in {
    assert(List.tail(l_string) == List.tail(l_int))
  }

  "List.setHead" should "replace the head value of the list" in {
    val new_list = List.setHead(l_string, "ghi")
    assert(new_list != l)
  }

  "List.drop" should " equal to List.tail if n=1" in {
    assert(List.drop(l, 1) == List.tail(l))
  }

  it should "drop first n values" in {
    assert(List.drop(l, 3) == List(4, 5))
  }

  "List.dropWhile" should "drop values as long as the predicate is matched" in {
    assert(List.dropWhile(l, (x: Int) => x < 3) == List(3, 4, 5))
  }

  "List.init" should "remove the last element" in {
    assert(List.init(l) == List(1, 2, 3, 4))
  }

  it should "be Nil if length of list is 1" in {
    assert(List.init(l_int) == Nil)
  }

  it should "be Nil if list is empty" in {
    assert(List.init(Nil) == Nil)
  }
}
