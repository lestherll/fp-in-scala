import org.scalatest.flatspec.AnyFlatSpec

import fpinscala.Chapter3._

class Chapter3Test extends AnyFlatSpec {

  "Exercise 3.1" should "result to 3" in {
    val x = List(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) =>x+y
      case Cons(h, t) => h + List.sum(t)
      case _ => 101
    }
    assert(x == 3)
  }

  "Nil" should "equal to Nil" in {
    assert(Nil == Nil)
  }

  "List that contain different types" should "not be equal" in {
    assert(List("a") != List(1))
  }

  it should "contain the same end tail" in {
    val list_with_string = List(1)
    val list_with_int = List(1)
    assert(List.tail(list_with_string) == List.tail(list_with_int))
  }

  it should "contain the same end tail even when types are different" in {
    val list_with_string = List("a")
    val list_with_int = List(1)
    assert(List.tail(list_with_string) == List.tail(list_with_int))
  }

  "setHead" should "replace the head value of the list" in {
    val l = List("abc", "def")
    val new_list = List.setHead(l, "ghi")
    assert(new_list != l)
  }

  "List.drop with n=1" should " equal to List.tail" in {
    val l = List(1, 2, 3, 4, 5)
    assert(List.drop(l, 1) == List.tail(l))
  }

  "List.drop" should "drop first n values" in {
    val l = List(1, 2, 3, 4, 5)
    assert(List.drop(l, 3) == List(4, 5))
  }

  "List.drop while" should "drop values as long as the predicate is matched" in {
    val l = List(1, 2, 3, 4, 5)
    assert(List.dropWhile(l, (x: Int) => x < 3) == List(3, 4, 5))
  }
}
