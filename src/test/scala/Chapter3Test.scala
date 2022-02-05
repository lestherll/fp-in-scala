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

  "List.length" should "count the number of elements" in {
    assert(List.length(l) == 5)
  }

  it should "return 0 for Nil or List()" in {
    assert(List.length(Nil) == 0)
    assert(List.length(List()) == 0)
  }

  "List.foldLeft" should "return the total with add function" in {
    assert(List.foldLeft(l, 0)((x, y) => x+y) == 15)
  }

  "List.sumLeft" should "return sum of the list elements" in {
    assert(List.sumLeft(l) == 15)
  }

  "List.reverse" should "reverse the list passed" in {
    assert(List.reverse(l) == List(5, 4, 3, 2, 1))
  }

  "List.foldLeftViaFR" should "fold" in {
    assert(List.foldLeftViaFR(l, 1)(_ * _) == 120)
  }

  "List.foldRightViaFL" should "fold" in {
    assert(List.foldRightViaFL(l, 1)(_ * _) == 120)
  }

  "List.reduce" should "reduce the elements given f" in {
    assert(List.reduce(l)(_ + _) == 15)
  }

  it should "produce Nil when list is empty" in {
    assertThrows[RuntimeException](List.reduce(Nil: List[Int])(_ + _))
  }

  "List.appendViaFL" should "append correctly" in {
    assert(List.appendViaFL(l, List(6, 7)) == List(1, 2, 3, 4, 5, 6, 7))
  }

  "List.appendViaFR" should "append correctly" in {
    assert(List.appendViaFR(l, List(6, 7)) == List(1, 2, 3, 4, 5, 6, 7))
  }

  "List.concat" should "concatenate list of lists" in {
    assert(List.concat(List(l, l, l)) == List(1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5))
  }

  "List.addOne" should "add one to each element" in {
    assert(List.addOne(l) == List(2, 3, 4, 5, 6))
  }  
}
