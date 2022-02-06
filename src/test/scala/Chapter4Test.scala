import org.scalatest.flatspec.AnyFlatSpec

import fpinscala._

class Chapter4Test extends AnyFlatSpec {

  val some = Some(1)

  // "Option.map" should "apply function over value" in {
  //   assert(some.map(_ + 1) == Some(2))
  // }

  "Option.flatMap" should "apply flatMap" in {
    assert(some.flatMap(a => Some(a)) == Some(1))
  }

  "Option.filter" should "filter values" in {
    assert(some.filter(a => a != 1) == None)
    assert(some.filter(a => a % 2 == 1) == Some(1))
  }
}
