import org.scalatest.flatspec.AnyFlatSpec
import fpinscala.Chapter2._

class Chapter2Test extends AnyFlatSpec {
  "fibonacci(10)" should "be 55" in {
    assert(fibonacci(10) == 55)
  }

  it should "not fail for big numbers eg n=9999" in {
    assert(fibonacci(9999) == 890489442)
  }
}
