package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  test("adding ints") {
    assert(1 + 2 === 3)
  }

  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s4 = singletonSet(4)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  trait UnionTestSets extends TestSets {
    val u123 = union(union(s1, s2), s3)
    val u234 = union(union(s2, s3), s4)
  }

  test("intersect contains only the intersection of [2, 3]") {
    new UnionTestSets {
      val intersected = intersect(u123, u234)
      assert(contains(intersected, 2), "intersect 2")
      assert(contains(intersected, 3), "intersect 3")
      assert(!contains(intersected, 1), "intersect 1")
      assert(!contains(intersected, 4), "intersect 4")
    }
  }

  test("diff contains only [1]") {
    new UnionTestSets {
      val diffSet = diff(u123, u234)
      assert(contains(diffSet, 1), "diff 1")
      assert(!contains(diffSet, 2), "diff 2")
      assert(!contains(diffSet, 3), "diff 3")
      assert(!contains(diffSet, 4), "diff 4")
    }
  }

  test("diff contains only [4]") {
    new UnionTestSets {
      val diffSet = diff(u234, u123)
      assert(contains(diffSet, 4), "diff 4")
      assert(!contains(diffSet, 2), "diff 2")
      assert(!contains(diffSet, 3), "diff 3")
      assert(!contains(diffSet, 1), "diff 1")
    }
  }

  test("filter contains only even numbers") {
    new UnionTestSets {

      val u1234 = union(u123, u234)
      val filtered = filter(u1234, n => n % 2 == 0)

      assert(contains(filtered, 2), "filter does not contain 2")
      assert(contains(filtered, 4), "filter does not contain 4")
      assert(!contains(filtered, 1), "filter  must not contain 1")
      assert(!contains(filtered, 3), "filter must not contain 3")
      assert(!contains(filtered, 200), "filter must not contain 200")
    }
  }

  test("filter contains only odd numbers") {
    new UnionTestSets {

      val u1234 = union(u123, u234)
      val filtered = filter(u1234, n => n % 2 != 0)

      assert(contains(filtered, 1), "filter  does not contain 1")
      assert(contains(filtered, 3), "filter does not contain 3")
      assert(!contains(filtered, 2), "filter contains 2")
      assert(!contains(filtered, 4), "filter contains 4")
    }
  }

  test("forall") {
    new UnionTestSets {
      val u1234 = union(u123, u234)
      assert(forall(u1234, n => n > 0))
      assert(forall(u1234, n => n < 0 || n > 0))
      assert(!forall(u1234, n => n % 2 == 0))
      
      val filtered = filter(u1234, n => n % 2 == 0)
      assert(forall(filtered, n => n % 2 == 0))
    }
  }
  
  
  test("exists"){
    new UnionTestSets{
      val u1234 = union(u123, u234)
      assert(exists(u1234, n => n > 0), "no number > 0 ")
      assert(exists(u1234, n => n % 2 == 0), "no eval number")
      assert(exists(u1234, n => n % 2 != 0), "no odd number ")
      assert(!exists(u1234, n => n < 0), "number < 0 contained")
      assert(!exists(u1234, n => n > 5), "number > 5 contained")
      
    }
  }

  test("map"){
    
    new UnionTestSets {
      val u1234 = union(u123, u234)
      val transformed = map(u1234, x => x * 2)
      assert(contains(transformed, 2))
      assert(contains(transformed, 4))
      assert(contains(transformed, 6))
      assert(contains(transformed, 8))
      
      assert(!contains(transformed, 1))
      assert(!contains(transformed, 3))
      assert(!contains(transformed, 5))
      assert(!contains(transformed, 7))
    }
  }

  test("map {1, 3, 4, 5, 7, 1000}"){
    
    new UnionTestSets {
      val u1234 = union(u123, u234)
      val all = union(union(union(u1234, singletonSet(5)), singletonSet(7)), singletonSet(1000))
      
      val transformed = map(all, x => x -1)
      assert(contains(transformed, 0))
      assert(contains(transformed, 2))
      assert(contains(transformed, 3))
      assert(contains(transformed, 4))
      assert(contains(transformed, 6))
      assert(contains(transformed, 999))
      
    }
  }
}