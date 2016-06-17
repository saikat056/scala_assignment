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
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }


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
    val s5 = singletonSet(5)
    val s6 = singletonSet(6)
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

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersect contains the common elements of each set") {
    new TestSets {
      val u1 = union(s1, s2)
      val u2 = union(s1, s3)
      val in = intersect(u1, u2)
      assert(contains(in, 1), "Intersect 1")
      assert(!contains(in, 2), "Intersect 2")
      assert(!contains(in, 3), "Intersect 3")
    }
  }

  test("diff contains the elements of set 's' not present in set 't'") {
    new TestSets {
      val u1 = union(s1, s2)
      val u2 = union(s1, s3)
      val dif = diff(u1, u2)
      assert(contains(dif, 2), "Diff 1")
      assert(!contains(dif, 1), "Diff 2")
      assert(!contains(dif, 3), "Diff 3")
    }
  }

  test("filter contains the elements of set 's' which satisfies predicate 'p'") {
    new TestSets {
      val u1 = union(s1, s2)
      val u2 = union(u1, s3)
      val filter_even = filter(u2, x =>x%2 == 0)
      val filter_odd = filter(u2, x =>x%2 == 1)
      val filter_gt_one = filter(u2, x =>x > 1)
      val filter_lt_three = filter(u2, x =>x < 3)

      assert(contains(filter_even, 2), "Filter 1")
      assert(!contains(filter_even, 1), "Filter 2")
      assert(!contains(filter_even, 3), "Filter 3")

      assert(!contains(filter_odd, 2), "Filter 4")
      assert(contains(filter_odd, 1), "Filter 5")
      assert(contains(filter_odd, 3), "Filter 6")

      assert(contains(filter_gt_one, 2), "Filter 7")
      assert(!contains(filter_gt_one, 1), "Filter 8")
      assert(contains(filter_gt_one, 3), "Filter 9")

      assert(contains(filter_lt_three, 2), "Filter 10")
      assert(contains(filter_lt_three, 1), "Filter 11")
      assert(!contains(filter_lt_three, 3), "Filter 12")
    }
  }


  test("forall checks whether the elements of set 's' satisfies predicate 'p'") {
    new TestSets {
      val bound = 1000
      val u1 = union(s1, s2)
      val u2 = union(u1, s3)

      assert(forall(u2, x => x>=1 && x<=3), "forall 1")
      assert(!forall(u2, x => x%2==0), "forall 2")
    }
  }


  test("exists checks whether there is at least one elem in set 's' that satisfies predicate 'p'") {
    new TestSets {
      val bound = 1000
      val u1 = union(s1, s2)
      val u2 = union(u1, s3)

      assert(exists(u2, x => x>=1 && x<=3), "forall 1")
      assert(exists(u2, x => x%2==0), "forall 2")
      assert(!exists(u2, x => x==4), "forall 2")
    }
  }


  test("map transforms a set 's' by applying `f` to each element of `s`") {
    new TestSets {
      val bound = 1000
      val u1 = union(s1, s2)
      val u2 = union(u1, s3)
      val map1 = map(u2, x => x+3)

      assert(contains(map1,4), "map 1")
      assert(contains(map1,5), "map 2")
      assert(contains(map1,6), "map 3")
    }
  }

}
