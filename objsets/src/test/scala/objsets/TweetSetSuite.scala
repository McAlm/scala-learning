package objsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TweetSetSuite extends FunSuite {
  trait TestSets {
    val set1 = new Empty
    val set2 = set1.incl(new Tweet("a", "a body", 20))
    val set3 = set2.incl(new Tweet("b", "b body", 20))
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)
    val set4c = set3.incl(c)
    val set4d = set3.incl(d)
    val set5 = set4c.incl(d)
  }

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  def size(set: TweetSet): Int = asSet(set).size

  test("filter: on empty set") {
    new TestSets {
      assert(size(set1.filter(tw => tw.user == "a")) === 0)
    }
  }

  test("filter: a on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.user == "a")) === 1)
    }
  }

  test("filter: 20 on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.retweets == 20)) === 2)
    }
  }

  test("union: set4c and set4d") {
    new TestSets {
      assert(size(set4c.union(set4d)) === 4)
    }
  }

  test("union: with empty set (1)") {
    new TestSets {
      assert(size(set5.union(set1)) === 4)
    }
  }

  test("union: with empty set (2)") {
    new TestSets {
      assert(size(set1.union(set5)) === 4)
    }
  }

  test("descending: set5") {
    new TestSets {
      val trends = set5.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.user == "a" || trends.head.user == "b")
    }
  }

  test("descending: expanded set5") {
    new TestSets {
      val tw1 = new Tweet("x", "x body", 35)
      val tw2 = new Tweet("y", "y body", 258)
      val tw3 = new Tweet("z", "z body", 11)
      val set6 = set5.incl(tw1).incl(tw2).incl(tw3)
      val trends = set6.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.user == "y")
      assert(trends.tail.head.user == "x")
    }
  }

  test("descending: most expanded set5") {
    val tw_w = new Tweet("w", "w body", 27)
    val tw_x = new Tweet("x", "x body", 35)
    val tw_y = new Tweet("y", "y body", 258)
    val tw_z = new Tweet("z", "z body", 11)
    val tw_a = new Tweet("a", "a body", 31)
    val tw_b = new Tweet("b", "b body", 31)
    val tw_c = new Tweet("c", "c body", 31)
    val tw_e = new Tweet("e", "e body", 42)
    val tw_f = new Tweet("f", "f body", 274)
    val tw_g = new Tweet("g", "g body", 65)
    val tw_h = new Tweet("h", "h body", 64)
    val tw_k = new Tweet("k", "k body", 0)
    val tw_l = new Tweet("l", "l body", 117)

    val firstSet = new Empty

    val setAll = firstSet.incl(tw_w).incl(tw_x).incl(tw_z).incl(tw_a).incl(tw_g).incl(tw_f).incl(tw_l).incl(tw_k).incl(tw_h).incl(tw_b).incl(tw_a).incl(tw_b).incl(tw_y)

    val trends = setAll.descendingByRetweet
    assert(!trends.isEmpty)
    assert(trends.head.user == "f")
    assert(trends.tail.head.user == "y")

  }

  test("filterAcc returns empty set for set1") {
    new TestSets {
      def testfunc(tw: Tweet) = tw.user == "a"
      val accumulated = set1.filterAcc(testfunc, new Empty)
      assert(size(accumulated) == 0)

    }
  }

  test("filterAcc returns non empty set for set2") {
    new TestSets {
      def testfunc(tw: Tweet) = tw.user == "a"
      val accumulated = set2.filterAcc(testfunc, new Empty)
      assert(size(accumulated) === 1)
    }
  }

  test("filterAcc returns non empty set for set5") {
    new TestSets {
      def testfunc(tw: Tweet) = tw.user == "a" || tw.user == "b" || tw.user == "c" || tw.user == "x"
      val accumulated = set5.filterAcc(testfunc, new Empty)
      assert(size(accumulated) === 3)
    }
  }

  test("getMaxRetweets returns 20 for set5") {
    new TestSets {
      assert(set5.mostRetweeted.retweets === 20)
    }
  }

  test("getMaxRetweets returns 35 for expanded set5") {
    new TestSets {
      val maxRetweeted = new Tweet("x", "x body", 35)
      val set6 = set5.incl(maxRetweeted)
      assert(set6.mostRetweeted.retweets === 35)
    }
  }
}
