package recfun

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PascalSuite extends FunSuite {
  import Main.pascal

    test("pascal: col=0,row=2") {
    assert(pascal(0,2) === 1)
  }

  test("pascal: col=1,row=2") {
    assert(pascal(1,2) === 2)
  }

  test("pascal: col=1,row=3") {
    assert(pascal(1,3) === 3)
  }
  
  test("pascal: col=0,row=1") {
    assert(pascal(0, 1) === 1)
  }
  test("pascal: col=1,row=1") {
    assert(pascal(1, 1) === 1)
  }

  test("pascal: col=2,row=3") {
    assert(pascal(2, 3) === 3)
  }


  test("assert column = 0 should always be 1") {
    for (row <- 0 to 10) {
      assert(pascal(0, row) === 1);
    }
  }

  test("assert last element in row should always be 1") {
    for (colEqrow <- 0 to 10) {
      assert(pascal(colEqrow, colEqrow) === 1);
    }
  }
}
