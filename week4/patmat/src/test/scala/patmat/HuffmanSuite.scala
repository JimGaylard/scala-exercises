package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("time counts incidents of char in string") {
    assert(times(string2Chars("aabccc")) === List(('a', 2), ('b', 1), ('c', 3)))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("singleton list tests for single leaf") {
    assert(singleton(List(Leaf('a', 1))))
    new TestTrees {
      assert(singleton(List(t2)))
    }
  }

  test("singleton list is true for large trees") {
    new TestTrees {
      assert(singleton(List(t2)))
    }
  }

  test("singleton list is false for multiple trees") {
    new TestTrees {
      assert(!singleton(List(t2, t1)))
    }
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("until recurses over a list of CodeTree") {
    new TestTrees {
      val trees: List[Leaf] = makeOrderedLeafList(List(('a', 2), ('b', 3), ('d', 4)))
      assert(until(singleton, combine)(trees) == List(t2))
    }
  }

  test("createCodeTree creates CodeTree from a list of Char") {
    new TestTrees {
      val chars: List[Char] = string2Chars("adbdbdadb")
      assert(createCodeTree(chars) == t2)
    }
  }

  test("decode gets the job done") {
    new TestTrees {
      assert(decode(t2, List(1, 0, 0, 0, 1)) == "dab".toList)
    }
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }

  test("decoded secret is secret") {
    assert(decodedSecret == "huffmanestcool".toList)
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

}
