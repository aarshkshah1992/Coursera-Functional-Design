package quickcheck

import common._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

import scala.annotation.tailrec


abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = {
    for {
      size <- Gen.choose(2, 1000)
      numbers <- Gen.listOfN(size, arbInt.arbitrary)
    } yield {
      numbers.foldLeft(empty)((heap, num) => insert(num, heap))
    }
  }


  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("gen2") = forAll {
    (a: Int, b: Int) =>
      val minNum = a min b
      findMin(insert(b, insert(a, empty))) == minNum
  }


  property("gen3") = forAll {
    (a: Int) =>
      isEmpty(deleteMin(insert(a, empty)))
  }

  property("gen4") = forAll { (h1: H, h2: H) =>
    if (isEmpty(h1) && isEmpty(h2)) true
    else if (isEmpty(h1)) {
      val minNum = findMin(h2)
      findMin(meld(h1, h2)) == minNum
    }
    else if (isEmpty(h2)) {
      val minNum = findMin(h1)
      findMin(meld(h1, h2)) == minNum
    }
    else {
      val min1 = findMin(h1)
      val min2 = findMin(h2)
      val minNum = findMin(meld(h1, h2))
      minNum == (min1 min min2)
    }

  }

  property("gen5") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  @tailrec
  final def getList(currHeap: H, acc: List[Int]): List[Int] = {
    if (isEmpty(currHeap)) acc.reverse
    else {
      val minNum = findMin(currHeap)
      getList(deleteMin(currHeap), minNum :: acc)
    }
  }

  property("gen6") = forAll { (h: H) =>
    val lst = getList(h, Nil)
    lst == lst.sorted
  }

  property("gen7") = forAll { (h1: H, h2: H) =>
    val combinedHeap = meld(h1, h2)
    val lst1 = getList(h1, Nil)
    val lst2 = getList(h2, Nil)
    val lst = getList(combinedHeap, Nil)
    (lst == lst.sorted) && (lst == ((lst1 ::: lst2).sorted))
  }


}
