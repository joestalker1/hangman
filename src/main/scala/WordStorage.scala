import cats.Monoid

import scala.collection.mutable.ArrayBuffer
import cats.implicits._

/**
 * Store strings in the single linked list with complexity O(1),then sort them by the demand.
 *
 */
case class WordInfo(len: Int, ref: String) extends Product with Serializable

case class WordStorage private(words: List[String] = List.empty) {
  type WordBucket = ArrayBuffer[WordInfo]

  val alphabetSize = 127

  private lazy val wordBuckets: Array[WordBucket] = {
    //buckets has empty items but wasting is tiny
    //TODO start using immutable collections)
    val buckets = Array.ofDim[WordBucket](alphabetSize)
    for (s <- words) {
      val wi = WordInfo(s.length, s)
      val firstChar = wi.ref(0)
      val index = firstChar.toInt
      if (buckets(index) == null) {
        buckets(index) = new ArrayBuffer[WordInfo]()
      }
      buckets(index) += wi
    }
    //sort words by word length inside a bucket
    for (i <- 0 until buckets.length if buckets(i) != null) {
      buckets(i) = buckets(i).sortBy(_.len)
    }
    buckets
  }

  def findByFirstLastCharLen(fromChar: Char, toChar: Char, expectedLen: Int): List[String] = {
    val index = fromChar.toInt
    if (wordBuckets(index) == null) List.empty
    else {
      var start = 0
      var end = wordBuckets(index).size - 1
      val wordList = wordBuckets(index)
      var resList: List[String] = List.empty
      while (start < end) {
        val mid = start + (end - start) / 2
        if (wordList(mid).len == expectedLen) {
          resList = takeNeighbours(wordList)(mid, expectedLen)
          start = end
        } else if (wordList(mid).len < expectedLen) start = mid + 1
        else end = mid - 1
      }
      resList.filter(_ (expectedLen - 1) == toChar)
    }
  }

  private def takeNeighbours(bucket: ArrayBuffer[WordInfo])(index: Int, expectedLen: Int): List[String] = {
    var from = index
    //go forward of the list
    while (from >= 0 && bucket(from).len == expectedLen) from -= 1
    if (from != index) from += 1
    //go back of the list
    var to = index
    while (to < bucket.length && bucket(to).len == expectedLen) to += 1
    if (to != index) to -= 1
    bucket.view(from, to + 1).map(_.ref).toList
  }
}

object WordStorage {

  implicit val wordStorageMonoid: Monoid[WordStorage] = new Monoid[WordStorage] {
    override def empty: WordStorage = WordStorage()

    override def combine(x: WordStorage, y: WordStorage): WordStorage = WordStorage(x.words ++ y.words)
  }
}