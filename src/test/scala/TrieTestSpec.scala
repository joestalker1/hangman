import cats.Id
import org.scalatest._

class TrieTestSpec extends FlatSpec with Matchers with OptionValues with Inside with Inspectors {
  "Trie" should "guess  a word in one string trie" in {
    val trie = Trie()
    trie.insert("recover")

    trie.markAsGuessed('r', 0)
    trie.markAsGuessed('r', 6)

    trie.guess().value should be('e')
    trie.markAsGuessed('e', 5)

    trie.guess().value should be('e')
    trie.markAsGuessed('e', 1)

    trie.guess().value should be('c')
    trie.markAsGuessed('c', 2)

    trie.guess() should be('defined)
  }

  "Trie" should "guess a word from 2 words with common prefix if last char of one word is guessed properly" in {
    val trie = Trie()
    trie.insert("bar")
    trie.insert("bag")

    trie.markAsGuessed('g', 2)
    trie.guess().value should be('b')
    trie.markAsGuessed('b', 0)
    trie.guess().value should be('a')
    trie.markAsGuessed('a', 1)
    trie.guessedString(3) should be("bag")
  }

  "Trie" should "return None if one char of word is marked as missing" in {

    val trie = Trie()
    trie.insert("bar")
    trie.insert("bag")

    trie.markAsMissing('r')

  }

  type Effect[A] = Id[A]

  "Trie" should "guess the word 'recover' if it loaded the all string from a file" in {
    val wordDict = new FileWordDict[Effect](List("test_dict.txt"))
    wordDict.loadDict()
    val words = wordDict.findByFirstLastCharLen('r', 'r', 7)
    val trie = Trie()
    words.foreach(trie.insert(_))
    trie.insert("recover")

    trie.markAsGuessed('r', 0)
    trie.markAsGuessed('r', 6)

    trie.guess().value should be('a')
    trie.markAsMissing('a')

    trie.guess().value should be('e')
    trie.markAsGuessed('e', 1)

    trie.guess().value should be('c')
    trie.markAsGuessed('c', 2)

    trie.guess().value should be('i')
    trie.markAsMissing('i')
  }
}
