import scala.collection.mutable.Queue

// I don't want to deal with immutable IndexedSeq.
case class SuffixNode(var children: Array[SuffixNode] = Array.ofDim[SuffixNode](127), var isEndOfWord: Boolean = false, parent: SuffixNode = null)

case class SuffixTree (root: SuffixNode = SuffixNode()) {
  private var guessedChars = Map.empty[Char, Set[Int]]
  private var missingChars = Set.empty[Char]
  private var levelToTrieNode = Map.empty[Int, List[SuffixNode]]

  def insert(s: String): Unit = {
    var crawl = root
    for (level <- 0 until s.length) {
      val index = s(level)
      if (crawl.children(index) == null) {
        crawl.children(index) = SuffixNode(parent = crawl)
        val list = levelToTrieNode.getOrElse(level, List.empty)
        levelToTrieNode = levelToTrieNode + (level -> (crawl.children(index) :: list))
      }
      crawl = crawl.children(index)
    }
    crawl.isEndOfWord = true
  }

  private def charInOtherPosOrNew(char: Char)(pos: Int) = guessedChars.get(char).isEmpty || !guessedChars(char)(pos)

  def markAsGuessed(char: Char, pos: Int): Unit = {
    val set = guessedChars.getOrElse(char, Set.empty[Int])
    guessedChars = guessedChars + (char -> (set + pos))
    // disable all chars at the position except 'guessed' one.
    levelToTrieNode.get(pos).foreach { list =>
      list.foreach { node =>
        val index = char.toInt
        for (i <- 0 until node.parent.children.length) {
          if (i != index) node.parent.children(i) = null
        }
      }
    }
  }

  def markAsMissing(char: Char): Unit = missingChars = missingChars + char

  def guess(): Option[Char] = {
    val queue = Queue.empty[SuffixNode]
    queue.enqueue(root)
    var res: Option[Char] = None
    var pos = 0
    while (queue.nonEmpty && res.isEmpty) {
      val node = queue.dequeue()
      var i = 0
      while (i < node.children.length && res.isEmpty) {
        val char = i.toChar
        if (node.children(i) != null && !missingChars(char)) {
          if (charInOtherPosOrNew(char)(pos)) res = Option(char)
          else queue.enqueue(node.children(i))
        }
        i += 1
      }
      pos += 1
    }
    res
  }

  def guessedString(len: Int): String = {
    val buf = new StringBuilder(len)
    for (_ <- 0 to len - 1) buf.append('_')
    guessedChars.foreach { case (char, set) =>
      set.foreach(buf(_) = char)
    }
    buf.result()
  }

  def guessedSize(): Int = guessedChars.values.map(_.size).sum
}
