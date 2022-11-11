import scala.collection.mutable.ArraySeq

object Hash extends App {
  val yukiguni = List("国境", "の", "長い", "トンネル", "を", "抜ける", "と", "雪国", "で", "あつ", "た")

  yukiguni.foreach(word => println(word, word.hashCode()))
  println()

  val occurrences = ArraySeq.fill(31)(0)

  def hash(word: String): Int = word.hashCode() % occurrences.size

  yukiguni.foreach(word => println(word, hash(word)))
  println()

  yukiguni.foreach(word => {
    val h = hash(word)
    occurrences(h) = occurrences(h) + 1
  })

  yukiguni.foreach(word => println(word, occurrences(hash(word))))
  println()
}

object OpenAddressing extends App {
  val HASH_SIZE = 31
  val entries: ArraySeq[String]  = ArraySeq.fill(HASH_SIZE)("")
  val occurrences: ArraySeq[Int] = ArraySeq.fill(HASH_SIZE)(0)

  def hash(word: String): Int = word.hashCode() % HASH_SIZE

  def new_occurrence(word: String): Unit = {
    var h = hash(word) % HASH_SIZE
    while (entries(h) != "" && entries(h) != word) h = (h + 1) % HASH_SIZE
    entries(h) = word
    occurrences(h) = occurrences(h) + 1
  }

  val yukiguni = List("国境", "の", "長い", "トンネル", "を", "抜ける", "と", "雪国", "で", "あつ", "た")
  yukiguni.foreach(word => new_occurrence(word))
  for (i <- 0 until HASH_SIZE) {
    if (entries(i) != "") println(f"$i ${entries(i)}(${occurrences(i)})")
  }
}
