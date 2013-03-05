import scala.io.Source
import java.io._

object SpeakingTongues {

  val source = Source.fromFile("A-small-practice.in").getLines().toList

  val textIn = List("ejp mysljylc kd kxveddknmc re jsicpdrysi",
                      "rbcpc ypc rtcsra dkh wyfrepkym veddknkmkrkcd",
                      "de kr kd eoya kw aej tysr re ujdr lkgc jv qz")
  
  val textOut = List("our language is impossible to understand",
                       "there are twenty six factorial possibilities",
                       "so it is okay if you want to just give up zq")

  val referenceTable = textIn.zip(textOut).map(x => x._1.toList zip x._2.toList).flatten.distinct

  def translate(text: String) = text.toList.map(x => referenceTable.find(_._1 == x).get._2) mkString

  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(f)
    try { op(p) } finally { p.close() }
  }

  def main(args: Array[String]) {
    val result = source.tail.map(x => translate(x))

    printToFile(new File("A-small-practice.out"))(p => result.zipWithIndex.foreach((x) => 
        p.println("Case #" + (x._2 + 1) + ": " + x._1)
    ))
  }
  
}
