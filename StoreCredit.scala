import scala.io.Source
import java.io._

object StoreCredit {

  val source = Source.fromFile("A-large-practice.in").getLines().toList

  def findItems(lines: List[String]): List[List[Int]] = lines match {
    case Nil => List()
    case _ :: Nil => List()
    case credit :: _ :: items :: rest => findPairItems(credit.toInt, items.split(" ").map(_.toInt).toList.zipWithIndex) :: findItems(rest)
    case credit :: _ :: rest => List()
  }

  def findPairItems(credit: Int, items: List[Pair[Int, Int]]): List[Int] = {

    def findPairItems1(itemsLeft: List[Pair[Int, Int]]): List[Int] = itemsLeft match {
      case last :: Nil =>
        findPairItems(credit, items.tail)
      case first :: second :: rest =>
        if ((first._1 + second._1) == credit)
          List((first._2 + 1), (second._2 + 1))
        else
          findPairItems1(first::rest)
      case Nil => List()
    }
    findPairItems1(items)
  }

  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(f)
    try { op(p) } finally { p.close() }
  }

  def main(args: Array[String]) {
    val result = findItems(source.tail)
    printToFile(new File("A-large-practice.out"))(p => result.zipWithIndex.foreach((x) => 
        p.println("Case #" + (x._2 + 1) + ": " + x._1.mkString(" "))
    ))
  }
}
