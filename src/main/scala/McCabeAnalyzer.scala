import scala.collection.mutable.ListBuffer
import scala.io.Source

/**
 *
 * Created by marcello.steiner on 2015-05-21.
 */
object McCabeAnalyzer {

  def startParsing(fileName: String): List[Function] = {
    val src = fileName
    val lines = Source.fromFile(src).getLines().filterNot(filterFunction)
    createFunctionList(lines, fileName)
  }

  def filterFunction(line: String): Boolean = {
    /*line.contains("Function") ||
      (line.contains("If") && line.contains("Then")) ||
      line.contains("Else") ||
      line.contains("For") ||
      line.contains("Case")||
      line.contains("End If") ||
      line.contains("End Function")*/
    line.matches("(?m)^\\s*$[\n\r]{1,}")
  }


  def createFunctionList(lines: Iterator[String], fileName: String): List[Function] = {
    val buffer = ListBuffer[Function]()
    while (lines.hasNext) {
      calculateFunctionScore(lines, fileName).foreach(op => buffer.append(op))
    }
    buffer.toList
  }

  def calculateFunctionScore(lines: Iterator[String], fileName: String): Option[Function] = {
    val firstLine = lines.next()
    if (!firstLine.contains("Function"))
      return None
    val name = firstLine.split(" ").dropWhile(!_.equals("Function"))(1)
    val body = lines.takeWhile(s => !s.contains("End Function")).toList
    val hash = body.mkString("").hashCode
    val score = body.foldLeft(1)((acc, line) => acc + getValueForLine(line))
    Some(Function(name, fileName, score, hash.toString))
  }

  def getValueForLine(line: String) = {
    line match {
      case s if s.contains("If") && s.contains("Then") => 1
      case s if s.contains("Case") => 1
      case s if s.contains("For") && !s.contains("Exit") => 1
      case _ => 0
    }
  }

  case class Function(name: String, file: String, mcCabeScore: Int, hash: String) {}

}



