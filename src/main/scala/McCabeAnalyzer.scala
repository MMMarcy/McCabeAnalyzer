import entities._

import scala.collection.mutable.ListBuffer
import scala.io.Source

/**
 *
 * Created by marcello.steiner on 2015-05-21.
 */
object McCabeAnalyzer {

  def startParsing(fileName: String): List[Option[String => (UFTFunction, Seq[UFTLine])]] = {
    val src = fileName
    val lines = Source.fromFile(src).getLines().filterNot(filterFunction)
    createFunctionList(lines, fileName)
  }

  def filterFunction(line: String): Boolean = {
    //TODO: remove commented lines
    line.matches("(?m)^\\s*$[\n\r]{1,}")
  }


  def createFunctionList(lines: Iterator[String], fileName: String):
  List[Option[String => (UFTFunction, Seq[UFTLine])]] = {
    val buffer = ListBuffer[Option[String => (UFTFunction, Seq[UFTLine])]]()
    while (lines.hasNext) {
      buffer += calculateFunctionScore(lines, fileName)
    }
    buffer.toList
  }

  def calculateFunctionScore(lines: Iterator[String], fileName: String):
  Option[String => (UFTFunction, Seq[UFTLine])] = {

    val firstLine = lines.next()
    if (!firstLine.contains("Function"))
      return None

    val name = firstLine.split(" ").dropWhile(!_.equals("Function"))(1)
    val body = lines.takeWhile(s => !s.contains("End Function")).toList
    val lineMetrics = body.map(line => calculateLineMetrics(line))
    val hash = body.mkString("").hashCode
    val score = body.foldLeft(1)((acc, line) => acc + getValueForLine(line))
    Some(str => (0, str, name, fileName, score, hash.toString) -> lineMetrics)
  }

  def calculateLineMetrics(line: String): UFTLine = {
    //TODO: improve logic for getting number of parameters
    val parametersCount = math.max(line.filter(_ == '(').length, line.filter(_ == ',').length)
    (0, 0, line.length, parametersCount)
  }

  def getValueForLine(line: String) = {
    line match {
      case s if s.contains("If") && s.contains("Then") => 1
      case s if s.contains("Case") => 1
      case s if s.contains("For") && !s.contains("Exit") => 1
      case _ => 0
    }
  }


}



