import java.nio.charset.{StandardCharsets, Charset}
import java.util.Locale

import entities._
import org.omg.IOP.Encoding

import scala.collection.mutable.ListBuffer
import scala.io.Source

/**
 *
 * Created by marcello.steiner on 2015-05-21.
 */
object McCabeAnalyzer {

  def startParsing(fileName: String): List[Option[Int => (UFTFunction, Seq[UFTLine])]] = {
    val lines = Source.fromFile(fileName, StandardCharsets.UTF_16LE.displayName()).getLines().filter(filterFunction)
    createFunctionList(lines, fileName)
  }

  def filterFunction(line: String): Boolean = {
    //TODO: remove commented lines
    !(line.contains("'") || line.matches("(?m)^\\s*$[\n\r]{1,}"))
  }


  def createFunctionList(lines: Iterator[String], fileName: String):
  List[Option[Int => (UFTFunction, Seq[UFTLine])]] = {
    val buffer = ListBuffer[Option[Int => (UFTFunction, Seq[UFTLine])]]()
    while (lines.hasNext) {
      buffer += calculateFunctionScore(lines, fileName)
    }
    buffer.toList
  }

  def calculateFunctionScore(lines: Iterator[String], fileName: String):
  Option[Int => (UFTFunction, Seq[UFTLine])] = {

    val firstLine = lines.next().toLowerCase.split(" ")

    val isFunctionDeclaration = firstLine.exists(s => s.equals("function") || s.equals("sub")) &&
    !firstLine.exists(s => s.equals("end"))


    if (!isFunctionDeclaration)
      return None

    println(firstLine.mkString(" "))

    val name = firstLine
      .dropWhile(l => !(l.equals("function") || l.equals("sub")))(1)

    val body = lines
      .map(_.toLowerCase)
      .takeWhile(s => !(s.contains("end function") || s.contains("end sub")))
      .toList

    val lineMetrics = body.map(line => calculateLineMetrics(line))
    val hash = body.mkString("").hashCode
    val score = body.foldLeft(1)((acc, line) => acc + getValueForLine(line))

    Some(str => (0, str, name, fileName, score, hash) -> lineMetrics)
  }

  def calculateLineMetrics(line: String): UFTLine = {
    //TODO: improve logic for getting number of parameters
    val parametersCount = math.max(line.filter(_ == '(').length, line.filter(_ == ',').length)
    (0, 0, line.length, parametersCount)
  }

  def getValueForLine(line: String): Int = {
    line match {
      case s if s.contains("if") && s.contains("then") => 1
      case s if s.contains("case") => 1
      case s if s.contains("for") && !s.contains("exit") => 1
      case s if s.contains("while") => 1
      case _ => 0
    }
  }


}



