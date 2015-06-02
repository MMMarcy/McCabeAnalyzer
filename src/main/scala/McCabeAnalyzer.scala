import java.io.File
import java.nio.charset.{Charset, StandardCharsets}

import entities._
import org.mozilla.universalchardet.UniversalDetector

import scala.collection.mutable.ListBuffer
import scala.io.Source

/**
 *
 * Created by marcello.steiner on 2015-05-21.
 */
object McCabeAnalyzer {

  def startParsing(fileName: String): List[Option[Int => (UFTFunction, Seq[UFTLine])]] = {
    val file = new File(fileName)
    val charset: Charset = guessEncoding(file)
    val lines = Source.fromFile(file, charset.displayName()).getLines().filter(filterFunction)
    createFunctionList(lines, fileName)
  }

  def guessEncoding(file: File): Charset = {
    val buf = Array.ofDim[Byte](4096)
    val fis = new java.io.FileInputStream(file)
    // (1)
    val detector = new UniversalDetector(null)

    // (2)
    var nRead = fis.read(buf)
    while (nRead > 0 && !detector.isDone) {
      detector.handleData(buf, 0, nRead)
      nRead = fis.read(buf)
    }
    // (3)
    detector.dataEnd()

    // (4)
    val detected = detector.getDetectedCharset
    val encoding = if (detected == null)
      StandardCharsets.US_ASCII
    else
      Charset.forName(detected)
    detector.reset()
    encoding
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
      !firstLine.exists(s => s.equals("end") || s.equals("exit"))


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



