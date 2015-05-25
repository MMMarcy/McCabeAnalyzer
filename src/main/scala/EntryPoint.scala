import java.io.File
import java.sql.{Connection, DriverManager}

import entities.{UFTLine, UFTFunction}

import scala.collection.mutable.ListBuffer

/**
 * Created by marcello.steiner on 2015-05-22.
 */
object EntryPoint {

  def main(args: Array[String]) {

    try {
      DatabaseHandler.openConnection()
      DatabaseHandler.createSchema()

      val rootFolder = new File(args(0).replaceAll("\\\\", "/"))
      val files = getFileRecursively(rootFolder)

      //TODO: add revision
      for{
        revision <- List("testRevision")
        file <- files
      }{
        val result = McCabeAnalyzer.startParsing(file.getAbsolutePath)
        val b = ListBuffer[(UFTFunction,Seq[UFTLine])]()
        result.foreach{
          case None =>
          case Some(res) => b += res(revision)
        }
        DatabaseHandler.insert(b.toList)
      }
    }
    catch {
      case e: Exception => e.printStackTrace()
    }
    finally DatabaseHandler.closeConnection()

  }



  def getFileRecursively(root: File): Array[File] = {
    if (root.isDirectory)
      root.listFiles().flatMap(getFileRecursively)
    else if (root.getName.endsWith(".mts"))
      Array(root)
    else
      Array.empty
  }

}
