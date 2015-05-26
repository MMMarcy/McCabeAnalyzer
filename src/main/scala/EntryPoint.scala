import java.io.File

import entities.{UFTFunction, UFTLine}
import org.tmatesoft.hg.core.HgChangeset
import repository.Analyzer

import scala.collection.mutable.ListBuffer

/**
 * Created by marcello.steiner on 2015-05-22.
 */
object EntryPoint {

  def main(args: Array[String]) {

    val rootFolder = new File(args(0).replaceAll("\\\\", "/"))

    try {
      setupDatabase()
      Analyzer.initRepositoryAnalyzer(rootFolder)

      for (revision <- Analyzer.getRevisions) {
        Analyzer.checkoutRevision(revision)
        getFileRecursively(rootFolder).foreach {
          analyzeFile(_, revision)
        }
      }
    }
    catch {
      case e: Exception => e.printStackTrace()
    }
    finally DatabaseHandler.closeConnection()

  }

  def analyzeFile(file: File, revision: HgChangeset): Unit = {
    val result = McCabeAnalyzer.startParsing(file.getAbsolutePath)
    val b = ListBuffer[(UFTFunction, Seq[UFTLine])]()
    result.foreach {
      case None =>
      case Some(res) => b += res(revision.getRevisionIndex.toString)
    }
    DatabaseHandler.insert(b.toList)

  }

  def setupDatabase(): Unit = {
    DatabaseHandler.openConnection()
    DatabaseHandler.createSchema()
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
