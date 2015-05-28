import java.io.File
import java.util.Locale

import entities.{UFTFunction, UFTLine}
import org.tmatesoft.hg.core.HgChangeset
import repository.HgAnalyzer

import scala.collection.mutable.ListBuffer

/**
 *
 * Created by Marcello Steiner on 2015-05-22.
 */
object EntryPoint {

  def main(args: Array[String]) {

    val rootFolder = new File(args(0).replaceAll("\\\\", "/"))

    try {
      setupDatabase()
      HgAnalyzer.initRepositoryAnalyzer(rootFolder)

      for (revision <- HgAnalyzer.getRevisions) {
        HgAnalyzer.checkoutRevision(revision)
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

  protected def getFileRecursively(root: File): Array[File] = {
    if (root.isDirectory)
      root.listFiles().flatMap(getFileRecursively)
    else {
      val name = root.getName.toLowerCase(Locale.ENGLISH)
      if (name.endsWith(".mts") || name.endsWith(".qfl") || name.endsWith(".vbs"))
        Array(root)
      else
        Array.empty
    }
  }

}
