import java.io.File
import java.sql.{Connection, DriverManager}

/**
 * Created by marcello.steiner on 2015-05-22.
 */
object EntryPoint {

  def main(args: Array[String]) {
    //TODO: get text files recursively given a root folder
    val rootFolder = new File(args(0).replaceAll("\\\\", "/"))
    val conn = openDatabase()


    val files = getFileRecursively(rootFolder)
    val results = files.flatMap(file => McCabeAnalyzer.startParsing(file.getAbsolutePath))

    try {

      val s = conn.createStatement()

      s.execute("Create table if not exists testTable (functionName TEXT, fileName TEXT, score TINYINT, hashcode VARCHAR(16))")
      conn.commit()

      results.foreach {
        res =>
          s.addBatch(s"""INSERT INTO TESTTABLE (FUNCTIONNAME, FILENAME, SCORE, HASHCODE) values ('${res.name}', '${res.file}', ${res.mcCabeScore}, '${res.hash}')""")
      }
      s.executeBatch()
      conn.commit()

    } catch {
      case e: Exception => e.printStackTrace()
    } finally {

      conn.close()
    }
  }

  def openDatabase(): Connection = {
    val path = """C:\Users\marcello.steiner\Desktop\UFT\test""".replaceAll("\\\\", "/")
    Class.forName("org.h2.Driver")
    DriverManager.getConnection(s"jdbc:h2:$path")
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
