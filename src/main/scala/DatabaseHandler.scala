import entities._
import slick.driver.H2Driver
import slick.driver.H2Driver.api._
import slick.jdbc.TransactionIsolation

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration


/**
 * Created by marcello.steiner on 2015-05-25.
 */
object DatabaseHandler {

  val functions = TableQuery[UFTFunctionClass]
  val lines = TableQuery[UFTLineClass]
  val transactionIsolation: TransactionIsolation = TransactionIsolation.ReadCommitted
  var db: Option[H2Driver.backend.Database] = None

  def openConnection(): Unit = {
    val tmp = Database.forConfig("h2mem1")
    db = Some(tmp)
  }

  def closeConnection(): Unit = {
    //
    db.foreach(_.close())
    db.foreach(_.shutdown)
  }

  def createSchema(): Unit = {
    require(db.isDefined)
    val createSchemaStmt = (functions.schema ++ lines.schema).create
    val result = db.map(_.run(DBIO.seq(createSchemaStmt).withPinnedSession))
    Await.result(result.get, Duration.Inf)
  }

  def insert(results: List[(UFTFunction, Seq[UFTLine])]): Unit = {
    require(db.isDefined)

    def f = { (couple: (UFTFunction, Seq[UFTLine])) =>
      val (fun, ls) = couple
      for {
        funId <- (functions returning functions.map(_.id)) += fun
        _ <- lines ++= ls.map(l => (l._1, funId, l._3, l._4))
      } yield ()
    }

    val insertStmt = DBIO.sequence(results.map(f))

    val g = db.map {
      connection =>
        connection.run {
          insertStmt
        }
    }
    g.foreach(_.onFailure { case s => println(s) })

    Await.result(g.get, Duration.Inf)
  }

}
