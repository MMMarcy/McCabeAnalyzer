import entities._
import slick.driver.H2Driver
import slick.driver.H2Driver.api._
import slick.jdbc.TransactionIsolation
import slick.jdbc.meta.MTable

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}


/**
 * Created by marcello.steiner on 2015-05-25.
 */
object DatabaseHandler {

  val functions = TableQuery[UFTFunctionClass]
  val lines = TableQuery[UFTLineClass]
  val transactionIsolation: TransactionIsolation = TransactionIsolation.ReadCommitted
  var db: Option[H2Driver.backend.Database] = None

  def openConnection(): Unit = {
    db = Some(Database.forConfig("h2mem1"))
  }

  def closeConnection(): Unit = {
    require(db.isDefined)

    db.foreach(_.close())
    db.foreach(_.shutdown)
  }

  def createSchema(): Unit = {
    require(db.isDefined)
    createTableIfNotExists(lines, functions)
  }

  private def createTableIfNotExists(tables: TableQuery[_ <: Table[_]]*): Future[Seq[Unit]] = {
    val database = db.get
    Future.sequence(
      tables map { table =>
        database.run(MTable.getTables(table.baseTableRow.tableName)).flatMap { result =>
          if (result.isEmpty) {
            database.run(table.schema.create)
          } else {
            Future.successful(())
          }
        }
      }
    )
  }

  def insert(results: List[(UFTFunction, Seq[UFTLine])]): Unit = {
    require(db.isDefined)

    val insertStmt = DBIO.sequence(results.map(tupleToDBAction))
    val g = db.map(_.run(insertStmt))

    //Log whatever failure pops out from this
    g.foreach(_.onFailure { case s => println(s) })
    Await.result(g.get, Duration.Inf)
  }

  def tupleToDBAction = { (couple: (UFTFunction, Seq[UFTLine])) =>
    val (fun, ls) = couple
    for {
      funId <- (functions returning functions.map(_.id)) += fun
      _ <- lines ++= ls.map(l => (l._1, funId, l._3, l._4))
    } yield ()
  }

}
