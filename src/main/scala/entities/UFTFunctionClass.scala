package entities

import slick.driver.H2Driver.api._
import slick.lifted.ProvenShape


/**
 * Created by marcello.steiner on 2015-05-25.
 */
class UFTFunctionClass(tag: Tag) extends Table[UFTFunction](tag, "FUNCTION") {

  // Every table needs a * projection with the same type as the table's type parameter
  override def * : ProvenShape[UFTFunction] =
    (id, revision, name, file, complexity, hashcode)

  def id = column[Int]("FUN_ID", O.PrimaryKey, O.AutoInc)

  // This is the primary key column
  def revision = column[Int]("FUN_REVISION")

  def name = column[String]("FUN_NAME")

  def file = column[String]("FUN_FILE")

  def complexity = column[Int]("FUN_COMPLEXITY")

  def hashcode = column[Int]("FUN_HASHCODE")

}