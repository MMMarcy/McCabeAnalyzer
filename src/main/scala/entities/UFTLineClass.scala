package entities

import slick.driver.H2Driver.api._
import slick.lifted.{ForeignKeyQuery, ProvenShape}




class UFTLineClass(tag: Tag) extends Table[UFTLine](tag, "LINE") {
  override def * : ProvenShape[UFTLine] = (id, funId, length, params)

  def id = column[Int]("LINE_ID", O.PrimaryKey, O.AutoInc)

  def length = column[Int]("LINE_LENGTH")

  def params = column[Int]("LINE_N_PARAMS")

  def function: ForeignKeyQuery[UFTFunctionClass, UFTFunction] =
    foreignKey("FUN_ID", funId, TableQuery[UFTFunctionClass])(_.id)

  def funId = column[Int]("FUN_ID")
}
