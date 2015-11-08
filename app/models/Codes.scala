package models

import play.api.Play
import scala.concurrent.Future
import play.api.db.slick.DatabaseConfigProvider
import play.api.db.slick.HasDatabaseConfig
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import slick.driver.JdbcProfile


case class Codes(var code_id: Option[Int] = null, code: String, var cash: Option[String] = null)

/**
  * User table
  */
trait CodeTable {
  self: HasDatabaseConfig[JdbcProfile] =>

  import driver.api._

  class CodeTable(tag: Tag) extends Table[Codes](tag, "User") {

    def code_id = column[Option[Int]]("code_id", O.PrimaryKey, O.AutoInc)

    def code = column[String]("code")

    def cash = column[Option[String]]("cash")

    def * = (code_id, code, cash) <>(Codes.tupled, Codes.unapply _)
  }

}

/**
  * User dao class
  */
class CodeDAO extends CodeTable with HasDatabaseConfig[JdbcProfile] {

  protected val dbConfig = DatabaseConfigProvider.get[JdbcProfile](Play.current)

  import driver.api._

  private lazy val codes = TableQuery[CodeTable]

  def all(): Future[Seq[Codes]] = db.run(codes.result).map(_.toList)

  def insert(code: Codes): Future[Unit] = db.run(codes += code).map(_ => ())

}