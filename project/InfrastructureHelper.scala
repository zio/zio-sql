import sbt.Keys.TaskStreams

import scala.sys.process._

object InfrastructureHelper {

  sealed trait Database { self =>

    // must align with the service name in docker-compose.yaml
    def name: String = self match {
      case Database.Postgres => "postgres"
      case Database.MySQL    => "mysql"
      case Database.MSSQL    => "mssql"
      case Database.Oracle   => "oracle"
    }

    def port: Int = self match {
      case Database.Postgres => 5432
      case Database.MySQL    => 3306
      case Database.MSSQL    => 1433
      case Database.Oracle   => 1521
    }
  }

  object Database {
    case object Postgres extends Database
    case object MySQL    extends Database
    case object MSSQL    extends Database
    case object Oracle   extends Database
  }

  val shell: Seq[String] =
    if (sys.props("os.name").contains("Windows")) Vector("cmd", "/c")
    else Vector("bash", "-c")

  def startService(db: Database, s: TaskStreams): Unit = {
    val dockerComposeUp = shell :+ s"docker-compose up -d ${db.name}"
    if (dockerComposeUp.! == 0) s.log.success(s"${db.name} started on port ${db.port}")
    else s.log.error(s"${db.name} was not able to start up")
  }

  def stopService(db: Database, s: TaskStreams): Unit = {
    val dockerComposeDown = shell :+ s"docker-compose stop ${db.name}"
    if (dockerComposeDown.! == 0) s.log.success(s"${db.name} was stopped")
    else s.log.error(s"${db.name} was not able to shut down properly")
  }
}
