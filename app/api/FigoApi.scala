package api

import java.net.{HttpURLConnection, URL}
import java.util.HashMap

import me.figo.FigoConnection
import me.figo.FigoSession
import me.figo.models._
import play.Configuration
import play.api.libs.json.{JsPath, Writes, Json}
import play.Logger

case class FigoUserAdress(
                           var country: Option[String],
                           var city: Option[String],
                           var street: Option[String],
                           var vat: Option[String],
                           var bill: Option[String],
                           var company: Option[String],
                           var street2: Option[String]
                         )
case class FigoUser(
                     var access_token: Option[String],
                     var name: Option[String],
                     var username: Option[String],
                     var email: Option[String],
                     var address: Option[FigoUserAdress],
                     var password: Option[String]
                   )

class MyFigoSession(token: String) extends FigoSession(token) {
  def getUser(figoUser: FigoUser): FigoUser = {
    val user: User = this.queryApi[User]("/rest/user", null, "GET", classOf[User])
    var list: HashMap[String, String] = user.getAddress()
    var userAddres: FigoUserAdress = new FigoUserAdress(
      Some(list.get("country")),
      Some(list.get("city")),
      Some(list.get("street")),
      Some(list.get("vat")),
      Some(list.get("bill")),
      Some(list.get("company")),
      Some(list.get("street2"))
    )
    figoUser.email = Some(user.getEmail())
    figoUser.name = Some(user.getName())
    figoUser.address = Some(userAddres)
    Logger.info("User {}", figoUser)
    figoUser
  }
}


class MyFigoConnection(clientId: String, clientSecret: String, redirectUri: String) extends FigoConnection(clientId, clientSecret, redirectUri) {
  def session(access_token: String) =  new MyFigoSession(access_token)
}


object FigoApi {

  val connection: MyFigoConnection = new MyFigoConnection(
    Configuration.root().getString("figo.id"),
    Configuration.root().getString("figo.secret"),
    Configuration.root().getString("figo.redirect")
  )

  def create(user: FigoUser) = connection.addUser(
    user.username.getOrElse(null),
    user.email.getOrElse(null),
    user.password.getOrElse(null),
    "en"
  )

  def createUserAndLogin(user: FigoUser) = connection.addUserAndLogin(
    user.username.getOrElse(null),
    user.email.getOrElse(null),
    user.password.getOrElse(null),
    "en"
  )

  def login(user: FigoUser) = connection.credentialLogin(
    user.username.getOrElse(null),
    user.password.getOrElse(null)
  )


  //def getAccounts(user: FigoUser) = connection.session(user).getAccounts()
  def getUser(user: FigoUser) = connection.session(user.access_token.getOrElse(null)).getUser(user)

  //def updateUser(user: FigoUser) = connection.session(user)

}
