package api

import me.figo.FigoConnection
import me.figo.FigoSession

import play.Configuration

case class FigoUser(
                     var access_token: Option[String],
                     var name: Option[String],
                     var username: Option[String],
                     var email: Option[String],
                     var address: Option[String],
                     var password: Option[String]
                   )


class MyFigoConnection(clientId: String, clientSecret: String, redirectUri: String) extends FigoConnection(clientId, clientSecret, redirectUri) {



  def session(user: FigoUser) = new FigoSession(user.access_token.getOrElse(null))
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


  def getUser(user: FigoUser) = connection.session(user).getUser()

  def updateUser(user: FigoUser) = connection.session(user)

}
