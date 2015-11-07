package api

import java.net.{HttpURLConnection, URL}

import me.figo.FigoConnection
import me.figo.internal.TokenResponse
import play.Configuration
import java.lang.reflect.Type
import java.nio.charset.Charset
import play.api.libs.json.Json

case class FigoUser(
                     token: Option[String],
                     name: Option[String],
                     username: Option[String],
                     email: Option[String],
                     address: Option[String],
                     password: Option[String]
                   )


class MyFigoConnection(clientId: String, clientSecret: String, redirectUri: String)  extends FigoConnection(clientId, clientSecret, redirectUri) {
  def sendRequest(path: String, data: Object,  method: String, token: String)(typeOfT: Type) = {
    val url: URL = new URL("https://api.figo.me" + path)
    val connection: HttpURLConnection = url.openConnection().asInstanceOf[HttpURLConnection]
    connection.setConnectTimeout(10000)
    connection.setReadTimeout(10000)
    this.setupTrustManager(connection)
    connection.setRequestMethod(method)
    connection.setRequestProperty("Authorization", token)
    connection.setRequestProperty("Accept", "application/json")
    connection.setRequestProperty("Content-Type", "application/json")
    if(data != null) {
      val encodedData: String = this.createGson().toJson(data)
      connection.setDoOutput(true)
      connection.getOutputStream().write(encodedData.getBytes(Charset.forName("UTF-8")))
    }
    this.processResponse(connection, typeOfT)
  }
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


  def getUser(user: FigoUser) = {
    connection.queryApi[TokenResponse](
      "/rest/user?token=" + user.token.getOrElse(null),
      user,
      "GET",
      FigoUser.getClass()
    )
  }

  def updateUser(user: FigoUser) = {
    connection.queryApi[FigoUser](
      "/rest/user?token=" + user.token.getOrElse(null),
      user,
      "PUT",
      FigoUser.getClass()
    )
  }

}
