package api

import java.net.{HttpURLConnection, URL}
import java.util
import java.util.Date

import me.figo.{FigoException, FigoConnection, FigoSession}
import me.figo.internal.{TaskStatusResponse, TaskTokenResponse}
import me.figo.models._
import play.Configuration
import play.api.libs.json.{JsPath, Writes, Json}
import play.Logger
import scala.collection.JavaConversions
import scala.collection.JavaConverters._

case class FigoLinkBank(
                         access_token: String,
                         var bank_code: String,
                         var country_code: String,
                         var credentials: Seq[String]
                       )

case class FigoUserAdress(
                           var country: Option[String],
                           var city: Option[String],
                           var street: Option[String],
                           var vat: Option[String],
                           var bill: Option[String] = Some("false"),
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

case class FigoAccountBalance(
                               var balance: BigDecimal,
                               var balance_date: Date,
                               var credit_line: BigDecimal,
                               var monthly_spending_limit: BigDecimal
                             )

case class FigoPaymentType(
                            var allowed_recipients: Option[Set[String]],
                            var max_purpose_length: Option[Int],
                            var supported_text_keys: Option[Set[String]],
                            var min_scheduled_date: Option[Date],
                            var max_scheduled_date: Option[Date],
                            var supported_file_formats: Option[Set[String]]
                          )

case class FigoAccount(
                        var account_id: String,
                        var bank_id: String,
                        var name: String,
                        var owner: String,
                        var auto_sync: Boolean,
                        var account_number: String,
                        var bank_code: String,
                        var bank_name: String,
                        var currency: String,
                        var iban: String,
                        var bic: String,
                        var account_type: String,
                        var icon: String,
                        var balance: FigoAccountBalance,
                        var additional_icons: Map[String, String],
                        var supported_payments: Map[String, FigoPaymentType]
                      )

class MyFigoSession(token: String) extends FigoSession(token) {

  def javaListToScalaSet[E](l: util.List[E]): Set[E] = JavaConversions.asScalaBuffer[E](l).toSet


  def mapUser(user: User, figoUser: FigoUser): User = {
    val userToUpdate = new User()
    var address: util.HashMap[String, String] = new util.HashMap[String, String]()
    userToUpdate.setName(figoUser.name.getOrElse(null))
    val figoAddress = figoUser.address.getOrElse(null)
    if (!figoUser.address.isEmpty) {
      address.put("country", figoAddress.country.getOrElse(null))
      address.put("city", figoAddress.city.getOrElse(null))
      address.put("street", figoAddress.street.getOrElse(null))
      address.put("vat", figoAddress.vat.getOrElse(null))
      address.put("bill", figoAddress.bill.getOrElse("false"))
      address.put("company", figoAddress.company.getOrElse(null))
      address.put("street2", figoAddress.street2.getOrElse(null))
      userToUpdate.setAddress(address)
    }
    userToUpdate
  }

  def mapFigoUser(user: User, figoUser: FigoUser): FigoUser = {
    var list: util.HashMap[String, String] = user.getAddress()
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

  def mapFigoAccount(account: Account): FigoAccount = {
    val balance = account.getBalance()

    var supported_payments = Map[String, FigoPaymentType]()
    val payments = account.getSupportedPaymentTypes()
    Logger.info("Payments {}", payments.keySet().toArray().toString())
    for ((k: String, payment: me.figo.models.PaymentType) <- JavaConversions.mapAsScalaMap(payments)) {
      val figoPayment: FigoPaymentType = new FigoPaymentType(
        Some(javaListToScalaSet[String](payment.getAllowedRecipients())),
        Some(payment.getMaxPurposeLength()),
        Some(javaListToScalaSet[String](payment.getSupportedTextKeys())),
        Some(payment.getMinScheduledDate()),
        Some(payment.getMaxScheduledDate()),
        Some(javaListToScalaSet[String](payment.getSupportedFileFormats()))
      )
      supported_payments ++ Map(k -> figoPayment)
    }

    Logger.info("supported_payments {}", supported_payments)

    new FigoAccount(
      account.getAccountId(),
      account.getBankId(),
      account.getName(),
      account.getOwner(),
      account.isAutoSync(),
      account.getAccountNumber(),
      account.getBankCode(),
      account.getBankName(),
      account.getCurrency(),
      account.getIBAN(),
      account.getBIC(),
      account.getType(),
      account.getIconUrl(),
      new FigoAccountBalance(
        balance.getBalance(),
        balance.getBalanceDate(),
        balance.getCreditLine(),
        balance.getMonthlySpendingLimit()
      ),
      JavaConversions.mapAsScalaMap(account.getAddtionalIcons()).toMap,
      supported_payments
    )
  }

  def linkAccount(bankLink: FigoLinkBank): TaskStatusResponse = {
    val tokenResponse: TaskTokenResponse = this.setupNewAccount(
      bankLink.bank_code,
      bankLink.country_code,
      JavaConversions.seqAsJavaList(bankLink.credentials),
      JavaConversions.seqAsJavaList(Seq[String]())
    )
    Logger.info("TaskToken {}", tokenResponse.getTaskToken())
    var taskStatus: TaskStatusResponse = this.getTaskState(tokenResponse)

    while (!taskStatus.isEnded() && !taskStatus.isErroneous() && !taskStatus.isWaitingForPin() && !taskStatus.isWaitingForResponse()) {
      taskStatus = this.getTaskState(tokenResponse)
      Thread.sleep(1000L)
    }

    if (taskStatus.isWaitingForPin() && !taskStatus.isEnded()) {
      throw new FigoException("Waiting for a pin", taskStatus.getMessage());
    } else if (taskStatus.isErroneous() && taskStatus.isEnded()) {
      throw new FigoException("Figo Exception", taskStatus.getMessage());
    } else {
      Logger.info("TaskToken {}", taskStatus.getMessage())
    }
    taskStatus
  }

  def mapAccount(account: Account, figoAccount: FigoAccount): Account = {
    account.setName(figoAccount.name)
    account.setOwner(figoAccount.owner)
    account
  }

  def getFigoAccounts(): Set[FigoAccount] = {
    var figoAccounts = Set[FigoAccount]()
    val accounts: Set[Account] = javaListToScalaSet[Account](this.getAccounts())
    Logger.info("Accounts {}", accounts.toString())
    accounts.foreach {
      case item => {
        figoAccounts += mapFigoAccount(item)
      }
    }
    figoAccounts
  }

  def getUser(figoUser: FigoUser): FigoUser = {
    val user: User = this.queryApi[User]("/rest/user", null, "GET", classOf[User])
    mapFigoUser(user, figoUser)
  }

  def updateUser(figoUser: FigoUser): FigoUser = {
    val user: User = this.queryApi[User]("/rest/user", mapUser(new User, figoUser), "PUT", classOf[User])
    mapFigoUser(user, figoUser)
  }
}


class MyFigoConnection(clientId: String, clientSecret: String, redirectUri: String) extends FigoConnection(clientId, clientSecret, redirectUri) {
  def session(access_token: String) = new MyFigoSession(access_token)
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


  def getAccounts(user: FigoUser) = connection.session(user.access_token.getOrElse(null)).getFigoAccounts()

  def getUser(user: FigoUser) = connection.session(user.access_token.getOrElse(null)).getUser(user)

  def updateUser(user: FigoUser) = connection.session(user.access_token.getOrElse(null)).updateUser(user)

  def linkAccount(link: FigoLinkBank) = connection.session(link.access_token).linkAccount(link)
}
