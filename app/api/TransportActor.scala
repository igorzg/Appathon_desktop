package api

import java.util
import java.util.Date

import akka.actor.{ActorLogging, ActorRef, Actor, Props}
import akka.event.LoggingReceive
import me.figo.FigoException
import me.figo.internal.{TaskStatusResponse, TaskTokenResponse, TokenResponse}
import play.Logger
import scala.concurrent.Future
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import scala.concurrent.ExecutionContext.Implicits.global
import play.api.Play.current
import api.{FigoApi, FigoUser, FigoUserAdress, FigoAccount, FigoPaymentType, FigoAccountBalance}

/**
  * Event
  * @param name
  * @param id
  * @param params
  * @param terminate
  */
case class Event(name: String,
                 id: String,
                 params: JsValue,
                 terminate: Boolean,
                 session_id: String,
                 var out: Option[ActorRef])

/**
  * Event handler
  */
class EventHandler {

  implicit lazy val userBankLinkWrite: Writes[FigoLinkBank] = (
    (JsPath \ "access_token").write[String] and
      (JsPath \ "bank_code").write[String] and
      (JsPath \ "country_code").write[String] and
      (JsPath \ "credentials").write[Seq[String]]
    ) (unlift(FigoLinkBank.unapply))

  implicit lazy val userBankLinkRead: Reads[FigoLinkBank] = (
    (JsPath \ "access_token").read[String] and
      (JsPath \ "bank_code").read[String] and
      (JsPath \ "country_code").read[String] and
      (JsPath \ "credentials").read[Seq[String]]
    ) (FigoLinkBank.apply _)


  implicit lazy val userFigoUserAdressWrites: Writes[FigoUserAdress] = (
    (JsPath \ "country").writeNullable[String] and
      (JsPath \ "city").writeNullable[String] and
      (JsPath \ "street").writeNullable[String] and
      (JsPath \ "vat").writeNullable[String] and
      (JsPath \ "bill").writeNullable[String] and
      (JsPath \ "company").writeNullable[String] and
      (JsPath \ "street2").writeNullable[String]
    ) (unlift(FigoUserAdress.unapply))

  implicit lazy val userFigoUserAdressReads: Reads[FigoUserAdress] = (
    (JsPath \ "country").readNullable[String] and
      (JsPath \ "city").readNullable[String] and
      (JsPath \ "street").readNullable[String] and
      (JsPath \ "vat").readNullable[String] and
      (JsPath \ "bill").readNullable[String] and
      (JsPath \ "company").readNullable[String] and
      (JsPath \ "street2").readNullable[String]
    ) (FigoUserAdress.apply _)


  implicit lazy val userFigoWrites: Writes[FigoUser] = (
    (JsPath \ "access_token").writeNullable[String] and
      (JsPath \ "name").writeNullable[String] and
      (JsPath \ "username").writeNullable[String] and
      (JsPath \ "email").writeNullable[String] and
      (JsPath \ "address").writeNullable[FigoUserAdress] and
      (JsPath \ "password").writeNullable[String]
    ) (unlift(FigoUser.unapply))


  implicit lazy val userFigoReads: Reads[FigoUser] = (
    (JsPath \ "access_token").readNullable[String] and
      (JsPath \ "name").readNullable[String] and
      (JsPath \ "username").readNullable[String] and
      (JsPath \ "email").readNullable[String] and
      (JsPath \ "address").readNullable[FigoUserAdress] and
      (JsPath \ "password").readNullable[String]
    ) (FigoUser.apply _)

  implicit lazy val paymentTypeReads: Reads[FigoPaymentType] = (
    (JsPath \ "allowed_recipients").readNullable[Set[String]] and
      (JsPath \ "max_purpose_length").readNullable[Int] and
      (JsPath \ "supported_text_keys").readNullable[Set[String]] and
      (JsPath \ "min_scheduled_date").readNullable[Date] and
      (JsPath \ "max_scheduled_date").readNullable[Date] and
      (JsPath \ "supported_file_formats").readNullable[Set[String]]
    ) (FigoPaymentType.apply _)

  implicit lazy val paymentTypeWrites: Writes[FigoPaymentType] = (
    (JsPath \ "allowed_recipients").writeNullable[Set[String]] and
      (JsPath \ "max_purpose_length").writeNullable[Int] and
      (JsPath \ "supported_text_keys").writeNullable[Set[String]] and
      (JsPath \ "min_scheduled_date").writeNullable[Date] and
      (JsPath \ "max_scheduled_date").writeNullable[Date] and
      (JsPath \ "supported_file_formats").writeNullable[Set[String]]
    ) (unlift(FigoPaymentType.unapply))


  implicit lazy val accountBalanceReads: Reads[FigoAccountBalance] = (
    (JsPath \ "balance").read[BigDecimal] and
      (JsPath \ "balance_date").read[Date] and
      (JsPath \ "credit_line").read[BigDecimal] and
      (JsPath \ "monthly_spending_limit").read[BigDecimal]
    ) (FigoAccountBalance.apply _)

  implicit lazy val accountBalanceWrites: Writes[FigoAccountBalance] = (
    (JsPath \ "balance").write[BigDecimal] and
      (JsPath \ "balance_date").write[Date] and
      (JsPath \ "credit_line").write[BigDecimal] and
      (JsPath \ "monthly_spending_limit").write[BigDecimal]
    ) (unlift(FigoAccountBalance.unapply))

  implicit lazy val accountFigoReads: Reads[FigoAccount] = (
    (JsPath \ "account_id").read[String] and
      (JsPath \ "bank_id").read[String] and
      (JsPath \ "name").read[String] and
      (JsPath \ "owner").read[String] and
      (JsPath \ "auto_sync").read[Boolean] and
      (JsPath \ "account_number").read[String] and
      (JsPath \ "bank_code").read[String] and
      (JsPath \ "bank_name").read[String] and
      (JsPath \ "currency").read[String] and
      (JsPath \ "iban").read[String] and
      (JsPath \ "bic").read[String] and
      (JsPath \ "account_type").read[String] and
      (JsPath \ "icon").read[String] and
      (JsPath \ "balance").read[FigoAccountBalance] and
      (JsPath \ "additional_icons").read[Map[String, String]]  and
      (JsPath \ "supported_payments").read[Map[String, FigoPaymentType]]
    ) (FigoAccount.apply _)


  implicit lazy val accountFigoWrites: Writes[FigoAccount] = (
    (JsPath \ "account_id").write[String] and
      (JsPath \ "bank_id").write[String] and
      (JsPath \ "name").write[String] and
      (JsPath \ "owner").write[String] and
      (JsPath \ "auto_sync").write[Boolean] and
      (JsPath \ "account_number").write[String] and
      (JsPath \ "bank_code").write[String] and
      (JsPath \ "bank_name").write[String] and
      (JsPath \ "currency").write[String] and
      (JsPath \ "iban").write[String] and
      (JsPath \ "bic").write[String] and
      (JsPath \ "account_type").write[String] and
      (JsPath \ "icon").write[String] and
      (JsPath \ "balance").write[FigoAccountBalance] and
      (JsPath \ "additional_icons").write[Map[String, String]]  and
      (JsPath \ "supported_payments").write[Map[String, FigoPaymentType]]
    ) (unlift(FigoAccount.unapply))
  /**
    * Events list
    */
  var events: Array[Event] = Array()


  /**
    * Add an event
    * @param event
    */
  def add(event: Event) = {
    events = events :+ event
  }

  /**
    * Handle errors
    * @param event
    * @param error
    */
  def error(event: Event, error: Throwable): Unit = {

    println(Json.obj(
      "name" -> event.name,
      "id" -> event.id,
      "data" -> Json.obj(
        "message" -> error.getMessage(),
        "error" -> true
      )
    ))
    event.out.get ! Json.obj(
      "name" -> event.name,
      "id" -> event.id,
      "data" -> Json.obj(
        "message" -> error.getMessage(),
        "error" -> true
      )
    )
  }


  /**
    * Trigger event by name
    * @param name
    */
  def broadcast(name: String): Unit = {
    events.foreach(item => {
      if (item.name.equals(name)) {
        handle(item)
      }
    })
  }


  /**
    * Brodcast event only for current user
    * @param name
    */
  def broadcastWithSession(name: String, session_id: String): Unit = {
    events.filter(item => item.session_id == session_id).foreach(item => {
      if (item.name.equals(name)) {
        handle(item)
      }
    })
  }

  /**
    * Handle event
    * @param e
    */
  def handle(e: Event) = {
    if (has(e)) {
      val event = get(e)
      Logger.info("Handle {}", event.toString)
      event.name match {

        case "logIn" => {
          val f: Future[TokenResponse] = Future {
            val user: FigoUser = event.params.as[FigoUser]
            FigoApi.login(user)
          }
          f onSuccess {
            case v: TokenResponse => {
              println("TokenResponse logIn {}", v)
              event.out.get ! Json.obj(
                "name" -> event.name,
                "id" -> event.id,
                "data" -> Json.obj(
                  "token" -> v.getAccessToken(),
                  "expires" -> v.getExpiresIn().toString(),
                  "refresh" -> v.getRefreshToken()
                )
              )
            }

          }
          f onFailure {
            case m: FigoException => {
              println("Error on login {}", m)
              sendGeneralError(event, "Wrong username or password!")
            }
            case t => sendGeneralError(event, "Wrong username or password!")
          }
        }
        case "createUser" => {
          val f: Future[TokenResponse] = Future {
            Logger.info("createUser params {}", event.params)
            val user: FigoUser = event.params.as[FigoUser]
            Logger.info("createUser {}", user)
            FigoApi.createUserAndLogin(user)
          }
          f onSuccess {
            case v: TokenResponse => {
              Logger.info("TokenResponse createUser {}", Json.obj(
                "token" -> v.getAccessToken(),
                "expires" -> v.getExpiresIn().toString(),
                "refresh" -> v.getRefreshToken()
              ))
              event.out.get ! Json.obj(
                "name" -> event.name,
                "id" -> event.id,
                "data" -> Json.obj(
                  "token" -> v.getAccessToken(),
                  "expires" -> v.getExpiresIn().toString(),
                  "refresh" -> v.getRefreshToken()
                )
              )
            }

          }
          f onFailure {
            case m: FigoException => {
              Logger.info("Error on createUser {} {} {} {}", m.getCause(), m.getMessage(), m.getStackTrace().toString(), m.getErrorCode())
              sendGeneralError(event, Json.obj(
                "message" -> "User with this username or email is registered!",
                "code" -> m.getErrorCode(),
                "error" -> true
              ).toString())
            }
            case t => sendGeneralError(event, "User with this username or email is registered!")
          }
        }

        case "updateUser" => {
          val f: Future[FigoUser] = Future {
            Logger.info("updateUser params {}", event.params)
            val user: FigoUser = event.params.as[FigoUser]
            Logger.info("updateUser {}", user)
            FigoApi.updateUser(user)
          }
          f onSuccess {
            case u: FigoUser => broadcastWithSession("getUser", event.session_id)
          }
          f onFailure {
            case m: FigoException => {
              Logger.info("Error on updateUser {} {} {} {}", m.getCause(), m.getMessage(), m.getStackTrace().toString(), m.getErrorCode())
              sendGeneralError(event, "User is not updated!")
            }
            case t => sendGeneralError(event, "User is not updated!")
          }
        }


        case "getUser" => {
          val f: Future[FigoUser] = Future {
            Logger.info("getUser params {}", event.params)
            val user: FigoUser = event.params.as[FigoUser]
            Logger.info("getUser {}", user)
            FigoApi.getUser(user)
          }
          f onSuccess {
            case user => {
              Logger.info("User update getUser {}", Json.toJson(user))
              event.out.get ! Json.obj(
                "name" -> event.name,
                "id" -> event.id,
                "data" -> Json.toJson(user)
              )
            }

          }
          f onFailure {
            case m: FigoException => {
              Logger.info("Error on getUser FigoException {} {} {} {}", m.getMessage(), m.getStackTrace().toString(), m.getErrorCode())
              sendGeneralError(event, "User don't exist")
            }
            case t => sendGeneralError(event, "User don't exist " + t.getMessage())
          }
        }

        case "getAccounts" => {
          val f: Future[Set[FigoAccount]] = Future {
            Logger.info("getAccounts params {}", event.params)
            val user: FigoUser = event.params.as[FigoUser]
            Logger.info("getAccounts {}", user)
            FigoApi.getAccounts(user)
          }
          f onSuccess {
            case data => {
              Logger.info("getAcctounts result {}", data)
              event.out.get ! Json.obj(
                "name" -> event.name,
                "id" -> event.id,
                "data" -> Json.toJson(data)
              )
            }
          }
          f onFailure {
            case m: FigoException => {
              Logger.info("Error on getUser FigoException {} {} {} {}", m.getMessage(), m.getStackTrace().toString(), m.getErrorCode())
              sendGeneralError(event, "Error while fetching account data")
            }
            case t => sendGeneralError(event, "Error while fetching account data")
          }
        }

        case "linkBank" => {
          val f: Future[TaskStatusResponse] = Future {
            Logger.info("linkBank params {}", event.params)
            val link: FigoLinkBank = event.params.as[FigoLinkBank]
            Logger.info("linkBank {}", link)
            FigoApi.linkAccount(link)
          }
          f onSuccess {
            case data => {
              Logger.info("link account {}", data.toString())
              event.out.get ! Json.obj(
                "name" -> event.name,
                "id" -> event.id,
                "data" -> Json.obj(
                  "message" -> data.getMessage()
                )
              )
              broadcastWithSession("getAccounts", event.session_id)
            }
          }
          f onFailure {
            case m : FigoException => {
              Logger.info("Error on linking account {} {} {} {}", m.getMessage(), m.getLocalizedMessage(), m.getErrorCode(), m.getStackTrace())
              sendGeneralError(event, "while linking bank account")
            }
            case m => {
              Logger.info("Error on linking account {} {}", m.getMessage(), m.getStackTrace().toString())
              sendGeneralError(event, "while linking bank account")
            }
          }
        }
      }
    }
    /**
      * Send general error
      * @param event
      * @param message
      */
    def sendGeneralError(event: Event, message: String): Unit = {
      Logger.info("Error on  {}", message)
      event.out.get ! Json.obj(
        "name" -> event.name,
        "id" -> event.id,
        "data" -> Json.obj(
          "message" -> message,
          "error" -> true
        )
      )
    }
  }

  /**
    * Clean all events by session id
    * @param session_id
    */
  def clean(session_id: String) = {
    events.filterNot(elm => elm.session_id == session_id)
  }

  /**
    * Has event
    * @param e
    * @return
    */
  def has(e: Event): Boolean = events.find(i => i.id == e.id).isDefined

  /**
    * Get an event
    * @param e
    * @return
    */
  def get(e: Event): Event = events.find(i => i.id == e.id).get

  /**
    * Remove event
    * @param e
    */
  def remove(e: Event): Unit = {
    if (has(e)) {
      events.filterNot(elm => elm == get(e))
    }
  }
}


/**
  * Transport actor
  * @param eventHandler
  */
class TransportActor(out: ActorRef, eventHandler: EventHandler) extends Actor with ActorLogging {
  /**
    * Format event
    */
  /**
    * Lazy reads for recursive package
    */
  implicit lazy val eventReads: Reads[Event] = (
    (JsPath \ "name").read[String] and
      (JsPath \ "id").read[String] and
      (JsPath \ "params").read[JsValue] and
      (JsPath \ "terminate").read[Boolean] and
      (JsPath \ "session_id").read[String] and
      (JsPath \ "out").readNullable(null)
    ) (Event.apply _)


  def receive = LoggingReceive {
    case json: JsValue => {
      val event: Event = json.as[Event]
      event.out = Option(out)
      if (event.name == "unload") {
        eventHandler.handle(event)
        eventHandler.clean(event.session_id)
      } else {
        if (event.terminate) {
          eventHandler.remove(event)
        } else if (!eventHandler.has(event)) {
          eventHandler.add(event)
        }
        eventHandler.handle(event)
      }

    }
    case _ => Logger.error("Transport actor received wrong type")
  }
}

/**
  * Transport actor
  */
object TransportActor {

  def props(eventHandler: EventHandler)(out: ActorRef) =
    Props(new TransportActor(out: ActorRef, eventHandler))
}