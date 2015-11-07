package api

import akka.actor.{ActorLogging, ActorRef, Actor, Props}
import akka.event.LoggingReceive
import me.figo.FigoException
import me.figo.internal.TokenResponse
import play.Logger
import scala.concurrent.Future
import play.api.libs.json.Json.JsValueWrapper
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import scala.concurrent.ExecutionContext.Implicits.global
import play.api.Play.current
import models.{User, UserDAO}
import api.{FigoApi, FigoUser}

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
  /**
    * Lazy reads for recursive package
    */
  implicit lazy val userReads: Reads[User] = (
    (JsPath \ "user_id").readNullable[Int] and
      (JsPath \ "name").read[String] and
      (JsPath \ "session_id").readNullable[String]
    ) (User.apply _)
  /**
    * Lazy write for recursive package config
    */
  implicit lazy val userWrites: Writes[User] = (
    (JsPath \ "name").writeNullable[Int] and
      (JsPath \ "name").write[String] and
      (JsPath \ "session_id").writeNullable[String]
    ) (unlift(User.unapply))


  val userDAO: UserDAO = new UserDAO


  implicit lazy val userFigoWrites: Reads[FigoUser] = (
    (JsPath \ "username").readNullable[String] and
      (JsPath \ "password").readNullable[String] and
      (JsPath \ "email").readNullable[String] and
      (JsPath \ "name").readNullable[String] and
      (JsPath \ "address").readNullable[String] and
      (JsPath \ "token").readNullable[String]
    ) (FigoUser.apply _)

  /**
    * Events list
    */
  var events: Array[Event] = Array()


  /**
    * Resolve user
    * @param out actor reference
    * @param event client event
    * @param user user record
    * @return
    */
  def resolveUser(out: ActorRef, event: Event)(user: Option[User]) = {
    var name: String = null
    if (user.isDefined) {
      name = user.get.name
    }
    out ! Json.obj(
      "name" -> event.name,
      "id" -> event.id,
      "data" -> Json.obj(
        "isLoggedIn" -> user.isDefined,
        "name" -> name
      )
    )
  }


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
              event.out.get ! Json.obj(
                "name" -> event.name,
                "id" -> event.id,
                "data" -> Json.obj(
                  "message" -> "Wrong username or password!",
                  "code" -> m.getErrorCode(),
                  "error" -> true
                )
              )
            }
            case t => {
              println("Error on login {}", t)
              event.out.get ! Json.obj(
                "name" -> event.name,
                "id" -> event.id,
                "data" -> Json.obj(
                  "message" -> "Wrong username or password!",
                  "error" -> true
                )
              )
            }
          }
        }
        case "createUser" => {
          val f: Future[TokenResponse] = Future {
            Logger.info("createUser params {}", event.params)
            val user: FigoUser = event.params.as[FigoUser]
            Logger.info("createUser user {}", user)
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
              event.out.get ! Json.obj(
                "name" -> event.name,
                "id" -> event.id,
                "data" -> Json.obj(
                  "message" -> "User with this username or email is registered!",
                  "code" -> m.getErrorCode(),
                  "error" -> true
                )
              )
            }
            case t => {
              Logger.info("Error on createUser {}", t)
              event.out.get ! Json.obj(
                "name" -> event.name,
                "id" -> event.id,
                "data" -> Json.obj(
                  "message" -> "User with this username or email is registered!",
                  "error" -> true
                )
              )
            }
          }
        }

        case "updateUser" => {
          val f: Future[TokenResponse] = Future {
            Logger.info("updateUser params {}", event.params)
            val user: FigoUser = event.params.as[FigoUser]
            Logger.info("updateUser user {}", user)
            FigoApi.createUserAndLogin(user)
          }
          f onSuccess {
            case v: TokenResponse => {
              Logger.info("TokenResponse updateUser {}", Json.obj(
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
              Logger.info("Error on updateUser {} {} {} {}", m.getCause(), m.getMessage(), m.getStackTrace().toString(), m.getErrorCode())
              event.out.get ! Json.obj(
                "name" -> event.name,
                "id" -> event.id,
                "data" -> Json.obj(
                  "message" -> "User is not updated!",
                  "code" -> m.getErrorCode(),
                  "error" -> true
                )
              )
            }
            case t => {
              Logger.info("Error on updateUser {}", t)
              event.out.get ! Json.obj(
                "name" -> event.name,
                "id" -> event.id,
                "data" -> Json.obj(
                  "message" -> "User is not updated!",
                  "error" -> true
                )
              )
            }
          }
        }


        case "getUser" => {
          val f: Future[TokenResponse] = Future {
            Logger.info("updateUser params {}", event.params)
            val user: FigoUser = event.params.as[FigoUser]
            Logger.info("updateUser user {}", user)
            FigoApi.getUser(user)
          }
          f onSuccess {
            case v: TokenResponse => {
              Logger.info("TokenResponse getUser {}", Json.obj(
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
              Logger.info("Error on getUser {} {} {} {}", m.getCause(), m.getMessage(), m.getStackTrace().toString(), m.getErrorCode())
              event.out.get ! Json.obj(
                "name" -> event.name,
                "id" -> event.id,
                "data" -> Json.obj(
                  "message" -> "User don't exist",
                  "code" -> m.getErrorCode(),
                  "error" -> true
                )
              )
            }
            case t => {
              Logger.info("Error on getUser {}", t)
              event.out.get ! Json.obj(
                "name" -> event.name,
                "id" -> event.id,
                "data" -> Json.obj(
                  "message" -> "User don't exist",
                  "error" -> true
                )
              )
            }
          }
        }

      }
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