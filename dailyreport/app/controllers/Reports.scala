package controllers

import javax.inject.Singleton

import org.joda.time.DateTime
import org.slf4j.{Logger, LoggerFactory}
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json._
import play.api.mvc._
import play.modules.reactivemongo.MongoController
import play.modules.reactivemongo.json.collection.JSONCollection
import reactivemongo.api.Cursor

import scala.concurrent.Future

/**
 * The Users controllers encapsulates the Rest endpoints and the interaction with the MongoDB, via ReactiveMongo
 * play plugin. This provides a non-blocking driver for mongoDB as well as some useful additions for handling JSon.
 * @see https://github.com/ReactiveMongo/Play-ReactiveMongo
 */
@Singleton
class Reports extends Controller with MongoController {

  private final val logger: Logger = LoggerFactory.getLogger(classOf[Reports])

  /*
   * Get a JSONCollection (a Collection implementation that is designed to work
   * with JsObject, Reads and Writes.)
   * Note that the `collection` is not a `val`, but a `def`. We do _not_ store
   * the collection reference to avoid potential problems in development with
   * Play hot-reloading.
   */
  def collection: JSONCollection = db.collection[JSONCollection]("reports")

  // ------------------------------------------ //
  // Using case classes + Json Writes and Reads //
  // ------------------------------------------ //

  import models.JsonFormats2._
  import models._

  def createUser = Action.async(parse.json) {
    request =>
    /*
     * request.body is a JsValue.
     * There is an implicit Writes that turns this JsValue as a JsObject,
     * so you can call insert() with this JsValue.
     * (insert() takes a JsObject as parameter, or anything that can be
     * turned into a JsObject using a Writes.)
     */
      request.body.validate[Report].map {
        rpt =>
        // `user` is an instance of the case class `models.User`
          collection.insert(rpt).map {
            lastError =>
              logger.debug(s"Successfully inserted with LastError: $lastError")
              Created(s"User Created")
          }
      }.getOrElse(Future.successful(BadRequest("invalid json")))
  }

  def updateUser(name: String, date: String) = Action.async(parse.json) {
    request =>
      request.body.validate[Report].map {
        rpt =>
          // find our user by first name and last name
          val nameSelector = Json.obj("name" -> name, "date" -> date)
          collection.update(nameSelector, rpt).map {
            lastError =>
              logger.debug(s"Successfully updated with LastError: $lastError")
              Created(s"User Updated")
          }
      }.getOrElse(Future.successful(BadRequest("invalid json")))
  }

  def findUsers = Action.async {
    // let's do our query
    val date = (new DateTime()).toString().substring(0, 10)
    val cursor: Cursor[Report] = collection.
      // find all
      find(Json.obj("date" -> date)).
      // sort them by creation date
      sort(Json.obj("name" -> -1)).
      // perform the query and get a cursor of JsObject
      cursor[Report]

    // gather all the JsObjects in a list
    val futureUsersList: Future[List[Report]] = cursor.collect[List]()

    // transform the list into a JsArray
    val futurePersonsJsonArray: Future[JsArray] = futureUsersList.map { rpts =>
      Json.arr(rpts)
    }
    // everything's ok! Let's reply with the array
    futurePersonsJsonArray.map {
      rpts =>
        Ok(rpts(0))
    }
  }

  def deleteReport(name: String, date: String) = Action {
    val nameSelector = Json.obj("name" -> name, "date" -> date)
    collection.remove(nameSelector).map{
      lastError =>
        logger.debug(s"Successfully delete with LastError: $lastError")
        Created(s"Report Deleted")
    }
    Ok("200")
  }

}
