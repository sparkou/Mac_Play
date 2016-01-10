package models

case class User( eName: Option[String],
                 cName: Option[String],
                 email: Option[String],
               passwd: Option[String],
               phone: Option[String],
               qq: Option[String],
               department: Option[String],
               leader: Option[String],
               team: Option[String],
               position: Option[String],
               hireDate: Option[String],
               hireStatus: Option[String],
               toStatus: Option[String],
               holidays: Seq[Holiday],
                 active: Boolean)

case class Holiday(htype: Option[String],
                  remain: Option[String],
                   standard: Option[String],
                    rest: Option[String])

object JsonFormats {
  import play.api.libs.json.Json
  // Generates Writes and Reads for Feed and User thanks to Json Macros
  implicit val holidayFormat = Json.format[Holiday]
  implicit val userFormat = Json.format[User]
}