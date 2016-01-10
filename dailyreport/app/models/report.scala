package models

case class Report( team: String,
                 name: String,
                 email: String,
                 date: String,
                 tasks: Seq[Task])

case class Task( caseNo: String,
                 caseLink: String,
                 caseTitle: String,
                 priority: String,
                 status: String,
                 jiraStatus: String,
                 caseType: String,
                 comments: String)

object JsonFormats2 {
  import play.api.libs.json.Json

  // Generates Writes and Reads for Feed and User thanks to Json Macros
  implicit val taskFormat = Json.format[Task]
  implicit val reportFormat = Json.format[Report]
}