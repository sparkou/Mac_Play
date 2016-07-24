package controllers

import java.io.{ File, PrintWriter }
import javax.inject.{ Inject, Singleton }

import com.github.tototoshi.play2.json4s.native._
import models._
import org.json4s._
import org.json4s.ext.JodaTimeSerializers
import play.api.data.Forms._
import play.api.data._
import play.api.data.validation.Constraints._
import play.api.mvc._

@Singleton
class Companies @Inject() (json4s: Json4s) extends Controller {

  import json4s._
  implicit val formats = DefaultFormats ++ JodaTimeSerializers.all

  def all = Action {

    val relation = new GenericRelationModel(Map("programmer_id" -> "ID"))
    val skillTable = new GenericTableRelationModel("programmer_skill", relation, null)
    val mainTable = new GenericTableRelationModel("programmer", null, List(skillTable))

    //    val table = new GenericTableModel("programmer")
    //    val dataList: List[GenericDataModel] = GenericTableModel.search(table)

    //    for (data <- dataList) {

    //      GenericTableModel.searchBy()
    //    }
    GenericTableRelationModel.getALLTables(mainTable)
    //    GenericDataModel.saveXML(table.pks, dataList)
    //    GenericDataModel.readXML(table.name)
    //    GenericDataModel.getData(0, "company")

    Ok(Extraction.decompose(Company.findAll))
  }

  def show(id: Long) = Action {
    Company.find(id) match {
      case Some(company) => Ok(Extraction.decompose(company))
      case _ => NotFound
    }
  }

  case class CompanyForm(name: String, url: Option[String] = None)

  private val companyForm = Form(
    mapping(
      "name" -> text.verifying(nonEmpty),
      "url" -> optional(text)
    )(CompanyForm.apply)(CompanyForm.unapply)
  )

  def create = Action { implicit req =>
    companyForm.bindFromRequest.fold(
      formWithErrors => BadRequest("invalid parameters"),
      form => {
        val company = Company.create(name = form.name, url = form.url)
        Created.withHeaders(LOCATION -> s"/companies/${company.id}")
        NoContent
      }
    )
  }

  def delete(id: Long) = Action {
    Company.find(id) match {
      case Some(company) =>
        company.destroy()
        NoContent
      case _ => NotFound
    }
  }

}
