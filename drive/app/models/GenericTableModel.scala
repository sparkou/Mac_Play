package models

import java.io.{ FileNotFoundException, IOException, File, PrintWriter }

import scalikejdbc.{ DB, _ }
import scalikejdbc.metadata.Table

import scala.xml.{ SAXException, Elem, Node, XML }

/**
 * Created by ouspark on 7/21/16.
 */
class GenericTableModel(tableName: String) {
  val name = tableName
  val columns = (DB.getTable(name).getOrElse(new Table(""))).columns
  val pks = for (col <- columns.filter(_.isPrimaryKey)) yield col.name
}

object GenericTableModel {
  def search(table: GenericTableModel)(implicit session: DBSession = AutoSession): List[GenericDataModel] = {
    val tableName = table.name
    val dataList: List[Map[String, Any]] = SQL(s"select * from ${tableName}").map(_.toMap).list.apply()
    val modelList: List[GenericDataModel] =
      for (data <- dataList) yield new GenericDataModel(tableName, table.pks /*(for (pk <- table.pks) yield data.get(pk)).hashCode().toString*/ , data)
    modelList
  }
}

class GenericDataModel(tableName: String, pks: List[String], data: Map[String, Any]) {
  override def toString = "Table: " + tableName + " --- Tag: " + tag + " --- Data: " + data.toMap
  val name = tableName
  val tag = tableName + "#" + pks.zip(for (pk <- pks) yield data(pk)).toMap.hashCode
  val cols = data
}

case class GenericTableRelationModel(tableName: String, relations: GenericRelationModel, subTables: List[GenericTableRelationModel]) {
  val name = tableName
  val relation = relations
  val subTable = subTables
  val columns = (DB.getTable(name).getOrElse(new Table(""))).columns
  val pks = for (col <- columns.filter(_.isPrimaryKey)) yield col.name
}
object GenericTableRelationModel {
  def getALLTables(domainTable: GenericTableRelationModel) = {
    //    val mainList = searchBy(domainTable, null)
    //    GenericDataModel.saveXML(domainTable.pks, mainList)
    //    for (data <- mainList) {
    //      for (subTable <- domainTable.subTables) {
    //        GenericDataModel.saveXML(subTable.pks, searchBy(subTable, data.cols))
    //      }
    //    }
  }

  def getTableData(table: GenericTableRelationModel, params: Map[String, Any], parentTag: String): Unit = {
    val dataList = searchBy(table, params)
    if (dataList.nonEmpty) {
      GenericDataModel.saveXML(table.pks, dataList, parentTag)
      for (data <- dataList) {
        if (table.subTables nonEmpty) {
          for (t <- table.subTables) {
            getTableData(t, data.cols, data.tag)
          }
        }
      }
    }
  }

  def searchBy(table: GenericTableRelationModel, params: Map[String, Any])(implicit session: DBSession = AutoSession): List[GenericDataModel] = {
    val tableName = table.name
    val sql = GenericRelationModel.sql(table.relation, params)
    val dataList: List[Map[String, Any]] = SQL(s"select * from ${tableName}" + sql).map(_.toMap).list.apply()
    val modelList: List[GenericDataModel] =
      for (data <- dataList) yield new GenericDataModel(tableName, table.pks, data)
    modelList
  }
}
case class GenericRelationModel(eqs: Map[String, String]) {
  val eq = eqs
}
object GenericRelationModel {
  def eq(relation: GenericRelationModel, params: Map[String, Any]) = (for (key <- relation.eq.keys) yield (s"${key} = ${params(relation.eq(key))}")).mkString(" AND ")
  def sql(relation: GenericRelationModel, params: Map[String, Any]) = if (relation == null) "" else " WHERE " + eq(relation, params)

}

object GenericDataModel {

  def col2XML(data: Map[String, Any]) = for (key <- data.keys) yield <col name={ key }>{ data(key) }</col>

  def data2XML(tag: String, data: Map[String, Any]) = {
    <data tag={ tag }>{ col2XML(data) }</data>
  }

  def toXML(pks: List[String], dataList: List[GenericDataModel], parentNode: Elem, parentTag: String) = {
    var root = parentNode
    if (root == null || root.isEmpty) {
      root = <root></root>
    }
    for (data <- dataList) {
      root = add2Root(root, data2XML(if (parentTag.isEmpty) data.tag else parentTag + "$" + data.tag, data.cols))
    }
    root
  }

  def add2Root(n: Node, newChild: Node) = n match {
    case Elem(prefix, label, attribs, scope, child @ _*) if (!child.exists(_.attributes("tag").text == newChild.attributes("tag").text)) =>
      Elem(prefix, label, attribs, scope, child.isEmpty, child ++ newChild: _*)
    case Elem(prefix, label, attribs, scope, child @ _*) if (child.exists(_.attributes("tag").text == newChild.attributes("tag").text)) =>
      Elem(prefix, label, attribs, scope, child.isEmpty, child: _*)
    case _ => throw new RuntimeException
  }

  def saveXML(pks: List[String], dataList: List[GenericDataModel], parentTag: String) = {
    if (!dataList.isEmpty) {
      val tableName = dataList.head.name
      try {
        val root = XML.loadFile(s"app/data/${tableName}.xml")
        XML.save(s"app/data/${tableName}.xml", toXML(pks, dataList, root, parentTag))
      } catch {
        case ex: FileNotFoundException => XML.save(s"app/data/${dataList.head.name}.xml", toXML(pks, dataList, null, parentTag))
        case ex: IOException => println("Had an IOException trying to read that file")
        case ex: SAXException => XML.save(s"app/data/${dataList.head.name}.xml", toXML(pks, dataList, null, parentTag))
      }
    }
  }
  def readXML(name: String) = {
    val root = XML.loadFile(s"app/data/${name}.xml")
    val writer = new PrintWriter(new File("app/data/test.txt"))
    writer.write(root.toString())
    writer.close()
  }
  def getData(tag: Int, name: String) = {
    val root = XML.loadFile(s"app/data/${name}.xml")
    val dataList = root \ "data"

    val list = for (data <- dataList) yield (data.attributes("tag").text, (for (col <- data \ "col") yield (col.attributes("name"), col.text)).toMap)
    val writer = new PrintWriter("app/data/data.txt")
    for (l <- list) {
      writer.write("Tag: " + l._1)
      writer.println
      writer.write("Data: " + l._2)
      writer.println
    }
    writer.close()
  }

}

