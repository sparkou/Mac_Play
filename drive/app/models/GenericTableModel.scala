package models

import java.io.{ File, PrintWriter }

import scala.xml.{ Text, XML, Elem, Node }
import scalikejdbc.DB
import scalikejdbc.metadata.Table
import scalikejdbc._

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
  val tag = pks.zip(for (pk <- pks) yield data(pk)).toMap.hashCode()
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
    val mainList = searchBy(domainTable, domainTable.relation, null)
    GenericDataModel.saveXML(domainTable.pks, mainList)
    for (data <- mainList) {
      for (subTable <- domainTable.subTables) {
        GenericDataModel.saveXML(subTable.pks, searchBy(subTable, subTable.relation, data.cols))
      }
    }
  }

  def searchBy(table: GenericTableRelationModel, relation: GenericRelationModel, params: Map[String, Any])(implicit session: DBSession = AutoSession): List[GenericDataModel] = {
    val tableName = table.name
    val sql = GenericRelationModel.sql(relation, params)
    val dataList: List[Map[String, Any]] = SQL(s"select * from ${tableName}" + sql).map(_.toMap).list.apply()
    val modelList: List[GenericDataModel] =
      for (data <- dataList) yield new GenericDataModel(tableName, table.pks /*(for (pk <- table.pks) yield data.get(pk)).hashCode().toString*/ , data)
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

  def data2XML(tag: Int, data: Map[String, Any]) = {
    <data tag={ tag.toString }>{ col2XML(data) }</data>
  }
  def tag(pks: List[String], data: Map[String, Any]) = {
    pks.zip(for (pk <- pks) yield data(pk)).toMap.hashCode()
  }
  def toXML(pks: List[String], dataList: List[GenericDataModel]) = {
    var root = <root></root>
    for (data <- dataList) {
      root = add2Root(root, data2XML(tag(pks, data.cols), data.cols))
    }
    root
  }
  def add2Root(n: Node, newChild: Node) = n match {
    case Elem(prefix, label, attribs, scope, child @ _*) =>
      Elem(prefix, label, attribs, scope, child ++ nchild: _*)
    case _ => sys.error("Can only add children to elements")
  }

  def removeSame(child: Node, newChild: Node): Unit = {

  }
//  }
//  n match {
//      case Elem(prefix, label, attribs, scope, child @ _*) =>
//        if(!(newChild \ "data").isEmpty) {
//          for(child <- (newChild \ "data")) {
//            if(!(n \ "data").contains(child)) {
//              Elem(prefix, label, attribs, scope, child ++ newChild: _*)
//            } else {
//              Elem(prefix, label, attribs, scope, child ++ newChild: _*)
//            }
//          }
//        }
//      case _ => sys.error("Can only add children to elements!")
//    }
//  }
  def saveXML(pks: List[String], dataList: List[GenericDataModel]) = {
    if (!dataList.isEmpty) {
      var root = XML.loadFile(s"app/data/${dataList.head.name}.xml")
      if (root == null) {
        XML.save(s"app/data/${dataList.head.name}.xml", toXML(pks, dataList))
      } else {
        for (data <- dataList) {
          root = add2Root(root, data2XML(tag(pks, data.cols), data.cols))
        }
        XML.save(s"app/data/${dataList.head.name}.xml", root)
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

