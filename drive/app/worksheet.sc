import scala.xml.{NodeBuffer, Elem, Node}
val l1 =List(1, 2, 3)
val l2 = List("1", "2", "3")
val l = l1.zip(l2).toMap

val xml = <root><data tag="915555227"><col name="ID">1</col><col name="NAME">Typesafe</col><col name="URL">http://typesafe.com/</col><col name="CREATED_AT">2016-07-20 00:04:52.246</col></data><data tag="-1821773821"><col name="ID">2</col><col name="NAME">Oracle</col><col name="URL">http://www.oracle.com/</col><col name="CREATED_AT">2016-07-20 00:04:52.246</col></data><data tag="-738382407"><col name="ID">3</col><col name="NAME">Google</col><col name="URL">http://www.google.com/</col><col name="CREATED_AT">2016-07-20 00:04:52.246</col></data><data tag="1681025371"><col name="URL">http://www.microsoft.com/</col><col name="ID">4</col><col name="CREATED_AT">2016-07-20 00:04:52.246</col><col name="NAME">Microsoft</col><col name="DELETED_AT">2016-07-23 13:59:33.026</col></data></root>
val childXML = <data tag="9155552271"><col name="ID">1</col><col name="NAME">Typesafe</col><col name="URL">http://typesafe.com/</col><col name="CREATED_AT">2016-07-20 00:04:52.246</col></data>
//  <data tag="-1821773821"><col name="ID">2</col><col name="NAME">Oracle</col><col name="URL">http://www.oracle.com/</col><col name="CREATED_AT">2016-07-20 00:04:52.246</col></data>
//  <data tag="-738382407"><col name="ID">3</col><col name="NAME">Google</col><col name="URL">http://www.google.com/</col><col name="CREATED_AT">2016-07-20 00:04:52.246</col></data>
//  <data tag="1681025371"><col name="URL">http://www.microsoft.com/</col><col name="ID">4</col><col name="CREATED_AT">2016-07-20 00:04:52.246</col><col name="NAME">Microsoft</col><col name="DELETED_AT">2016-07-23 13:59:33.026</col></data>


//val child = xml.child.filter(_.attributes("tag").text == "91555522").isEmpty
//val root = new Node
def add2Root(n: Node, newChild: Node) = n match {
  case Elem(prefix, label, attribs, scope, child @ _*) if(!child.exists(_.attributes("tag").text == newChild.attributes("tag").text)) =>
    Elem(prefix, label, attribs, scope, child.isEmpty, child ++ newChild : _*)
  case Elem(prefix, label, attribs, scope, child @ _*) if(child.exists(_.attributes("tag").text == newChild.attributes("tag").text)) =>
    Elem(prefix, label, attribs, scope, child.isEmpty, child : _*)
  case _ => throw new RuntimeException
}
add2Root(xml, childXML)

//xml.child.exists(_.attributes("tag").text == childXML.attributes("tag").text)
//root

