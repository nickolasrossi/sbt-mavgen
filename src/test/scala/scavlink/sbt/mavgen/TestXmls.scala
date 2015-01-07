package scavlink.sbt.mavgen

import scala.xml.XML

object TestXmls {
  lazy val commonXml = XML.load(getClass.getResourceAsStream("/mavgen/common.xml"))
}
