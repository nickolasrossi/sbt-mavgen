package scavlink.sbt.mavgen

import org.scalatest.WordSpec
import scavlink.sbt.mavgen.TestXmls._

import scala.xml.Node

class BundleGeneratorSpec extends WordSpec with GeneratorSpec {
  val generatorTest = GeneratorTest(
    BundleGenerator,
    "Bundle",
    "Bundle",
    "",
    node => commonXml
  )

  def find(name: String): Option[Node] = Some(commonXml)
}
