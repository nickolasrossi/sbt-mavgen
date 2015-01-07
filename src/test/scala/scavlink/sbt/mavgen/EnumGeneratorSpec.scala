package scavlink.sbt.mavgen

import org.scalatest.WordSpec
import scavlink.sbt.mavgen.EnumGenerator._
import scavlink.sbt.mavgen.TestXmls._

import scala.xml.Node

class EnumGeneratorSpec extends WordSpec with GeneratorSpec {
  val enumNodes = commonXml \ "enums" \ "enum"

  val generatorTest = GeneratorTest(
    EnumGenerator,
    "Enum",
    "MavResult",
    "MAV_RESULT",
    node => <mavlink>
      <enums>
        {node}
      </enums>
    </mavlink>
  )

  "unmarshal" should {
    "unmarshal a valid enum from xml" in {
      val expectedValues = Seq(
        EnumValue("ACCEPTED", "Command ACCEPTED and EXECUTED", "0"),
        EnumValue("TEMPORARILY_REJECTED", "Command TEMPORARY REJECTED/DENIED", "1"),
        EnumValue("DENIED", "Command PERMANENTLY DENIED", "2"),
        EnumValue("UNSUPPORTED", "Command UNKNOWN/UNSUPPORTED", "3"),
        EnumValue("FAILED", "Command executed, but failed", "4")
      )

      val expected = Enum("MavResult", "result from a mavlink command", "common", expectedValues, isFlag = false)

      assertEnum("MAV_RESULT", expected)
    }

    "unmarshal all enums without an exception" in {
      assertResult(30) {
        unmarshal("common", commonXml).length
      }
    }

    def assertEnum(name: String, expected: Enum) =
      find(name) match {
        case Some(node) =>
          assertResult(expected) {
            unmarshalOne("common", node)
          }

        case None =>
          fail(s"Unable to find $name in xml")
      }
  }

  def find(name: String): Option[Node] = enumNodes.find(n => (n \ "@name").text == name)
}
