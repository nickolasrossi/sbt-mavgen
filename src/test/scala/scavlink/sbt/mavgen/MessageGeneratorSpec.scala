package scavlink.sbt.mavgen

import org.scalatest.WordSpec
import scavlink.sbt.mavgen.MessageGenerator._
import scavlink.sbt.mavgen.TestXmls._

import scala.xml.Node

class MessageGeneratorSpec extends WordSpec with GeneratorSpec {
  val messageNodes = commonXml \ "messages" \ "message"

  val generatorTest = GeneratorTest(
    MessageGenerator,
    "Message",
    "Message",
    "PARAM_VALUE",
    node => <mavlink>
      <messages>
        {node}
      </messages>
    </mavlink>
  )

  "unmarshal" should {
    "unmarshal a valid heartbeat message from xml" in {
      val expectedFields = Seq(
        Field("`type`", "a", FieldType("uint8_t", "", 1), ScalaType("Byte", "0"), "Type of the MAV (quadrotor, helicopter, etc., up to 15 types, defined in MAV_TYPE ENUM)"),
        Field("autopilot", "b", FieldType("uint8_t", "", 1), ScalaType("Byte", "0"), "Autopilot type / class. defined in MAV_AUTOPILOT ENUM"),
        Field("baseMode", "c", FieldType("uint8_t", "", 1), ScalaType("Byte", "0"), "System mode bitfield, see MAV_MODE_FLAG ENUM in mavlink/include/mavlink_types.h"),
        Field("customMode", "d", FieldType("uint32_t", "", 1), ScalaType("Int", "0"), "A bitfield for use for autopilot-specific flags."),
        Field("systemStatus", "e", FieldType("uint8_t", "", 1), ScalaType("Byte", "0"), "System status flag, see MAV_STATE ENUM"),
        Field("mavlinkVersion", "f", FieldType("uint8_t", "", 1), ScalaType("Byte", "0"), "MAVLink version, not writable by user, gets added by protocol because of magic data type: uint8_t_mavlink_version")
      )

      val marshalOrder = Seq(3, 0, 1, 2, 4, 5).map(expectedFields)

      val expected = Message(0,
        "HEARTBEAT",
        "Heartbeat",
        "The heartbeat message shows that a system is present and responding. The type of the MAV and Autopilot hardware allow the receiving system to treat further messages from this system appropriate (e.g. by laying out the user interface based on the autopilot).",
        "common",
        expectedFields,
        marshalOrder,
        50
      )

      assertMessage(expected)
    }

    "unmarshal a message with a Tuple field" in {
      val expectedFields = Seq(
        Field("timeBootMs", "a", FieldType("uint32_t", "", 1), ScalaType("Int", "0"), "Timestamp in milliseconds since system boot"),
        Field("targetSystem", "b", FieldType("uint8_t", "", 1), ScalaType("SystemId", "0"), "System ID"),
        Field("targetComponent", "c", FieldType("uint8_t", "", 1), ScalaType("ComponentId", "0"), "Component ID"),
        Field("typeMask", "d", FieldType("uint8_t", "", 1), ScalaType("Byte", "0"), "Mappings: If any of these bits are set, the corresponding input should be ignored: bit 1: body roll rate, bit 2: body pitch rate, bit 3: body yaw rate. bit 4-bit 6: reserved, bit 7: throttle, bit 8: attitude"),
        Field("q", "e", FieldType("float", "", 4), ScalaType("(Float,Float,Float,Float)", "(0,0,0,0)"), "Attitude quaternion (w, x, y, z order, zero-rotation is 1, 0, 0, 0)"),
        Field("bodyRollRate", "f", FieldType("float", "", 1), ScalaType("Float", "0"), "Body roll rate in radians per second"),
        Field("bodyPitchRate", "g", FieldType("float", "", 1), ScalaType("Float", "0"), "Body roll rate in radians per second"),
        Field("bodyYawRate", "h", FieldType("float", "", 1), ScalaType("Float", "0"), "Body roll rate in radians per second"),
        Field("thrust", "i", FieldType("float", "", 1), ScalaType("Float", "0"), "Collective thrust, normalized to 0 .. 1 (-1 .. 1 for vehicles capable of reverse trust)")
      )

      val marshalOrder = Seq(0, 4, 5, 6, 7, 8, 1, 2, 3).map(expectedFields)

      val expected = Message(82,
        "SET_ATTITUDE_TARGET",
        "SetAttitudeTarget",
        "Set the vehicle attitude and body angular rates.",
        "common",
        expectedFields,
        marshalOrder,
        49
      )

      assertMessage(expected)
    }

    "unmarshal a message with string and enum fields" in {
      val expectedFields = Seq(
        Field("paramId", "a", FieldType("char", "", 16), ScalaType("String", "\"\""), "Onboard parameter id, terminated by NULL if the length is less than 16 human-readable chars and WITHOUT null termination (NULL) byte if the length is exactly 16 chars - applications have to provide 16+1 bytes storage if the ID is stored as string"),
        Field("paramValue", "b", FieldType("float", "", 1), ScalaType("Float", "0"), "Onboard parameter value"),
        Field("paramType", "c", FieldType("uint8_t", "MAV_PARAM_TYPE", 1), ScalaType("MavParamType.Value", "MavParamType(0)"), "Onboard parameter type: see the MAV_PARAM_TYPE enum for supported data types."),
        Field("paramCount", "d", FieldType("uint16_t", "", 1), ScalaType("Short", "0"), "Total number of onboard parameters"),
        Field("paramIndex", "e", FieldType("uint16_t", "", 1), ScalaType("Short", "0"), "Index of this onboard parameter")
      )

      val marshalOrder = Seq(1, 3, 4, 0, 2).map(expectedFields)

      val expected = Message(22,
        "PARAM_VALUE",
        "ParamValue",
        "Emit the value of a onboard parameter. The inclusion of param_count and param_index in the message allows the recipient to keep track of received parameters and allows him to re-request missing parameters after a loss or timeout.",
        "common",
        expectedFields,
        marshalOrder,
        220)

      assertMessage(expected)
    }

    "unmarshal all messages without an exception" in {
      assertResult(109) {
        unmarshal("common", commonXml).length
      }
    }

    def assertMessage(expected: Message) =
      find(expected.name) match {
        case Some(node) =>
          assertResult(expected) {
            unmarshalMessage("common", node)
          }

        case None =>
          fail(s"Unable to find ${ expected.name } in xml")
      }
  }

  def find(name: String): Option[Node] = messageNodes.find(n => (n \ "@name").text == name)
}
