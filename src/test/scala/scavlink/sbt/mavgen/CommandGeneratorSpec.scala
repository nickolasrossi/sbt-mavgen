package scavlink.sbt.mavgen

import org.scalatest.WordSpec
import scavlink.sbt.mavgen.CommandGenerator._
import scavlink.sbt.mavgen.TestXmls._

import scala.xml.Node

class CommandGeneratorSpec extends WordSpec with GeneratorSpec {
  val cmdNodes = (commonXml \ "enums" \ "enum").filter(n => (n \ "@name").text == "MAV_CMD") \ "entry"

  val generatorTest = GeneratorTest(
    CommandGenerator,
    "Command",
    "Command",
    "16",
    node => <mavlink>
      <enums>
        <enum name="MAV_CMD">
          {node}
        </enum>
      </enums>
    </mavlink>
  )

  "unmarshal" should {
    "unmarshal a command with all parameters specified" in {
      val expectedParams = Seq(
        CommandParam("holdTime", 1, "Hold time in decimal seconds. (ignored by fixed wing, time to stay at MISSION for rotary wing)"),
        CommandParam("acceptanceRadius", 2, "Acceptance radius in meters (if the sphere with this radius is hit, the MISSION counts as reached)"),
        CommandParam("param3", 3, "0 to pass through the WP, if > 0 radius in meters to pass by WP. Positive value for clockwise orbit, negative value for counter-clockwise orbit. Allows trajectory control."),
        CommandParam("desiredYawAngle", 4, "Desired yaw angle at MISSION (rotary wing)"),
        CommandParam("location", 5, "Latitude / Longitude / Altitude", "Coordinates", "Geo()")
      )

      val expected = Command("NavWaypoint", "16", "common", "Navigate to MISSION.", expectedParams, true)

      assertCommand(expected)
    }

    "unmarshal a command with some parameters unspecified" in {
      val expectedParams = Seq(
        CommandParam("desiredYawAngle", 4, "Desired yaw angle."),
        CommandParam("location", 5, "Latitude / Longitude / Altitude", "Coordinates", "Geo()")
      )

      val expected = Command("NavLand", "21", "common", "Land at location", expectedParams, true)

      assertCommand(expected)
    }

    "unmarshal a command with x/y/z names in location position" in {
      val expectedParams = Seq(
        CommandParam("regionOfInteresetMode", 1, "Region of intereset mode. (see MAV_ROI enum)"),
        CommandParam("missionIndexOrTargetId", 2, "MISSION index/ target ID. (see MAV_ROI enum)"),
        CommandParam("roiIndex", 3, "ROI index (allows a vehicle to manage multiple ROI's)"),
        CommandParam("location", 5, "x the location of the fixed ROI (see MAV_FRAME) / y / z", "Coordinates", "Geo()")
      )

      val expected = Command("NavRoi", "80", "common", "Sets the region of interest (ROI) for a sensor set or the vehicle itself. This can then be used by the vehicles control system to control the vehicle attitude and the attitude of various sensors such as cameras.", expectedParams, true)

      assertCommand(expected)
    }

    "unmarshal a command without latitude/longitude/altitude" in {
      val expectedParams = Seq(
        CommandParam("delay", 1, "Delay in seconds (decimal)")
      )

      val expected = Command("ConditionDelay", "112", "common", "Delay mission state machine.", expectedParams, false)

      assertCommand(expected)
    }

    "unmarshal all commands without an exception" in {
      assertResult(57) {
        unmarshal("common", commonXml).length
      }
    }

    def assertCommand(expected: Command) =
      find(expected.id) match {
        case Some(node) =>
          assertResult(expected) {
            unmarshalOne("common", node)
          }

        case None =>
          fail(s"Unable to find ${ expected.id } in xml")
      }
  }

  def find(id: String): Option[Node] = cmdNodes.find(n => (n \ "@value").text == id)
}
