package scavlink.sbt.mavgen

import org.scalatest.WordSpec
import scavlink.sbt.mavgen.CommandDescriptionParser._

class CommandFunctionSpec extends WordSpec {
  "the description parser" should {
    "split on whitespace" in {
      assertField("whyDoesThisSplit", "why does this split")
    }

    "split on underscores" in {
      assertField("whyDoesThisSplit", "why_does_this_split")
    }

    "split on dashes, underscores, and whitespace" in {
      assertField("whyDoesThisSplit", "why----does__this \t split")
    }

    "camel-case correctly" in {
      assertResult("thisIsMyString") {
        List("this", "is", "my", "string").reduceLeft(camel)
      }
    }

    "stop at a comma" in {
      assertField("radiusAroundMission", "Radius around MISSION, meters. If positive loiter clockwise, else counter-clockwise")
    }

    "stop at a comma with no trailing space" in {
      assertField("radiusAroundMission", "Radius around MISSION,meters if positive loiter clockwise, else counter-clockwise")
    }

    "stop at a period" in {
      assertField("desiredYawAngle", "Desired yaw angle. for you")
    }

    "stop at a period with no trailing space" in {
      assertField("desiredYawAngle", "Desired yaw angle.for you")
    }

    "stop at a question" in {
      assertField("enabled", "enabled? yes or no")
    }

    "stop at a question with no trailing space" in {
      assertField("enabled", "enabled?yes or no")
    }

    "stop at an exclamation" in {
      assertField("enabled", "enabled! yes or no")
    }

    "stop at an exclamation with no trailing space" in {
      assertField("enabled", "enabled!yes or no")
    }

    "treat trailing slash as the word \"or\"" in {
      assertField("missionIndexOrTargetId", "MISSION index/ target ID. (see MAV_ROI enum)")
    }

    "treat leading slash as the word \"or\"" in {
      assertField("missionIndexOrTargetId", "MISSION index /target ID. (see MAV_ROI enum)")
    }

    "treat separate slash as the word \"or\"" in {
      assertField("missionIndexOrTargetId", "MISSION index / target ID. (see MAV_ROI enum)")
    }

    "stop at parentheses" in {
      assertField("action", "action (0=disable, 1=enable, 2=release, for some systems see PARACHUTE_ACTION enum, not in general message set.)")
      assertField("seconds", "seconds(decimal)")
    }

    "stop at a colon" in {
      assertField("speedDuringYawChange", "speed during yaw change:[deg per second]")
    }

    "stop at the word \"in\"" in {
      assertField("holdTime", "Hold time in decimal seconds. (ignored by fixed wing, time to stay at MISSION for rotary wing)")
    }

    "leave in stop words if specified" in {
      assertResult("holdTimeInDecimalSeconds") {
        parse("Hold time in decimal seconds. (ignored by fixed wing, time to stay at MISSION for rotary wing)", "fallback", true)
      }
    }

    "remove illegal characters from name parts" in {
      assertField("zoomAbsolutePosition", "Zoom's absolute position")
    }

    "include trailing number" in {
      assertField("genericDimension4", "Generic dimension 4, in the sensor's raw units")
    }

    "use the fallback name if the first character is a digit" in {
      assertField("fallback", "0: Disable local obstacle avoidance / local path planning (without resetting map), 1: Enable local path planning, 2: Enable and reset local path planning")
    }
  }

  def assertField(expected: String, description: String) =
    assertResult(expected) {
      parse(description, "fallback")
    }
}
