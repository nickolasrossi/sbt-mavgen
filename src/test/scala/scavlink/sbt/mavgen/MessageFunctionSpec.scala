package scavlink.sbt.mavgen

import org.scalatest.{Matchers, WordSpec}
import scavlink.sbt.mavgen.MessageGenerator._

class MessageFunctionSpec extends WordSpec with Matchers {
  "ordName" should {
    "convert a number less than 26 to a single character" in {
      caseName(0) shouldBe "a"
      caseName(12) shouldBe "m"
      caseName(25) shouldBe "z"
    }

    "convert a number larger than 26 to a string of characters" in {
      caseName(26) shouldBe "aa"
      caseName(28) shouldBe "ac"
      caseName(57) shouldBe "bf"
      caseName(701) shouldBe "zz"
      caseName(702) shouldBe "aaa"
      caseName(18277) shouldBe "zzz"
    }
  }

  "parseType" should {
    "extract a single-valued field type" in {
      parseType("int8_t", "") shouldBe FieldType("int8_t", "", 1)
    }

    "treat mavlink_version as a plain uint8" in {
      parseType("uint8_t_mavlink_version", "") shouldBe FieldType("uint8_t", "", 1)
    }

    "parse an array type" in {
      parseType("float[12]", "") shouldBe FieldType("float", "", 12)
    }

    "parse an enum type" in {
      parseType("uint8_t", "MAV_CMD") shouldBe FieldType("uint8_t", "MAV_CMD", 1)
    }

    "parse an enum array type" in {
      parseType("uint8_t[6]", "MAV_CMD") shouldBe FieldType("uint8_t", "MAV_CMD", 6)
    }

    "zero out the enum type if it's MAV_MODE" in {
      parseType("uint8_t", "MAV_MODE") shouldBe FieldType("uint8_t", "", 1)
    }
  }

  "scalaTypeOf" should {
    "treat uint8_t as Byte" in {
      scalaTypeOf(FieldType("uint8_t", "", 1)) shouldBe ScalaType("Byte", "0")
    }

    "treat uint16_t as Short" in {
      scalaTypeOf(FieldType("uint16_t", "", 1)) shouldBe ScalaType("Short", "0")
    }

    "treat uint32_t as Int" in {
      scalaTypeOf(FieldType("uint32_t", "", 1)) shouldBe ScalaType("Int", "0")
    }

    "treat enum as an enum type" in {
      scalaTypeOf(FieldType("uint8_t", "MAV_CMD", 1)) shouldBe ScalaType("MavCmd.Value", "MavCmd(0)")
    }

    "treat an array <= 4 elements as a Tuple" in {
      scalaTypeOf(FieldType("uint32_t", "", 4)) shouldBe ScalaType("(Int,Int,Int,Int)", "(0,0,0,0)")
    }

    "treat an array > 4 elements as a Vector" in {
      scalaTypeOf(FieldType("uint32_t", "", 9)) shouldBe ScalaType("Vector[Int]", "Vector.fill(9)(0)")
    }

    "treat a char array as a String" in {
      scalaTypeOf(FieldType("char", "", 5)) shouldBe ScalaType("String", "\"\"")
    }
  }

  "collapseXYZ" should {
    "collapse contiguous fields that end in x/y/z" in {
      val fields = Seq(
        Field("targetSystem", "a", FieldType("uint8_t", "", 1), ScalaType("Byte", "0"), "target system"),
        Field("targetComponent", "b", FieldType("uint8_t", "", 1), ScalaType("Byte", "0"), "target component"),
        Field("posX", "c", FieldType("float", "", 1), ScalaType("Float", "0"), "position x"),
        Field("posY", "d", FieldType("float", "", 1), ScalaType("Float", "0"), "position y"),
        Field("posZ", "e", FieldType("float", "", 1), ScalaType("Float", "0"), "position z"),
        Field("time", "f", FieldType("uint32_t", "", 1), ScalaType("Int", "0"), "time since boot")
      )

      val expected = Seq(
        Field("targetSystem", "a", FieldType("uint8_t", "", 1), ScalaType("Byte", "0"), "target system"),
        Field("targetComponent", "b", FieldType("uint8_t", "", 1), ScalaType("Byte", "0"), "target component"),
        Field("posXYZ", "c", FieldType("float", "", 3), ScalaType("(Float,Float,Float)", "(0,0,0)"), "position xyz"),
        Field("time", "f", FieldType("uint32_t", "", 1), ScalaType("Int", "0"), "time since boot")
      )

      assertResult(expected) {
        collapseXYZ(fields)
      }
    }

    "collapse contiguous fields that are named only x/y/z" in {
      val fields = Seq(
        Field("targetSystem", "a", FieldType("uint8_t", "", 1), ScalaType("Byte", "0"), "target system"),
        Field("targetComponent", "b", FieldType("uint8_t", "", 1), ScalaType("Byte", "0"), "target component"),
        Field("x", "c", FieldType("float", "", 1), ScalaType("Float", "0"), "position x"),
        Field("y", "d", FieldType("float", "", 1), ScalaType("Float", "0"), "position y"),
        Field("z", "e", FieldType("float", "", 1), ScalaType("Float", "0"), "position z"),
        Field("time", "f", FieldType("uint32_t", "", 1), ScalaType("Int", "0"), "time since boot")
      )

      val expected = Seq(
        Field("targetSystem", "a", FieldType("uint8_t", "", 1), ScalaType("Byte", "0"), "target system"),
        Field("targetComponent", "b", FieldType("uint8_t", "", 1), ScalaType("Byte", "0"), "target component"),
        Field("xyz", "c", FieldType("float", "", 3), ScalaType("(Float,Float,Float)", "(0,0,0)"), "position xyz"),
        Field("time", "f", FieldType("uint32_t", "", 1), ScalaType("Int", "0"), "time since boot")
      )

      assertResult(expected) {
        collapseXYZ(fields)
      }
    }

    "return the list as is if an xyz group isn't found" in {
      val fields = Seq(
        Field("targetSystem", "a", FieldType("uint8_t", "", 1), ScalaType("Byte", "0"), "target system"),
        Field("targetComponent", "b", FieldType("uint8_t", "", 1), ScalaType("Byte", "0"), "target component"),
        Field("x", "c", FieldType("float", "", 1), ScalaType("Float", "0"), "position x"),
        Field("y", "d", FieldType("float", "", 1), ScalaType("Float", "0"), "position y"),
        Field("time", "e", FieldType("uint32_t", "", 1), ScalaType("Int", "0"), "time since boot")
      )

      assertResult(fields) {
        collapseXYZ(fields)
      }
    }
  }

  "mergeSequence" should {
    "combine a group of fields ending in sequential numbers starting at 0" in {
      val fields = Seq(
        Field("targetSystem", "a", FieldType("uint8_t", "", 1), ScalaType("Byte", "0"), "target system"),
        Field("targetComponent", "b", FieldType("uint8_t", "", 1), ScalaType("Byte", "0"), "target component"),
        Field("data0", "c", FieldType("float", "", 1), ScalaType("Float", "0"), "data value 0"),
        Field("data1", "d", FieldType("float", "", 1), ScalaType("Float", "0"), "data value 1"),
        Field("data2", "e", FieldType("float", "", 1), ScalaType("Float", "0"), "data value 2"),
        Field("data3", "f", FieldType("float", "", 1), ScalaType("Float", "0"), "data value 3"),
        Field("data4", "g", FieldType("float", "", 1), ScalaType("Float", "0"), "data value 4"),
        Field("data5", "h", FieldType("float", "", 1), ScalaType("Float", "0"), "data value 5"),
        Field("time", "i", FieldType("uint32_t", "", 1), ScalaType("Int", "0"), "time since boot")
      )

      val expected = Seq(
        Field("targetSystem", "a", FieldType("uint8_t", "", 1), ScalaType("Byte", "0"), "target system"),
        Field("targetComponent", "b", FieldType("uint8_t", "", 1), ScalaType("Byte", "0"), "target component"),
        Field("data", "c", FieldType("float", "", 6), ScalaType("Vector[Float]", "Vector.fill(6)(0)"), "data value 0-5"),
        Field("time", "i", FieldType("uint32_t", "", 1), ScalaType("Int", "0"), "time since boot")
      )

      assertResult(expected) {
        collapseFields(mergeSequence)(fields)
      }
    }

    "combine a group of fields ending in sequential numbers starting at 1" in {
      val fields = Seq(
        Field("targetSystem", "a", FieldType("uint8_t", "", 1), ScalaType("Byte", "0"), "target system"),
        Field("targetComponent", "b", FieldType("uint8_t", "", 1), ScalaType("Byte", "0"), "target component"),
        Field("data1", "c", FieldType("float", "", 1), ScalaType("Float", "0"), "data value 1"),
        Field("data2", "d", FieldType("float", "", 1), ScalaType("Float", "0"), "data value 2"),
        Field("data3", "e", FieldType("float", "", 1), ScalaType("Float", "0"), "data value 3"),
        Field("data4", "f", FieldType("float", "", 1), ScalaType("Float", "0"), "data value 4"),
        Field("data5", "g", FieldType("float", "", 1), ScalaType("Float", "0"), "data value 5"),
        Field("data6", "h", FieldType("float", "", 1), ScalaType("Float", "0"), "data value 6"),
        Field("time", "i", FieldType("uint32_t", "", 1), ScalaType("Int", "0"), "time since boot")
      )

      val expected = Seq(
        Field("targetSystem", "a", FieldType("uint8_t", "", 1), ScalaType("Byte", "0"), "target system"),
        Field("targetComponent", "b", FieldType("uint8_t", "", 1), ScalaType("Byte", "0"), "target component"),
        Field("data", "c", FieldType("float", "", 6), ScalaType("Vector[Float]", "Vector.fill(6)(0)"), "data value 1-6"),
        Field("time", "i", FieldType("uint32_t", "", 1), ScalaType("Int", "0"), "time since boot")
      )

      assertResult(expected) {
        collapseFields(mergeSequence)(fields)
      }
    }

    "return the list as is if a sequence isn't found" in {
      val fields = Seq(
        Field("targetSystem", "a", FieldType("uint8_t", "", 1), ScalaType("Byte", "0"), "target system"),
        Field("targetComponent", "b", FieldType("uint8_t", "", 1), ScalaType("Byte", "0"), "target component"),
        Field("time", "c", FieldType("uint32_t", "", 1), ScalaType("Int", "0"), "time since boot")
      )

      assertResult(fields) {
        collapseFields(mergeSequence)(fields)
      }
    }
  }

  "conformTarget" should {
    "convert targetSystem and targetComponent fields into special types" in {
      val fields = Seq(
        Field("targetSystem", "a", FieldType("uint8_t", "", 1), ScalaType("Byte", "0"), "target system"),
        Field("targetComponent", "b", FieldType("uint8_t", "", 1), ScalaType("Byte", "0"), "target component"),
        Field("time", "c", FieldType("uint32_t", "", 1), ScalaType("Int", "0"), "time since boot")
      )

      val expected = Seq(
        Field("targetSystem", "a", FieldType("uint8_t", "", 1), ScalaType("SystemId", "0"), "target system"),
        Field("targetComponent", "b", FieldType("uint8_t", "", 1), ScalaType("ComponentId", "0"), "target component"),
        Field("time", "c", FieldType("uint32_t", "", 1), ScalaType("Int", "0"), "time since boot")
      )

      assertResult(expected) {
        expected.map(conformTarget)
      }
    }

    "fix the \"target\" variation on the target system field name" in {
      val fields = Seq(
        Field("target", "a", FieldType("uint8_t", "", 1), ScalaType("Byte", "0"), "target system"),
        Field("targetComponent", "b", FieldType("uint8_t", "", 1), ScalaType("Byte", "0"), "target component"),
        Field("time", "c", FieldType("uint32_t", "", 1), ScalaType("Int", "0"), "time since boot")
      )

      val expected = Seq(
        Field("targetSystem", "a", FieldType("uint8_t", "", 1), ScalaType("SystemId", "0"), "target system"),
        Field("targetComponent", "b", FieldType("uint8_t", "", 1), ScalaType("ComponentId", "0"), "target component"),
        Field("time", "c", FieldType("uint32_t", "", 1), ScalaType("Int", "0"), "time since boot")
      )

      assertResult(expected) {
        expected.map(conformTarget)
      }
    }

    "fix the \"targetSystemId\" variation on the target system field name" in {
      val fields = Seq(
        Field("targetSystemId", "a", FieldType("uint8_t", "", 1), ScalaType("Byte", "0"), "target system"),
        Field("targetComponent", "b", FieldType("uint8_t", "", 1), ScalaType("Byte", "0"), "target component"),
        Field("time", "c", FieldType("uint32_t", "", 1), ScalaType("Int", "0"), "time since boot")
      )

      val expected = Seq(
        Field("targetSystem", "a", FieldType("uint8_t", "", 1), ScalaType("SystemId", "0"), "target system"),
        Field("targetComponent", "b", FieldType("uint8_t", "", 1), ScalaType("ComponentId", "0"), "target component"),
        Field("time", "c", FieldType("uint32_t", "", 1), ScalaType("Int", "0"), "time since boot")
      )

      assertResult(expected) {
        expected.map(conformTarget)
      }
    }
  }
}
