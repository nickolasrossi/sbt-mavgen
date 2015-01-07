package scavlink.sbt.mavgen

import org.scalatest.{Matchers, WordSpec}
import scavlink.sbt.mavgen.EnumGenerator._

class EnumFunctionSpec extends WordSpec with Matchers {
  def value(n: Int) = EnumValue("val" + n, "value " + n, n.toString)
  def altValue(n: Int) = EnumValue("ALT_val" + n, "value " + n, n.toString)

  val values3 = Seq(value(1), value(2), value(3))

  val enums2 = Seq(
    Enum("EnumA", "enum a", "common", values3, isFlag = false),
    Enum("EnumB", "enum b", "common", values3, isFlag = false)
  )

  "mergeEnum" should {
    "append the new enum if there is no duplicate in the list" in {
      val ne = Enum("EnumC", "enum c", "common", values3, isFlag = false)
      val expected = enums2 :+ ne

      assertResult(expected) {
        mergeEnum(enums2, ne)
      }
    }

    "merge the new enum if there is a duplicate" in {
      val altValues = Seq(value(10), value(11))
      val ne = Enum("EnumB", "enum b", "alt", altValues, isFlag = false)

      val expected = Seq(
        enums2(0),
        Enum("EnumB", "enum b", "common", values3 ++ Seq(altValue(10), altValue(11)), isFlag = false)
      )

      assertResult(expected) {
        mergeEnum(enums2, ne)
      }
    }
  }

  "addUnknown" should {
    "add a zero value if one doesn't exist" in {
      val expected = enums2.map(e => e.copy(values = EnumValue("_UNKNOWN", "", "0") +: e.values))
      assertResult(expected) {
        addUnknown(enums2)
      }
    }

    "not add a zero value if one exists" in {
      val values = Seq(
        EnumValue("val0", "value 0", "0"),
        EnumValue("val1", "value 1", "1"),
        EnumValue("val2", "value 2", "2"),
        EnumValue("val3", "value 3", "3")
      )

      val enums = Seq(
        enums2(0).copy(values = values),
        enums2(1)
      )

      val expected = Seq(
        enums(0),
        enums(1).copy(values = EnumValue("_UNKNOWN", "", "0") +: enums(1).values)
      )

      assertResult(expected) {
        addUnknown(enums)
      }
    }
  }

  "mergeValue" should {
    "add the enum value if it isn't in the list" in {
      val expected = values3 :+ value(4)
      assertResult(expected) {
        mergeValue(values3, value(4))
      }
    }

    "not add the enum value if it's in the list" in {
      assertResult(values3) {
        mergeValue(values3, value(3))
      }
    }
  }

  "mergedValueName" should {
    "prepend uppercase bundle name" in {
      mergedValueName("common", "val1") shouldBe "COMMON_val1"
    }

    "not duplicate an underscore" in {
      mergedValueName("common", "_val1") shouldBe "COMMON_val1"
    }
  }

  "chopPrefix" should {
    "remove the prefix string from every value name" in {
      val expected = Seq(
        EnumValue("l1", "value 1", "1"),
        EnumValue("l2", "value 2", "2"),
        EnumValue("l3", "value 3", "3")
      )

      assertResult(expected) {
        chopPrefix("va")(values3)
      }
    }

    "remove only the parts of the prefix that match" in {
      val values = Seq(
        EnumValue("val1", "value 1", "1"),
        EnumValue("vest", "vest", "99"),
        EnumValue("not", "not", "100")
      )

      val expected = Seq(
        EnumValue("l1", "value 1", "1"),
        EnumValue("est", "vest", "99"),
        EnumValue("not", "not", "100")
      )

      assertResult(expected) {
        chopPrefix("va")(values)
      }
    }
  }

  "trimHeadWords" should {
    "remove any common words that all values start with" in {
      val values = Seq(
        EnumValue("this_is_val1", "value 1", "1"),
        EnumValue("this_is_val2", "value 2", "2"),
        EnumValue("this_is_val3", "value 3", "3")
      )

      val expected = Seq(
        EnumValue("val1", "value 1", "1"),
        EnumValue("val2", "value 2", "2"),
        EnumValue("val3", "value 3", "3")
      )

      assertResult(expected) {
        trimHeadWords(values)
      }
    }
  }

  "fixDigits" should {
    "append an underscore to enum values that start with a digit" in {
      val values = Seq(
        EnumValue("1", "1", "1"),
        value(2)
      )

      val expected = Seq(
        EnumValue("_1", "1", "1"),
        value(2)
      )

      assertResult(expected) {
        fixDigits(values)
      }
    }
  }

  "fillValues" should {
    "fill in unspecified numeric values" in {
      val values = Seq(
        EnumValue("val1", "value 1", ""),
        EnumValue("val2", "value 2", ""),
        EnumValue("val3", "value 3", "")
      )

      val expected = Seq(
        EnumValue("val1", "value 1", "1"),
        EnumValue("val2", "value 2", "2"),
        EnumValue("val3", "value 3", "3")
      )

      assertResult(expected) {
        fillValues(values)
      }
    }

    "fill unspecified values with the previous value plus 1" in {
      val values = Seq(
        EnumValue("val1", "value 1", ""),
        EnumValue("val2", "value 2", "8"),
        EnumValue("val3", "value 3", "")
      )

      val expected = Seq(
        EnumValue("val1", "value 1", "1"),
        EnumValue("val2", "value 2", "8"),
        EnumValue("val3", "value 3", "9")
      )

      assertResult(expected) {
        fillValues(values)
      }
    }

    "not duplicate values when filling" in {
      val values = Seq(
        EnumValue("val1", "value 1", ""),
        EnumValue("val2", "value 2", "1"),
        EnumValue("val3", "value 3", "")
      )

      val expected = Seq(
        EnumValue("val1", "value 1", "2"),
        EnumValue("val2", "value 2", "1"),
        EnumValue("val3", "value 3", "3")
      )

      assertResult(expected) {
        fillValues(values)
      }
    }
  }

  "parseNumeric" should {
    "parse decimal" in {
      parseNumeric("0") shouldBe 0
      parseNumeric("1") shouldBe 1
      parseNumeric("10") shouldBe 10
      parseNumeric("9999") shouldBe 9999
    }

    "parse hex" in {
      parseNumeric("0x1") shouldBe 1
      parseNumeric("0xc") shouldBe 12
      parseNumeric("0x0c") shouldBe 12
      parseNumeric("0xe1") shouldBe 225
      parseNumeric("0xa492") shouldBe 42130
    }

    "parse binary" in {
      parseNumeric("0b1") shouldBe 1
      parseNumeric("0b11100001") shouldBe 225
      parseNumeric("0b1010010010010010") shouldBe 42130
    }
  }

  "isFlag" should {
    "return true if all values are single bits" in {
      val values = Seq(
        EnumValue("val1", "value 1", "1"),
        EnumValue("val2", "value 2", "2"),
        EnumValue("val4", "value 4", "4"),
        EnumValue("val32768", "value 32768", "32768")
      )

      assert(isFlag(values))
    }

    "return false if any value is not a single bit" in {
      assert(!isFlag(values3))
    }
  }
}
