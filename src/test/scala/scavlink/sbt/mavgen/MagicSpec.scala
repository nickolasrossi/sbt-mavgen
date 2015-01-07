package scavlink.sbt.mavgen

import org.scalatest.WordSpec
import scavlink.sbt.mavgen.MessageGenerator._

import scala.xml.XML

class MagicSpec extends WordSpec {
  val xml = XML.load(getClass.getResourceAsStream("/mavgen/common.xml"))

  // taken from ardupilot project
  val sourceMagics = Map(0 -> 50, 1 -> 124, 2 -> 137, 4 -> 237, 5 -> 217, 6 -> 104, 7 -> 119, 11 -> 89, 20 -> 214,
    21 -> 159, 22 -> 220, 23 -> 168, 24 -> 24, 25 -> 23, 26 -> 170, 27 -> 144, 28 -> 67, 29 -> 115, 30 -> 39, 31 -> 246,
    32 -> 185, 33 -> 104, 34 -> 237, 35 -> 244, 36 -> 222, 37 -> 212, 38 -> 9, 39 -> 254, 40 -> 230, 41 -> 28, 42 -> 28,
    43 -> 132, 44 -> 221, 45 -> 232, 46 -> 11, 47 -> 153, 48 -> 41, 49 -> 39, 50 -> 214, 51 -> 223, 52 -> 141, 53 -> 33,
    54 -> 15, 55 -> 3, 56 -> 100, 57 -> 24, 58 -> 239, 59 -> 238, 60 -> 30, 61 -> 153, 62 -> 183, 63 -> 51,
    64 -> 82, 65 -> 118, 66 -> 148, 67 -> 21, 69 -> 243, 70 -> 124, 73 -> 38, 74 -> 20, 75 -> 158, 76 -> 152,
    77 -> 143, 80 -> 127, 81 -> 106, 82 -> 49, 83 -> 22, 84 -> 143, 85 -> 140, 86 -> 5,
    87 -> 150, 89 -> 231, 90 -> 183, 91 -> 63, 92 -> 54, 100 -> 175, 101 -> 102, 102 -> 158, 103 -> 208, 104 -> 56,
    105 -> 93, 106 -> 138, 107 -> 108, 108 -> 32, 109 -> 185, 110 -> 84, 111 -> 34, 112 -> 124, 113 -> 124,
    114 -> 237, 115 -> 4, 116 -> 76, 117 -> 128, 118 -> 56, 119 -> 116, 120 -> 134, 121 -> 237, 122 -> 203, 123 -> 250,
    124 -> 87, 125 -> 203, 126 -> 220, 127 -> 25, 128 -> 226, 130 -> 29, 131 -> 223, 132 -> 85, 133 -> 6, 134 -> 229,
    135 -> 203, 136 -> 1, 147 -> 154, 148 -> 49, 149 -> 15, 248 -> 8, 249 -> 204, 250 -> 49, 251 -> 170,
    252 -> 44, 253 -> 83, 254 -> 46)

  "the magic number calculator" should {
    "compute magic numbers that match the values from droidplanner for all common messages" in {
      val messages = unmarshal("common", xml)
      val magics = messages.map(m => m.id -> m.magic)

      val mismatches =
        for ((id, magic) <- magics if sourceMagics.isDefinedAt(id) && sourceMagics(id) != magic)
        yield id ->(magic, sourceMagics(id))

      assertResult(Map.empty)(mismatches.toMap)
    }
  }
}
