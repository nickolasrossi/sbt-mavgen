package scavlink.sbt.mavgen

import java.io.File

import org.fusesource.scalate.{Template, TemplateEngine}
import org.scalatest.{Matchers, WordSpec}
import sbt.Logger

import scala.xml.Elem

class GeneratorFunctionSpec extends WordSpec with Matchers {
  val fixture = new Generator {
    def generate(xmls: Map[String, Elem], templates: Map[String, Template], outputPath: File)(implicit engine: TemplateEngine, log: Logger): Map[File, String] = ???
  }

  "camelClassName" should {
    "split on underscore and capitalize all parts" in {
      fixture.className("some_kind_of_name") shouldBe "SomeKindOfName"
    }

    "capitalize a single part" in {
      fixture.className("some") shouldBe "Some"
    }
  }

  "safeFieldName" should {
    "split on underscore and capitalize all but the first part" in {
      fixture.fieldName("some_kind_of_name") shouldBe "someKindOfName"
    }

    "not capitalize a single part" in {
      fixture.fieldName("some") shouldBe "some"
    }

    "put back-ticks around reserved word" in {
      fixture.reservedWords.foreach { s =>
        fixture.fieldName(s) shouldBe s"`$s`"
      }
    }
  }

  "parsePath" should {
    "parse a plain path" in {
      val base = new File("parent", "dir")
      val bundle = "common"
      val template = "MAVLink.message.enums.Enum"

      val (dir, pkg, name) = fixture.parsePath(base, bundle, template)
      dir shouldBe new File(new File(new File(base, "MAVLink"), "message"), "enums")
      pkg shouldBe "MAVLink.message.enums"
      name shouldBe "Enum"
    }

    "substitute the bundle for a single underscore" in {
      val base = new File("parent", "dir")
      val bundle = "common"
      val template = "MAVLink.message._.Messages"

      val (dir, pkg, name) = fixture.parsePath(base, bundle, template)
      dir shouldBe new File(new File(new File(base, "MAVLink"), "message"), bundle)
      pkg shouldBe "MAVLink.message.common"
      name shouldBe "Messages"
    }

    "substitute the bundle capitalized for a double underscore" in {
      val base = new File("parent", "dir")
      val bundle = "common"
      val template = "MAVLink.connection.marshal.__Marshaller"

      val (dir, pkg, name) = fixture.parsePath(base, bundle, template)
      dir shouldBe new File(new File(new File(base, "MAVLink"), "connection"), "marshal")
      pkg shouldBe "MAVLink.connection.marshal"
      name shouldBe "CommonMarshaller"
    }

    "substitute the bundle more than once" in {
      val base = new File("parent", "dir")
      val bundle = "common"
      val template = "MAVLink.connection._.__Marshaller"

      val (dir, pkg, name) = fixture.parsePath(base, bundle, template)
      dir shouldBe new File(new File(new File(base, "MAVLink"), "connection"), bundle)
      pkg shouldBe "MAVLink.connection.common"
      name shouldBe "CommonMarshaller"
    }
  }

  "scalaSource" should {
    "append the scala extension to the name" in {
      fixture.scalaSource("file") shouldBe "file.scala"
    }
  }
}
