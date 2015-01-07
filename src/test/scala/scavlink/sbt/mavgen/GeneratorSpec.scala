package scavlink.sbt.mavgen

import java.io.File

import org.fusesource.scalate.{TemplateEngine, TemplateSource}
import org.scalatest.WordSpec
import sbt.{BasicLogger, ControlEvent, Level, LogEvent}

import scala.io.Source
import scala.xml.{Elem, Node}

case class GeneratorTest(generator: Generator, baseName: String, outputName: String,
                         sampleNode: String, xmlWrapper: Elem => Elem)

/**
 * Provides an abstract test of the generate() function.
 */
trait GeneratorSpec {
  self: WordSpec =>

  val generatorTest: GeneratorTest
  val resourcePath = "generator-test"

  implicit val scalateEngine = new TemplateEngine
  scalateEngine.escapeMarkup = false

  implicit val logger = new BasicLogger {
    def control(event: ControlEvent.Value, message: => String): Unit = {}
    def logAll(events: Seq[LogEvent]): Unit = {}
    def log(level: Level.Value, message: => String): Unit = { println(message) }
    def success(message: => String): Unit = {}
    def trace(t: => Throwable): Unit = {}
  }

  def find(name: String): Option[Node]

  "generate" should {
    "produce valid output from a scalate template" in {
      val test = generatorTest

      val name = s"MAVLink.test.${ test.baseName }"
      val template = scalateEngine.load(TemplateSource.fromUri(s"/$resourcePath/$name.ssp", scalateEngine.resourceLoader))
      val outputPath = new File("parent")

      val expectedPath = new File(new File(new File(outputPath, "MAVLink"), "test"), s"${ test.outputName }.scala")
      val expectedText = Source.fromInputStream(getClass.getResourceAsStream(s"/$resourcePath/${ test.baseName }Output.txt")).mkString

      find(test.sampleNode) match {
        case Some(node: Elem) =>
          val output = test.generator.generate(Map("common" -> test.xmlWrapper(node)), Map(name -> template), outputPath)
          assertResult(1)(output.size)

          val (file, text) = output.head
          assertResult(expectedPath)(file)
          assertResult(expectedText)(text)

        case _ =>
          fail(s"Unable to find ${ test.sampleNode } element in xml")
      }
    }
  }
}
