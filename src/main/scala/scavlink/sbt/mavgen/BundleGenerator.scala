package scavlink.sbt.mavgen

import java.io.File

import org.fusesource.scalate.{Template, TemplateEngine}
import sbt.Logger

import scala.xml.Elem

object BundleGenerator extends Generator {
  def generate(xmls: Map[String, Elem], templates: Map[String, Template], outputPath: File)
              (implicit engine: TemplateEngine, log: Logger): Map[File, String] = {
    val bundles = xmls.map(s => s._1)

    templates.map { case (name, template) =>
      val (dir, pkg, fname) = parsePath(outputPath, "", name)
      log.debug(s"Rendering $fname")
      val text = engine.layout("layout", template, Map("pkg" -> pkg, "bundles" -> bundles))
      new File(dir, scalaSource(fname)) -> text
    }
  }
}
