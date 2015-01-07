package scavlink.sbt.mavgen

import java.io.File

import org.fusesource.scalate.{Template, TemplateEngine}
import sbt._

import scala.annotation.tailrec
import scala.xml.Elem

trait Generator {
  /**
   * Generate source code for all XML blocks times all Scalate templates.
   * @param xmls map of bundle name to top-level xml node
   * @param templates map of template name to Scalate template
   * @param outputPath base directory for output file names
   * @param engine Scalate template engine
   * @param log sbt logger
   * @return map of output file name to source text
   */
  def generate(xmls: Map[String, Elem], templates: Map[String, Template], outputPath: File)
              (implicit engine: TemplateEngine, log: Logger): Map[File, String]

  /**
   * Convert underscore-delimited string to a camel-cased class name.
   */
  def className(name: String): String =
    name.toLowerCase.split('_').map(_.capitalize).mkString

  /**
   * Convert underscore-delimited string to a field name.
   * Surrounds the result in back-ticks if it's a Scala reserved word.
   */
  def fieldName(name: String): String = {
    val parts = name.split('_').map(s => if (s.head.isUpper) s.toLowerCase else s)
    val fname = parts.head + parts.tail.map(_.capitalize).mkString
    if (reservedWords.contains(fname)) "`" + fname + "`" else fname
  }

  /**
   * Parse a template source file name into a file path and package.
   * @param base base output path
   * @param bundle bundle name
   * @param template template file name
   * @return (directories, package, filename)
   */
  def parsePath(base: File, bundle: String, template: String): (File, String, String) = {
    val t = template.replace("__", bundle.capitalize).replace("_", bundle)
    val parts = t.split("\\.")
    val init = parts.init.toList
    (buildPath(base, init), init.mkString("."), parts.last)
  }

  /**
   * Append ".scala" to the file name.
   */
  def scalaSource(file: String) = file + ".scala"

  /**
   * Build a directory path from parts.
   */
  @tailrec
  final def buildPath(base: File, names: List[String]): File = names match {
    case Nil => base
    case f :: fs => buildPath(new File(base, f), fs)
  }

  val reservedWords =
    Set(
      "abstract",
      "case",
      "catch",
      "class",
      "def",
      "do",
      "else",
      "extends",
      "false",
      "final",
      "finally",
      "for",
      "forSome",
      "if",
      "implicit",
      "import",
      "lazy",
      "match",
      "new",
      "null",
      "object",
      "override",
      "package",
      "private",
      "protected",
      "return",
      "sealed",
      "super",
      "this",
      "throw",
      "trait",
      "try",
      "true",
      "type",
      "val",
      "var",
      "while",
      "with",
      "yield")
}
