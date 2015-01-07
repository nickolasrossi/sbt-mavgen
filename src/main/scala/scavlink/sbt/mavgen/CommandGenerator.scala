package scavlink.sbt.mavgen

import org.fusesource.scalate.{Template, TemplateEngine}
import sbt.{File, Logger}

import scala.annotation.tailrec
import scala.util.parsing.combinator.RegexParsers
import scala.xml.{Elem, Node}


case class Command(name: String, id: String, bundle: String, description: String, params: Seq[CommandParam], hasLocation: Boolean = false)

case class CommandParam(name: String, index: Int, description: String, ftype: String = "Float", default: String = "0") {
  val plainName: String = if (name.length > 0 && name.head == '`' && name.last == '`') {
    name.substring(1, name.length - 1)
  } else {
    name
  }

  def assignment(prefix: String): String = if (name == "location" && ftype == "Coordinates") {
    s"$prefix$index = location.x.toFloat, $prefix${ index + 1 } = location.y.toFloat, $prefix${ index + 2 } = location.z.toFloat"
  } else {
    s"$prefix$index = $plainName"
  }
}

object CommandGenerator extends Generator {
  def generate(xmls: Map[String, Elem], templates: Map[String, Template], outputPath: File)
              (implicit engine: TemplateEngine, log: Logger): Map[File, String] = {
    val commands = xmls.map { case (k, v) => k -> unmarshal(k, v) }

    for {
      (name, template) <- templates
      (bundle, cmds) <- commands
    } yield {
      val (dir, pkg, fname) = parsePath(outputPath, bundle, name)
      log.debug(s"Rendering $fname")
      val text = engine.layout("layout", template, Map("cmds" -> cmds, "bundle" -> bundle, "pkg" -> pkg))
      new File(dir, scalaSource(fname)) -> text
    }
  }

  def unmarshal(bundle: String, xml: Elem): Seq[Command] = {
    val cmdNodes = (xml \ "enums" \ "enum").filter(n => (n \ "@name").text == "MAV_CMD") \ "entry"
    for (c <- cmdNodes if !(c \ "description").text.trim.startsWith("NOP")) yield unmarshalOne(bundle, c)
  }

  def unmarshalOne(bundle: String, node: Node): Command = {
    val params = for {
      p <- node \ "param" if !p.text.trim.equalsIgnoreCase("Empty") && !p.text.trim.equalsIgnoreCase("Reserved")
    } yield {
      val index = (p \ "@index").text.toInt
      val desc = p.text.trim
      val field = fieldName(CommandDescriptionParser.parse(desc, "param" + index))
      CommandParam(field, index, desc)
    }

    val name = properMethodName((node \ "@name").text.trim, bundle)
    val nparams = collapseLocation(params)

    Command(
      name,
      (node \ "@value").text.trim,
      bundle,
      (node \ "description").text.trim,
      nparams,
      nparams.length < params.length
    )
  }

  /**
   * Turn a MAV_CMD enum identifier into a method name for the class.
   */
  def properMethodName(name: String, bundle: String): String = {
    val s = if (name.startsWith(prefix)) name.substring(prefix.length) else name
    className(s)
  }

  private val prefix = "MAV_CMD_"

  def collapseLocation(params: Seq[CommandParam]): Seq[CommandParam] = {
    findLatLonAlt(params) match {
      case -1 => params
      case n =>
        val description = params.slice(n, n + 3).map(_.description).mkString(" / ")
        val param = CommandParam("location", params(n).index, description, "Coordinates", "Geo()")
        (params.take(n) :+ param) ++ params.drop(n + 3)
    }
  }

  @tailrec
  def findLatLonAlt(params: Seq[CommandParam], i: Int = 0): Int = {
    def isLocation(index: Int, name1: String, name2: String, name3: String): Boolean = {
      (name1.startsWith("lat") && name2.startsWith("lon") && name3.startsWith("alt")) ||
        (index == 5 && name1.startsWith("x") && name2.startsWith("y") && name3.startsWith("z"))
    }

    if (params.length < 3) {
      -1
    } else {
      if (isLocation(params(0).index, params(0).name, params(1).name, params(2).name)) {
        i
      } else {
        findLatLonAlt(params.tail, i + 1)
      }
    }
  }
}


/**
 * Since command descriptions in MAVLink xml don't specify field names,
 * this object attempts to deduce a parameter name from the description text.
 * - split description into words on whitespace, underscore, and dash
 * - treat "/" as the word "or"
 * - terminate on stop word or non-alphanumeric character (except "/")
 * - combine into camel-case field name
 */
object CommandDescriptionParser extends RegexParsers {
  def anyWord: Parser[String] = """[a-zA-Z0-9']+""".r ^^ trunc
  def word: Parser[String] = anyWord ^? new NotStopWord("in", "if", "to", "at", "as", "e")
  def slash: Parser[String] = "/" ^^^ { "or" }
  def sep: Parser[String] = """[_\-\s]*""".r

  def words: Parser[List[String]] = repsep(word | slash, sep)
  def allWords: Parser[List[String]] = repsep(anyWord | slash, sep)

  // handle whitespace explicitly above in "sep"
  override val whiteSpace = "".r

  /**
   * Attempt to parse description into a field name.
   * @param description source string
   * @param fallback value to return if parsing fails
   * @return field name or fallback
   */
  def parse(description: String, fallback: String, keepStopWords: Boolean = false): String = {
    val parser = if (keepStopWords) allWords else words
    parse(parser, description) match {
      case Success(r, _) =>
        val name = r.map(_.toLowerCase).reduceLeft(camel)
        if (name.head.isDigit) fallback else name

      case _ => fallback
    }
  }

  def camel(s1: String, s2: String) = s1 + s2.capitalize

  def trunc(s: String): String = {
    val n = s.indexWhere(!_.isLetterOrDigit)
    if (n >= 0) s.substring(0, n) else s
  }

  class NotStopWord(stopWords: String*) extends PartialFunction[String, String] {
    def isDefinedAt(x: String) = !stopWords.contains(x)
    def apply(x: String) = x
  }
}
