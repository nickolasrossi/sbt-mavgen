package scavlink.sbt.mavgen

import org.fusesource.scalate._
import sbt.{File, Logger}

import scala.annotation.tailrec
import scala.xml.{Elem, Node}

case class Enum(name: String, description: String, bundle: String, values: Seq[EnumValue], isFlag: Boolean)
case class EnumValue(name: String, description: String, value: String)

/**
 * Generates one file per enumeration definition, first massaging the enum values as necessary.
 * Enumerations from all bundles are placed in the same package.
 * 
 * If a bundle adds values to a "common" enumeration (e.g. MAV_CMD), those values are added to the
 * common enumeration with the bundle name prepended.
 */
object EnumGenerator extends Generator {
  def generate(xmls: Map[String, Elem], templates: Map[String, Template], outputPath: File)
              (implicit engine: TemplateEngine, log: Logger): Map[File, String] = {
    val es = xmls.flatMap { case (k, v) => unmarshal(k, v) }

    val f = mergeEnums _ andThen addUnknown
    val enums = f(es)

    templates.flatMap { case (name, template) =>
      val (dir, pkg, fname) = parsePath(outputPath, "", name)
      enums.map { e =>
        log.debug(s"Rendering ${e.name}")
        val text = engine.layout(template.source.uri, template, Map("enum" -> e, "pkg" -> pkg))
        new File(dir, scalaSource(e.name)) -> text
      }
    }
  }

  /**
   * Unmarshal XML enum definitions into a collection of Enum objects.
   */
  def unmarshal(bundle: String, xml: Elem): Seq[Enum] = {
    val enumNodes = xml \ "enums" \ "enum"
    enumNodes.map(e => unmarshalOne(bundle, e))
  }

  def unmarshalOne(bundle: String, e: Node): Enum = {
    val ename = (e \ "@name").text.trim
    val evs = (e \ "entry").map {
      v => EnumValue((v \ "@name").text.trim, (v \ "description").text.trim, (v \ "@value").text.trim)
    }

    val f = addValues(ename) _ andThen chopPrefix(ename) andThen trimHeadWords andThen fixDigits andThen fillValues
    val values = f(evs)

    Enum(
      className(ename),
      (e \ "description").text.trim,
      bundle,
      values,
      isFlag(values)
    )
  }

  /**
   * Merge enums of the same name from different bundles into a single class.
   */
  def mergeEnums(enums: Iterable[Enum]): Iterable[Enum] = enums.foldLeft(Seq[Enum]())(mergeEnum)

  /**
   * If the new enum already exists in the list, add its values to the existing one with the bundle name prepended.
   */
  def mergeEnum(enums: Seq[Enum], e: Enum): Seq[Enum] = enums.indexWhere(x => x.name == e.name) match {
    case -1 => enums :+ e
    case n =>
      val values = enums(n).values ++ e.values.map(v => v.copy(name = mergedValueName(e.bundle, v.name)))
      val ne = enums(n).copy(values = values.foldLeft(Seq[EnumValue]())(mergeValue))
      (enums.take(n) :+ ne) ++ enums.drop(n+1)
  }

  /**
   * Add UNKNOWN zero enum value where there isn't a zero value defined.
   * This should happen after merging enums from different bundles, in case the merge brought in a defined zero.
   */
  def addUnknown(enums: Iterable[Enum]): Iterable[Enum] = {
    for (e <- enums) yield {
      if (!e.values.exists(_.value == "0"))
        e.copy(values = EnumValue("_UNKNOWN", "", "0") +: e.values)
      else
        e
    }
  }

  /**
   * A hack to add any hardcoded enum values that aren't in the protocol definition.
   * - ardupilot will return a SET_MODE (11) in MAV_COMMAND_ACK even though it's not defined in MAV_CMD enum
   */
  def addValues(name: String)(values: Seq[EnumValue]): Seq[EnumValue] = {
    if (name == "MAV_CMD")
      EnumValue("ACK_SET_MODE", "", "11") +: values
    else
      values
  }

  /**
   * Only add the enum value if its numeric value isn't already in the list.
   */
  def mergeValue(values: Seq[EnumValue], nv: EnumValue) =
    if (values.indexWhere(v => v.value == nv.value) >= 0) values else values :+ nv

  /**
   * Prepend the bundle name when merging enum.
   */
  def mergedValueName(bundle: String, name: String): String =
    if (name(0) == '_') bundle.toUpperCase + name else bundle.toUpperCase + "_" + name

  /**
   * Remove the first part of every name that matches the first part of the prefix.
   */
  def chopPrefix(prefix: String)(values: Seq[EnumValue]): Seq[EnumValue] = {
    for (v <- values) yield {
      val n = matchPrefix(v.name, prefix)
      if (n > 0) v.copy(name = v.name.substring(n)) else v
    }
  }

  /**
   * Compares the start of s with the start of prefix, returning the index up to where they match.
   * After comparison, if the next char in s is an underscore, increment the index one more.
   */
  def matchPrefix(s: String, prefix: String): Int = {
    var i = 0
    while (i < prefix.length && prefix(i) == s(i)) i += 1
    if (i > 0 && s(i) == '_') i += 1
    i
  }
  
  /**
   * Removes any starting words that all enum values have in common, using underscore as a word break.
   */
  @tailrec
  def trimHeadWords(values: Seq[EnumValue]): Seq[EnumValue] = values match {
    case Nil => values
    case ev :: Nil => values
    case _ =>
      // collapse all first words up to '_' into a Set
      val heads = values.map(v => v.name.substring(0, v.name.indexOf('_') + 1)).toSet.toList
      // if it's a Set of 1 that isn't an empty string, then they're all the same, so keep chopping
      heads match {
        case s :: Nil if s != "" => trimHeadWords(values.map(v => EnumValue(v.name.substring(s.length), v.description, v.value)))
        case _ => values
      }
  }

  /**
   * Prepend underscore to any enum values that start with a digit.
   */
  def fixDigits(values: Seq[EnumValue]): Seq[EnumValue] =
    values.map(v => if (v.name.head.isDigit) EnumValue("_" + v.name, v.description, v.value) else v)

  /**
   * Fill in any unspecified numeric values, preferring to fill the previous value plus 1.
   */
  def fillValues(values: Seq[EnumValue]) : Seq[EnumValue] = {
    var numbers = values.map(v => parseNumeric(v.value)).toArray

    values.zipWithIndex.map { case (v, i) =>
      v.value match {
        case "" =>
          val p = 1 + (if (i == 0) 0 else numbers(i-1))
          val n = if (numbers.contains(p)) numbers.max + 1 else p
          numbers(i) = n
          v.copy(value = n.toString)

        case _ => v
      }
    }
  }

  /**
   * Parse numeric value from enum definition.
   * The number might be specified as decimal, hex or binary.
   */
  def parseNumeric(s: String): Int = {
    if (s == "") {
      -1
    } else if (s.toLowerCase.startsWith("0x")) {
      java.lang.Long.parseLong(s.substring(2), 16).toInt
    } else if (s.toLowerCase.startsWith("0b")) {
      java.lang.Long.parseLong(s.substring(2), 2).toInt
    } else {
      java.lang.Long.parseLong(s).toInt
    }
  }

  /**
   * Determines whether all values are single-bit values.
   */
  def isFlag(evs: Seq[EnumValue]): Boolean = {
    val numbers = evs.map(ev => parseNumeric(ev.value))
    numbers.forall(allBits.contains)
  }

  val allBits = (0 until 32).map(1L << _)
}
