package scavlink.sbt.mavgen

import org.fusesource.scalate.{Template, TemplateEngine}
import sbt.{File, Logger}

import scala.annotation.tailrec
import scala.xml.{Elem, Node}

/**
 * Generates files based on the list of defined protocol messages. The file list includes:
 * - single file of all Message case classes
 * - marshallers and unmarshallers
 * - CRC magic numbers
 */
object MessageGenerator extends Generator {

  val scalaTypeMap = Map("char" -> "Byte",
    "float" -> "Float", "double" -> "Double",
    "int8_t" -> "Byte", "uint8_t" -> "Byte",
    "int16_t" -> "Short", "uint16_t" -> "Short",
    "int32_t" -> "Int", "uint32_t" -> "Int",
    "int64_t" -> "Long", "uint64_t" -> "Long")

  val requireTypes = List(/*"int8_t", "int16_t",*/ "uint8_t", "uint16_t", "uint32_t")

  def generate(xmls: Map[String, Elem], templates: Map[String, Template], outputPath: File)
              (implicit engine: TemplateEngine, log: Logger): Map[File, String] = {
    val messages = xmls.map { case (k, v) => k -> unmarshal(k, v) }

    for {
      (name, template) <- templates
      (bundle, msgs) <- messages
    } yield {
      val (dir, pkg, fname) = parsePath(outputPath, bundle, name)
      log.debug(s"Rendering $fname")
      val text = engine.layout("layout", template, Map("messages" -> msgs, "pkg" -> pkg, "bundle" -> bundle))
      new File(dir, scalaSource(fname)) -> text
    }
  }

  def unmarshal(bundle: String, xml: Elem): Seq[Message] = {
    val messageNodes = xml \ "messages" \ "message"
    messageNodes.map { node => unmarshalMessage(bundle, node) }
  }
  
  def unmarshalMessage(bundle: String, node: Node): Message = {
    val msgName = (node \ "@name").text.trim

    // must calculate magic number from raw field list
    val magicFields = (node \ "field").map { f =>
      val ftype = parseType((f \ "@type").text.trim, (f \ "@enum").text.trim)
      MagicField((f \ "@name").text.trim, ftype.dataType, ftype.elem)
    }
    val magic = Magic(msgName, magicFields)

    // build our massaged field list
    val rawFields = (node \ "field").zipWithIndex.map { case (f, n) => unmarshalField(f, n) }
    val fields = collapse(rawFields.map(conformTarget))
    val marshalOrder = fields.sortWith((f1, f2) => Magic.fieldOrder(f1.ftype.dataType, f2.ftype.dataType))

    Message(
      (node \ "@id").text.trim.toInt,
      msgName,
      className(msgName),
      (node \ "description").text.trim,
      bundle,
      fields,
      marshalOrder,
      magic
    )
  }
  
  def unmarshalField(node: Node, index: Int): Field = {
    val fdesc = node.text.trim
    val ftype = parseType((node \ "@type").text.trim, (node \ "@enum").text.trim)
    val scalaType = scalaTypeOf(ftype)
    val ord = caseName(index)
    Field(fieldName((node \ "@name").text.trim), ord, ftype, scalaType, fdesc)    
  }
  
  /**
   * Convert field ordinal into a valid Scala token for a case statement match in the marshaller.
   */
  def caseName(value: Int): String = value match {
    case n if n < 26 => ('a' + n).toChar.toString
    case n => caseName(n / 26 - 1) + caseName(n % 26)
  }

  /**
   * Extract type, array and enum into a canonical definition of the field type.
   */
  def parseType(ftype: String, enum: String): FieldType = {
    def trimType(s: String) = if (s == "uint8_t_mavlink_version") "uint8_t" else s

    // hack: MavMode is really a flag, so we can't declare fields with that type
    val ename = if (enum == "MAV_MODE") "" else enum

    val p = ftype.indexOf('[')
    if (p > 0) {
      val q = ftype.indexOf(']', p)
      val elem = ftype.substring(p + 1, q).toInt
      FieldType(trimType(ftype.substring(0, p)), ename, elem)
    }
    else {
      FieldType(trimType(ftype), ename, 1)
    }
  }

  /**
   * Return the (scala type, default value) for a parsed field type.
   */
  def scalaTypeOf(ftype: FieldType): ScalaType = ftype match {
    case FieldType(_, e, elem) if e != "" =>
      val ename = className(e)
      arrayType(ename + ".Value", elem, ename + "(0)")

    case FieldType("char", _, 1) => ScalaType("Byte", "0")
    case FieldType("char", _, _) => ScalaType("String", "\"\"")
    case FieldType(t, _, elem) => arrayType(scalaTypeMap(t), elem, "0")
  }

  /**
   * Build an appropriate array type if the number of elements > 1.
   * Use a Tuple for elements <= 4, Vector otherwise.
   * Tuple is a more natural represntation for fields that are group structures, e.g. xyz position,
   * which all the arrays <= 4 seem to be.
   */
  def arrayType(baseType: String, elem: Int, default: String): ScalaType = elem match {
    case 1 => ScalaType(baseType, default)
    case n if n <= 4 => ScalaType("(" + List.fill(n)(baseType).mkString(",") + ")", "(" + List.fill(n)(default).mkString(",") + ")")
    case n => ScalaType("Vector[" + baseType + "]", "Vector.fill(" + n + ")(" + default + ")")
  }

  /**
   * Scala case classes can have a maximum of 22 fields.
   * If there are more than 22 fields, some must be grouped together to reduce the total number.
   * This method collapses field groups with names ending in X/Y/Z or a sequence of numbers.
   */
  def collapse(fields: Seq[Field]): Seq[Field] = {
    if (fields.length <= 22) {
      fields
    } else {
      val f = collapseXYZ _ andThen collapseFields(mergeSequence)
      f(fields)
    }
  }

  def collapseFields(f: (Seq[Field], Field) => Seq[Field])(fields: Seq[Field]): Seq[Field] =
    fields.foldLeft(Seq[Field]())(f)

  /**
   * Collapse all groups of x/y/z fields into a single field of Tuple3 type.
   */
  @tailrec
  def collapseXYZ(fields: Seq[Field]): Seq[Field] = {
    def inc(c: Char, n: Int) = (c + n).toChar

    def renameXYZ(s: String, ix: Int) =
      if (ix < 0) s else s.take(ix) + s(ix) + inc(s(ix), 1) + inc(s(ix), 2) + s.drop(ix + 1)

    def descriptionFindX(s: String): Int = {
      val sl = s.toLowerCase
      val i = sl.indexOf(" x")
      if (i >= 0) i + 1 else if (sl.startsWith("x ")) 0 else -1
    }

    val n = indexOfXYZ(fields)
    if (n < 0) {
      fields
    } else {
      val f = fields(n)
      val fname = renameXYZ(f.name, f.name.toLowerCase.indexOf('x'))
      val fdesc = renameXYZ(f.description, descriptionFindX(f.description))
      val ftype = f.ftype.copy(elem = 3)
      val scalaType = scalaTypeOf(ftype)
      val nf = Field(fname, f.ord, ftype, scalaType, fdesc)
      val nseq = fields.take(n) ++ (nf +: fields.drop(n + 3))
      collapseXYZ(nseq)
    }
  }

  /**
   * Returns the first index of a group of three X/Y/Z fields.
   */
  @tailrec
  def indexOfXYZ(fields: Seq[Field], n: Int = 0): Int = {
    if (fields.length < 3) {
      -1
    } else {
      val f0 = fields(0)
      val f1 = fields(1)
      val f2 = fields(2)
      val ix = f0.name.toLowerCase.indexOf('x')
      if (ix >= 0 && equalsExcept(f0.name, f1.name, ix, 'y') && equalsExcept(f0.name, f2.name, ix, 'z')
        && f0.ftype == f1.ftype && f0.ftype == f2.ftype) {
        n
      } else {
        indexOfXYZ(fields.tail, n + 1)
      }
    }
  }

  /**
   * Are two strings equal except at position n, where string 2 contains character c.
   */
  def equalsExcept(s1: String, s2: String, n: Int, c: Char): Boolean = {
    if (s1.length != s2.length) {
      false
    } else {
      var i = 0
      while (i < s1.length && ((i == n && s2(i).toLower == c.toLower) || s1(i) == s2(i))) i += 1
      i == s1.length
    }
  }

  /**
   * Assign special types to any target system and component field.
   * Also conforms field names to "targetSystem" and "targetComponent".
   */
  def conformTarget(field: Field): Field = field match {
    case Field("target" | "targetSystem" | "targetSystemId", _, _, ScalaType("Byte", _), _) =>
      field.copy(name = "targetSystem", stype = ScalaType("SystemId", "0"))

    case Field("targetComponent" | "targetComponentId", _, _, ScalaType("Byte", _), _) =>
      field.copy(name = "targetComponent", stype = ScalaType("ComponentId", "0"))

    case _ => field
  }

  /**
   * Used to fold a sequence of fields whose names end in the numbers 0 or 1 through N.
   * If next field matches the last field in the list, fold by incrementing the last's array count by 1.
   * Otherwise, append the next field to the list.
   */
  def mergeSequence(fields: Seq[Field], nf: Field): Seq[Field] = {
    val (nfi, nfval) = parseNumericSuffix(nf.name)
    if (nfi < 0) {
      fields :+ nf
    } else {
      val lf = fields.last
      val (lfi, lfval) = parseNumericSuffix(lf.name)
      val lfname = if (lfi < 0) lf.name else lf.name.substring(0, lfi)

      // fold condition: [name0 (elem=1), name1] or [name1 (elem=1), name2] or [name (elem=n), nameN] or [name (elem=n), nameN+1]
      if (lfname == nf.name.substring(0, nfi) && lf.ftype.dataType == nf.ftype.dataType && (
        (lf.ftype.elem == 1 && nfval == lfval + 1)
          || (lf.ftype.elem > 1 && (nfval == lf.ftype.elem || nfval == lf.ftype.elem + 1))
        )) {
        val lftype = lf.ftype.copy(elem = lf.ftype.elem + 1)
        val stype = scalaTypeOf(lftype)
        fields.dropRight(1) :+ Field(lfname, lf.ord, lftype, stype, rewriteDescription(lf.description, lftype.elem))
      }
      else {
        fields :+ nf
      }
    }
  }

  /**
   * Returns the parsed number and its index position from the end of the string.
   */
  def parseNumericSuffix(s: String): (Int, Int) = {
    val i = lastIndexOfNumber(s)
    if (i >= 0) (i, s.substring(i).toInt) else (-1, -1)
  }

  /**
   * Rewrites sequence field description depending on original:
   * {text}0 => {text}0-(elem-1)
   * {text}1 => {text}1-elem
   * {text}0-n => {text}0-(elem-1)
   * {text}1-n => {text}1-elem
   */
  def rewriteDescription(desc: String, elem: Int): String = {
    val range = parseLastRange(desc)
    if (range.atIndex < 0) desc else desc.substring(0, range.atIndex) + range.from + "-" + (elem - 1 + range.from)
  }


  case class SequenceRange(from: Int, to: Int, atIndex: Int)

  def parseLastRange(s: String): SequenceRange = {
    val k = lastIndexOfNumber(s)
    if (k < 0) {
      SequenceRange(-1, -1, -1)
    } else {
      val v2 = s.substring(k).toInt
      var i = k
      while (i > 0 && s(i - 1) == '-') i -= 1

      val j = lastIndexOfNumber(s.substring(0, i))
      val v1 = if (j < 0) -1 else s.substring(j, i).toInt

      if (v1 >= 0) SequenceRange(v1, v2, j) else SequenceRange(v2, -1, i)
    }
  }

  /**
   * If the string ends with a number, returns the starting position of that number.
   */
  def lastIndexOfNumber(s: String): Int = {
    var i = s.length
    while (i > 0 && s(i - 1).isDigit) i -= 1
    if (i < s.length) i else -1
  }
}


case class Message(id: Int, name: String, className: String, description: String, bundle: String, fields: Seq[Field], marshalOrder: Seq[Field], magic: Int)
case class FieldType(dataType: String, enum: String, elem: Int)
case class ScalaType(dataType: String, default: String)

case class Field(name: String, ord: String, ftype: FieldType, stype: ScalaType, description: String) {
  val plainName = if (name.length > 0 && name.head == '`' && name.last == '`') name.substring(1, name.length - 1) else name

  val marshalField = ftype.elem match {
    case 1 => ftype.dataType
    case n if n <= 4 => ftype.dataType + "_" + n
    case n => ftype.dataType + "_(" + n + ")"
  }

  val marshalMethodCall = {
    val arg = if (ftype.enum != "") ord + ".id" else ord
    "_" + marshalField + "(" + arg + ")"
  }

  val unmarshalMethodCall = ftype.enum match {
    case "" => marshalField
    case _ => stype.dataType.takeWhile(_ != '.') + "(" + marshalField + ")"
  }
}