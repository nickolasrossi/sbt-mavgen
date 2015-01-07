package scavlink.sbt.mavgen

case class MagicField(name: String, ftype: String, elem: Int)

/**
 * Compute magic number from a message definition.
 */
object Magic {
  val fieldLengths = Map(
    "float" -> 4, 
    "double" -> 8, 
    "char" -> 1, 
    "int8_t" -> 1, 
    "uint8_t" -> 1,
    "uint8_t_mavlink_version" -> 1, 
    "int16_t" -> 2, 
    "uint16_t" -> 2, 
    "int32_t" -> 4,
    "uint32_t" -> 4,
    "int64_t" -> 8,
    "uint64_t" -> 8
  )

  def apply(name: String, fields: Seq[MagicField]): Int = {
    val bytes = messageDef(name, fields)
    val crc = bytes.foldLeft(0xffff)(x25acc)
    (crc & 0xff) ^ (crc >> 8)
  }

  def messageDef(name: String, fields: Seq[MagicField]): Seq[Byte] =
    fields
      .sortWith((f1, f2) => fieldOrder(f1.ftype, f2.ftype))
      .foldLeft((name + " ").getBytes)(addField)

  def addField(bs: Array[Byte], field: MagicField) =
    bs ++ 
      (field.ftype + " " + field.name + " ").getBytes ++ 
      (if (field.elem > 1) Seq(field.elem.toByte) else Nil)
  
  def fieldOrder(type1: String, type2: String) = fieldLengths(type1) > fieldLengths(type2)

  def x25acc(acc: Int, b: Byte): Int = {
    val v1 = unsigned(b) ^ (acc & 0xff)
    val v2 = v1 ^ ((v1 << 4) & 0xff)
    ((acc >> 8) & 0xff) ^ (v2 << 8) ^ (v2 << 3) ^ ((v2 >> 4) & 0xf)
  }

  def unsigned(b: Byte): Int = b & 0xff
}
