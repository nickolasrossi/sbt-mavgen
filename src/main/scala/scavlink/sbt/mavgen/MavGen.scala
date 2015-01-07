package scavlink.sbt.mavgen

import java.io.FileWriter

import org.fusesource.scalate.TemplateEngine
import sbt._

import scala.collection.immutable.ListMap
import scala.xml.{Elem, XML}

/**
 * sbt task for generating Scala source files from MAVLink protocol definitions.
 */
object MavGen {
  /**
   * Invoke the plugin.
   * @param mavgenPath source root for MAVLink protocol files and code templates
   * @param outputPath destination path for generated source files
   * @param log sbt logger
   * @return list of created files
   */
  def apply(mavgenPath: File, outputPath: File, log: Logger): Seq[File] = {
    implicit val logger = log

    // drop initial absolute path so that it isn't used in Scalate generated package names
    // (if it contains reserved words, the Scalate template will not compile)
    val currentPath = new File(".").getAbsolutePath.dropRight(1)
    val path = mavgenPath.getPath
    val relativePath = if (path.startsWith(currentPath)) path.drop(currentPath.length) else path

    // scan source root for files and directories
    val files = new File(relativePath).listFiles()

    // load XML files
    val xmlFiles = files.filter(_.ext == "xml")
    val xmls = ListMap(xmlFiles.map(f => f.base -> XML.loadFile(f)).sortWith(bundleLt): _*)

    // load templates from directories
    val dirs = files.filter(_.isDirectory)
    val templateFiles = dirs.view.map { d =>
      d.name -> d.listFiles().filter(_.ext == "ssp")
    }.toMap

    // prepare scalate engine
    implicit val engine = new TemplateEngine
    engine.escapeMarkup = false
    engine.workingDirectory

    val templates = templateFiles.mapValues { files =>
      files.view.map(f => f.base -> engine.load(f)).toMap
    }

    // generate source code for four types of source files
    val outputFiles =
      EnumGenerator.generate(xmls, templates("enum"), outputPath) ++
        MessageGenerator.generate(xmls, templates("message"), outputPath) ++
        CommandGenerator.generate(xmls, templates("command"), outputPath) ++
        BundleGenerator.generate(xmls, templates("bundle"), outputPath)

    // write all files
    outputFiles.foreach { case (file, text) =>
      log.info(file.getAbsolutePath)
      writeFile(file, text)
    }

    log.info(s"Generated ${ outputFiles.size } source files")
    outputFiles.keySet.toSeq
  }

  def writeFile(file: File, text: String): Unit = {
    val dir = file.getParentFile
    dir.mkdirs()
    val out = new FileWriter(file)
    out.write(text)
    out.close()
  }

  def bundleLt(s1: (String, Elem), s2: (String, Elem)): Boolean = {
    if (s1._1 == "common") true
    else if (s2._1 == "common") false
    else s1._1 < s2._1
  }
}
