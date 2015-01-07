package scavlink.sbt.mavgen

import sbt.Keys._
import sbt._

/**
 * sbt plugin for mavgen task.
 *
 * Properties:
 * mavgen-xml-directory: where to find message definition files. defaults to src/main/resources/mavgen
 * mavgen-output-directory: where to generate source files. defaults to src/main/scala
 */
object MavGenPlugin extends AutoPlugin {

  object autoImport {
    val mavgenTask = TaskKey[Seq[File]]("mavgen", "Generate MAVLink message classes")

    val mavgenDirectory = SettingKey[File]("mavgen-xml-directory", "Message definition files")
    val outputDirectory = SettingKey[File]("mavgen-output-directory", "Output path for source files")

    lazy val baseMavGenSettings: Seq[Def.Setting[_]] = Seq(
      mavgenTask := { MavGen(mavgenDirectory.value, outputDirectory.value, streams.value.log) },
      mavgenDirectory := resourceDirectory.value / "mavgen",
      outputDirectory := scalaSource.value
    )
  }

  import scavlink.sbt.mavgen.MavGenPlugin.autoImport._

  override val projectSettings =
    inConfig(Compile)(baseMavGenSettings) ++
    inConfig(Test)(baseMavGenSettings)
}
