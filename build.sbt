lazy val baseName       = "Tagfalter"
lazy val baseNameL      = baseName.toLowerCase
lazy val projectVersion = "0.1.0"

lazy val gitHost        = "codeberg.org"
lazy val gitUser        = "sciss"
lazy val gitRepo        = baseName

lazy val buildInfoSettings = Seq(
  // ---- build info ----
  buildInfoKeys := Seq(name, organization, version, scalaVersion, description,
    BuildInfoKey.map(homepage) { case (k, opt)           => k -> opt.get },
    BuildInfoKey.map(licenses) { case (_, Seq((lic, _))) => "license" -> lic }
  ),
  buildInfoOptions += BuildInfoOption.BuildTime
)

lazy val commonSettings = Seq(
  version      := projectVersion,
  homepage     := Some(url(s"https://$gitHost/$gitUser/$gitRepo")),
  scalaVersion := "2.13.8",
  licenses     := Seq("AGPL v3+" -> url("http://www.gnu.org/licenses/agpl-3.0.txt")),
  run / fork   := true,
) ++ assemblySettings

lazy val root = project.in(file("."))
  .enablePlugins(BuildInfoPlugin)
  .settings(commonSettings)
  .settings(buildInfoSettings)
//  .settings(assemblySettings)
  .settings(
    name := baseName,
    description  := "An art piece",
    libraryDependencies ++= Seq(
      "de.sciss"      %% "audiofile"                  % deps.main.audioFile,    // record data as sound file
      "de.sciss"      %% "fileutil"                   % deps.main.fileUtil,     // utility functions
      "de.sciss"      %% "model"                      % deps.main.model,        // events
      "de.sciss"      %% "numbers"                    % deps.main.numbers,      // numeric utilities
      "de.sciss"      %% "scalaosc"                   % deps.main.osc,          // open sound control
      "de.sciss"      %% "scalacolliderugens-plugins" % deps.main.ugen,         // third party ugens
      "de.sciss"      %% "soundprocesses-core"        % deps.main.soundProcesses,
      "net.harawata"  %  "appdirs"                    % deps.main.appDirs,      // finding standard directories
      "org.rogach"    %% "scallop"                    % deps.main.scallop,      // command line option parsing
    ),
    scalacOptions += "-deprecation",
    assembly / assemblyJarName := s"$baseName.jar",
    resolvers += Resolver.sonatypeRepo("snapshots"),  // needed for hid4java
    buildInfoPackage := "de.sciss.tagfalter",
  )

lazy val deps = new {
  val main = new {
    val appDirs         = "1.2.1"
    val audioFile       = "2.4.0"
    val fileUtil        = "1.1.5"
    val model           = "0.3.5"
    val numbers         = "0.2.1"
    val osc             = "1.3.1"
    val scallop         = "4.1.0"
    val soundProcesses  = "4.14.4"
    val ugen            = "1.21.4"
  }
}

lazy val assemblySettings = Seq(
  // ---- assembly ----
  assembly / test            := {},
  assembly / target          := baseDirectory.value,
  ThisBuild / assemblyMergeStrategy := {
    case "logback.xml" => MergeStrategy.last
    case PathList("org", "xmlpull", _ @ _*)              => MergeStrategy.first
    case PathList("org", "w3c", "dom", "events", _ @ _*) => MergeStrategy.first // Apache Batik
    case p @ PathList(ps @ _*) if ps.last endsWith "module-info.class" =>
      println(s"DISCARD: $p")
      MergeStrategy.discard // Jackson, Pi4J
    case x =>
      val old = (ThisBuild / assemblyMergeStrategy).value
      old(x)
  },
//  assembly / fullClasspath := (Test / fullClasspath).value // https://github.com/sbt/sbt-assembly/issues/27
)
