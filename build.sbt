val scala3Version = "3.2.0-RC4"
val appVersion = "0.1.0-SNAPSHOT"

lazy val commonSettings = Seq(
  version := appVersion,
  scalaVersion := scala3Version,
  libraryDependencies ++= Seq(
    "io.grpc" % "grpc-netty-shaded" % scalapb.compiler.Version.grpcJavaVersion,
    "org.typelevel" %% "cats-effect" % "3.3.14",
    "org.typelevel" %% "cats-core" % "2.8.0",
    "org.typelevel" %% "cats-mtl" % "1.3.0",
    "org.typelevel" %% "log4cats-core" % "2.3.2",
    "org.typelevel" %% "log4cats-slf4j" % "2.3.2",
    "org.typelevel" %% "mouse" % "1.1.0"
  ),
  scalacOptions ++= Seq("-feature")
)

lazy val protobuf = project
  .in(file("protobuf"))
  .settings(
    commonSettings,
    libraryDependencies ++= Seq(
      "com.thesamet.scalapb" %% "scalapb-runtime" % scalapb.compiler.Version.scalapbVersion % "protobuf"
    )
  )
  .enablePlugins(Fs2Grpc)

lazy val atcui = project
  .in(file("atcui"))
  .dependsOn(protobuf)
  .settings(
    commonSettings,
    name := "atc-ui",
    fork := true,
    wartremoverErrors ++= Warts.unsafe,
    libraryDependencies ++= Seq(
      "org.scalafx" %% "scalafx" % "18.0.2-R29"
    ),
    libraryDependencies ++= {
      // Determine OS version of JavaFX binaries
      lazy val osName = System.getProperty("os.name") match {
        case n if n.startsWith("Linux")   => "linux"
        case n if n.startsWith("Mac")     => "mac"
        case n if n.startsWith("Windows") => "win"
        case _                            => throw new Exception("Unknown platform!")
      }
      Seq("base", "controls", "fxml", "graphics", "media", "swing", "web")
        .map(m => "org.openjfx" % s"javafx-$m" % "18" classifier osName)
    }
  )

lazy val atcprocessor = project
  .in(file("atcprocessor"))
  .dependsOn(protobuf)
  .settings(
    commonSettings,
    name := "atc-control",
    fork := true,
    wartremoverErrors ++= Warts.unsafe,
    wartremoverExcluded += sourceManaged.value / "core" / "shared" / "src",
    libraryDependencies ++= Seq(
      "co.fs2" %% "fs2-core" % "3.2.12",
      "dev.optics" %% "monocle-core" % "3.1.0",
      "dev.optics" %% "monocle-macro" % "3.1.0"
    )
  )

lazy val root = project
  .in(file("."))
  .settings(commonSettings)
  .aggregate(protobuf, atcui, atcprocessor)
