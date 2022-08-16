val scala3Version = "3.1.3"
val appVersion = "0.1.0-SNAPSHOT"

lazy val protobuf = project
  .in(file("protobuf"))
  .settings(
    version := appVersion,
    scalaVersion := scala3Version
  )
  .enablePlugins(Fs2Grpc)

lazy val atcui = project
  .in(file("atcui"))
  .dependsOn(protobuf)
  .settings(
    name := "atc-ui",
    fork := true,
    version := appVersion,
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.scalafx" %% "scalafx" % "18.0.2-R29",
      "io.grpc" % "grpc-netty-shaded" % scalapb.compiler.Version.grpcJavaVersion
    ),
    libraryDependencies ++= {
      // Determine OS version of JavaFX binaries
      lazy val osName = System.getProperty("os.name") match {
        case n if n.startsWith("Linux")   => "linux"
        case n if n.startsWith("Mac")     => "mac"
        case n if n.startsWith("Windows") => "win"
        case _ => throw new Exception("Unknown platform!")
      }
      Seq("base", "controls", "fxml", "graphics", "media", "swing", "web")
        .map(m => "org.openjfx" % s"javafx-$m" % "18" classifier osName)
    }
  )

lazy val atcprocessor = project
  .in(file("atcprocessor"))
  .dependsOn(protobuf)
  .settings(
    name := "atc-control",
    fork := true,
    version := appVersion,
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % "3.3.14",
      "org.typelevel" %% "cats-core" % "2.8.0",
      "co.fs2" %% "fs2-core" % "3.2.12",
      "io.grpc" % "grpc-netty-shaded" % scalapb.compiler.Version.grpcJavaVersion,
      "dev.optics" %% "monocle-core" % "3.1.0",
      "dev.optics" %% "monocle-macro" % "3.1.0"
    )
  )

lazy val root = project
  .in(file("."))
  .aggregate(protobuf, atcui, atcprocessor)
