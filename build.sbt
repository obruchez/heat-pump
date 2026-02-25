lazy val root = (project in file("."))
  .settings(
    name := "heat-pump",
    version := "0.1.0",
    scalaVersion := "2.13.16",
    libraryDependencies ++= Seq(
      // JSON
      "io.circe" %% "circe-core"    % "0.14.10",
      "io.circe" %% "circe-generic" % "0.14.10",
      "io.circe" %% "circe-parser"  % "0.14.10",
      // Config
      "com.typesafe" % "config" % "1.4.3",
      // Google Sheets API
      "com.google.api-client"    % "google-api-client-gson"          % "2.7.2",
      "com.google.apis"          % "google-api-services-sheets"      % "v4-rev20250106-2.0.0",
      "com.google.auth"          % "google-auth-library-oauth2-http" % "1.31.0",
      // Test
      "org.scalatest" %% "scalatest" % "3.2.19" % Test
    ),
    assembly / assemblyMergeStrategy := {
      case PathList("META-INF", "MANIFEST.MF") => MergeStrategy.discard
      case PathList("META-INF", "versions", "9", "module-info.class") => MergeStrategy.discard
      case PathList("META-INF", xs @ _*) if xs.lastOption.exists(_.endsWith(".SF"))  => MergeStrategy.discard
      case PathList("META-INF", xs @ _*) if xs.lastOption.exists(_.endsWith(".DSA")) => MergeStrategy.discard
      case PathList("META-INF", xs @ _*) if xs.lastOption.exists(_.endsWith(".RSA")) => MergeStrategy.discard
      case PathList("META-INF", "services", _*) => MergeStrategy.concat
      case PathList("META-INF", "native-image", _*) => MergeStrategy.first
      case PathList("module-info.class") => MergeStrategy.discard
      case x if x.endsWith(".properties") => MergeStrategy.first
      case x if x.endsWith(".proto") => MergeStrategy.first
      case x =>
        val oldStrategy = (assembly / assemblyMergeStrategy).value
        oldStrategy(x)
    },
    assembly / mainClass := Some("heatpump.Main")
  )
