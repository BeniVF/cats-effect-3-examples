import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

val catsV = "2.3.0"
val catsEffectV = "3.0-8096649"
val fs2V = "3.0-5795280"

val kindProjectorV = "0.11.2"
val betterMonadicForV = "0.3.1"

// Projects
lazy val `cats-effect-3-exercises` = project
  .in(file("."))
  .disablePlugins(MimaPlugin)
  .enablePlugins(NoPublishPlugin)
  .aggregate(core)

lazy val core = project
  .in(file("core"))
  .settings(commonSettings)
  .settings(
    name := "cats-effect-3-exercises"
  )

lazy val site = project
  .in(file("site"))
  .disablePlugins(MimaPlugin)
  .enablePlugins(MicrositesPlugin)
  .enablePlugins(MdocPlugin)
  .enablePlugins(NoPublishPlugin)
  .settings(commonSettings)
  .dependsOn(core)
  .settings {
    import microsites._
    Seq(
      micrositeName := "cats-effect-3-exercises",
      micrositeDescription := "A basic project",
      micrositeAuthor := "Beni",
      micrositeGithubOwner := "Beni",
      micrositeGithubRepo := "cats-effect-3-exercises",
      micrositeBaseUrl := "/cats-effect-3-exercises",
      micrositeDocumentationUrl := "https://www.javadoc.io/doc/io.benivf/cats-effect-3-exercises_2.13",
      micrositeGitterChannelUrl := "Beni/libraries", // Feel Free to Set To Something Else
      micrositeFooterText := None,
      micrositeHighlightTheme := "atom-one-light",
      micrositePalette := Map(
        "brand-primary" -> "#3e5b95",
        "brand-secondary" -> "#294066",
        "brand-tertiary" -> "#2d5799",
        "gray-dark" -> "#49494B",
        "gray" -> "#7B7B7E",
        "gray-light" -> "#E5E5E6",
        "gray-lighter" -> "#F4F3F4",
        "white-color" -> "#FFFFFF"
      ),
      micrositeCompilingDocsTool := WithMdoc,
      scalacOptions in Tut --= Seq(
        "-Xfatal-warnings",
        "-Ywarn-unused-import",
        "-Ywarn-numeric-widen",
        "-Ywarn-dead-code",
        "-Ywarn-unused:imports",
        "-Xlint:-missing-interpolator,_"
      ),
      micrositePushSiteWith := GitHub4s,
      micrositeGithubToken := sys.env.get("GITHUB_TOKEN"),
      micrositeExtraMdFiles := Map(
        file("CODE_OF_CONDUCT.md") -> ExtraMdFileConfig(
          "code-of-conduct.md",
          "page",
          Map(
            "title" -> "code of conduct",
            "section" -> "code of conduct",
            "position" -> "100"
          )
        ),
        file("LICENSE") -> ExtraMdFileConfig(
          "license.md",
          "page",
          Map("title" -> "license", "section" -> "license", "position" -> "101")
        )
      )
    )
  }

// General Settings
lazy val commonSettings = Seq(
  scalaVersion := "2.13.4",
  crossScalaVersions := Seq(scalaVersion.value),
  addCompilerPlugin(
    "org.typelevel" %% "kind-projector" % kindProjectorV cross CrossVersion.full
  ),
  addCompilerPlugin("com.olegpy" %% "better-monadic-for" % betterMonadicForV),
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-core" % catsV,
    "org.typelevel" %% "cats-effect" % catsEffectV,
    "co.fs2" %% "fs2-core" % fs2V,
    "co.fs2" %% "fs2-io" % fs2V,
    "org.scalameta" %% "munit" % "0.7.19" % Test
  )
)

// General Settings
inThisBuild(
  List(
    organization := "io.benivf",
    developers := List(
      Developer("Beni", "Beni", "", url("https://github.com/Beni"))
    ),
    homepage := Some(url("https://github.com/Beni/cats-effect-3-exercises")),
    licenses += ("MIT", url("http://opensource.org/licenses/MIT")),
    pomIncludeRepository := { _ => false },
    ThisBuild / testFrameworks += new TestFramework("munit.Framework"),
    scalacOptions in (Compile, doc) ++= Seq(
      "-groups",
      "-sourcepath",
      (baseDirectory in LocalRootProject).value.getAbsolutePath,
      "-doc-source-url",
      "https://github.com/Beni/cats-effect-3-exercises/blob/v" + version.value + "€{FILE_PATH}.scala"
    )
  )
)
