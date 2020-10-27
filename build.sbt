name := "outwatchcomponents"

version := "0.1"

scalaVersion := "2.13.2"  //5

//requireJsDomEnv in Test := true

//ThisBuild / useCoursier := false

//crossScalaVersions := Seq("2.11.12", "2.12.10", "2.13.0")

enablePlugins(ScalaJSPlugin, ScalaJSBundlerPlugin)

//dependencyOverrides in ThisBuild += "org.webjars.npm" % "js-tokens" % "5.0.0"

version in webpack := "4.41.2"

//version in startWebpackDevServer := "3.1.4"

scalaJSUseMainModuleInitializer := true

useYarn := true

scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule)) // configure Scala.js to emit a JavaScript module instead of a top-level script
//webpackBundlingMode := BundlingMode.Application

webpackConfigFile in fastOptJS := Some(baseDirectory.value / "webpack.config.dev.js")  //with 1.1.1

//webpackConfigFile in fastLinkJS := Some(baseDirectory.value / "webpack.config.dev.js")  //with 1.3.0

//skip in packageJSDependencies := false

resolvers += "jitpack" at "https://jitpack.io"

libraryDependencies ++= Seq(
  "com.chuusai" %%% "shapeless" % "2.3.3",
  "org.scalatest" %%% "scalatest" % "3.1.1" % Test,
  "com.github.outwatch.outwatch" %%% "outwatch" % "61deece",
  "com.github.outwatch.outwatch" %%% "outwatch-util" % "61deece",//If you want to use utilities for Store, WebSocket or Http, add the following:
  "com.github.cornerman.colibri" %%% "colibri-monix" % "9add104",
  "com.github.outwatch.outwatch" %%% "outwatch-monix" % "61deece",// for handler factories
)
