import sbt.*

object Dependencies {
  // cats
  val catsCore   = "org.typelevel" %% "cats-core"   % "2.13.0"
  val catsEffect = "org.typelevel" %% "cats-effect" % "3.5.7"
  val catsTest = "org.typelevel" %% "munit-cats-effect-3" % "1.0.7" % Test

  // tapir
  val tapirVersion = "1.11.13"

  val tapirHttp4s     = "com.softwaremill.sttp.tapir" %% "tapir-http4s-server"     % tapirVersion
  val tapirSwagger    = "com.softwaremill.sttp.tapir" %% "tapir-swagger-ui-bundle" % tapirVersion
  val tapirTethys     = "com.softwaremill.sttp.tapir" %% "tapir-json-tethys"       % tapirVersion
  val tapirSttpClient = "com.softwaremill.sttp.tapir" %% "tapir-sttp-client"       % tapirVersion

  // http4s
  val http4sVersion = "0.23.30"

  val http4sServer = "org.http4s" %% "http4s-ember-server" % http4sVersion
  val http4sDsl    = "org.http4s" %% "http4s-dsl"          % http4sVersion

  // sttp
  val sttpVersion = "3.10.2"

  val sttpCore = "com.softwaremill.sttp.client3" %% "core" % sttpVersion
  val sttpCats = "com.softwaremill.sttp.client3" %% "cats" % sttpVersion

  // logback
  val logback = "ch.qos.logback" % "logback-classic" % "1.5.16"

  // tethys
  val tethysVersion = "0.29.3"

  val tethysCore       = "com.tethys-json" %% "tethys-core"       % tethysVersion
  val tethysJackson    = "com.tethys-json" %% "tethys-jackson213" % tethysVersion
  val tethysDerivation = "com.tethys-json" %% "tethys-derivation" % tethysVersion

  // pureconfig
  val pureConfigVersion = "0.17.8"

  val pureConfigCore    = "com.github.pureconfig" %% "pureconfig-core"           % pureConfigVersion
  val pureConfigGeneric = "com.github.pureconfig" %% "pureconfig-generic-scala3" % pureConfigVersion

  // quartz4s
  val quartz = "com.itv" %% "quartz4s-core" % "1.0.4"

  val telegramiumCore = "io.github.apimorphism" %% "telegramium-core" % "9.803.0"
  val telegramiumHigh = "io.github.apimorphism" %% "telegramium-high" % "9.803.0"

  val allDeps: Seq[ModuleID] = Seq(
    catsCore,
    catsEffect,
    catsTest,
    tapirHttp4s,
    tapirSwagger,
    tapirTethys,
    tapirSttpClient,
    http4sServer,
    http4sDsl,
    sttpCore,
    sttpCats,
    logback,
    tethysCore,
    tethysJackson,
    tethysDerivation,
    pureConfigCore,
    pureConfigGeneric,
    quartz,
    telegramiumCore,
    telegramiumHigh
  )
}
