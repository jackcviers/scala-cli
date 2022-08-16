package scala.cli.commands

import caseapp.core.RemainingArgs

import scala.build.internal.Constants
import scala.cli.ScalaCli
import scala.cli.internal.ProcUtil
import scala.cli.signing.shared.Secret
import scala.cli.commands.util.ScalaCliSttpBackend

// current version / latest version + potentially information that
// scala-cli should be updated (and that should take SNAPSHOT version
// into account and mention that one is ahead of stable version)

// the state of bloop (running/not running + version and JVM used)

// if there are duplicated scala-cli on classpath

// whether all native dependencies for native / js are installed

// information about location of binary / main class that is being used

// information if scala-cli can access Maven central / scala-cli
// github with some tips and diagnostics about proxies
// (@alexarchambault could you provide more details on what can be
// printed?)

// information if scala-cli is used as a native application or is using JVM

object Doctor extends ScalaCommand[DoctorOptions] {
  override def group = "Doctor"

  def run(options: DoctorOptions, args: RemainingArgs): Unit = {
    checkIsVersionOutdated(options.ghToken.map(_.get()))
    checkBloopStatus()
    checkDuplicatesOnPath()
    checkNativeDependencies()
    checkJSDependencies()
    // checkBinaryOrMainClass()??
    checkAccessToMavenOrGithub()
    checkIsNativeOrJvm()

    println("") // sometimes last line is cut off
  }

  private def checkIsVersionOutdated(ghToken: Option[Secret[String]]): Unit = {
    val currentVersion        = Constants.version
    val newestScalaCliVersion = Update.newestScalaCliVersion(ghToken)
    val isOutdated = CommandUtils.isOutOfDateVersion(newestScalaCliVersion, currentVersion)
    if (isOutdated)
      println(
        s"Your scala-cli version is out of date. your version: $currentVersion. please update to: $newestScalaCliVersion"
      )
    else
      println(s"Your scala-cli version ($currentVersion) is current.")
  }

  private def checkBloopStatus(): Unit = {
    // TODO
  }

  // the semantics of PATH isn't just built into unix shells.  it is
  // part of the 'exec' series of system calls.
  private def checkDuplicatesOnPath(): Unit = {
    val scalaCliPaths = ProcUtil.findApplicationPathsOnPATH(ScalaCli.progName)

    if (scalaCliPaths.size > 1)
      println(
        s"scala-cli would not be able to update itself since it is installed in multiple directories: ${scalaCliPaths.mkString(", ")}."
      )
    else if (Update.isScalaCLIInstalledByInstallationScript())
      println(
        s"scala-cli could update itself since it is correctly installed in only one location: ${scalaCliPaths.mkString}."
      )
    else
      println(
        s"scala-cli can be updated by your package manager since it is correctly installed in only one location: ${scalaCliPaths.mkString}."
      )

  }

  private def checkNativeDependencies(): Unit = {
    // TODO
  }

  private def checkJSDependencies(): Unit = {
    // TODO
  }

  // checkBinaryOrMainClass()??

  private def checkAccessToMavenOrGithub(): Unit = {
    // todo: maybe move to top?
    import scala.cli.commands.util.CommonOps._
    import scala.util.chaining._
    val loggingOptions = LoggingOptions()
    val logger         = loggingOptions.logger

    val backend = ScalaCliSttpBackend.httpURLConnection(logger)

    import sttp.client3.basicRequest
    import sttp.client3._

    try {
      val responseMvnCentralDir = basicRequest.get(
        uri"https://repo1.maven.org/maven2/ch/epfl/scala/library-example_2.13/1.0.1/"
      ).send(backend)

      val responseMvnCentralPom = basicRequest.get(
        uri"https://repo1.maven.org/maven2/ch/epfl/scala/library-example_2.13/1.0.1/library-example_2.13-1.0.1.pom"
      ).send(backend)

      val responseMvnCentralJar = basicRequest.get(
        uri"https://repo1.maven.org/maven2/ch/epfl/scala/library-example_2.13/1.0.1/library-example_2.13-1.0.1.jar"
      ).send(backend)

      val checks = List(
        "mvncentral access"  -> (responseMvnCentralDir.code.code != 200),
        "mvn central jar dl" -> (responseMvnCentralJar.code.code != 200),
        "mvncentral pom dl"  -> (responseMvnCentralPom.code.code != 200),
        "mvn central access proxied" -> ((responseMvnCentralDir.code.code == 403 || responseMvnCentralDir.code.code == 504) && responseMvnCentralDir.headers.exists(header => header.name == "Proxy-Connection" && header.value == "Keep-Alive"))
      )

      println(responseMvnCentralDir.code.code)


      println("maven central checks:")
      checks
        .map { case (prompt, passed) => if(passed) s"$prompt: X" else s"$prompt: O" }.mkString(
          "\t",
          "\n\t",
          ""
        )
        .pipe(println)
    }
    catch {
      case ce: SttpClientException.ConnectException =>
        println("Couldn't connect to maven central, maybe you're offline?")
    }
  }

  private def checkIsNativeOrJvm(): Unit = {
    if (System.getProperty("org.graalvm.nativeimage.kind") == "executable")
      println("Your scala-cli is a native application.")
    else {
      val jvmVersion        = System.getProperty("java.vm.name")
      val javaVendorVersion = System.getProperty("java.vendor.version")
      println(
        s"Your scala-cli is using the java launcher with JVM: $jvmVersion ($javaVendorVersion)."
      )
    }
  }

}
