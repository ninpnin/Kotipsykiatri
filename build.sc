import mill._
import mill.scalalib._

object psykiatri extends ScalaModule {
  def scalaVersion = "2.12.4"

  //org.scala-lang.modules"  "scala-swing"  "2.1.1"
  def moduleDeps = Seq(dictionary, convo)
  def ivyDeps = Agg(
    ivy"org.scala-lang.modules::scala-swing:2.1.1"
  )

  def mainClass = Some("gui.kotipsykiatri")

  object utilities extends ScalaModule {
    def scalaVersion = "2.12.4"
  }

  object dictionary extends ScalaModule {
  	def moduleDeps = Seq(utilities)
    def scalaVersion = "2.12.4"
  }

  object convo extends ScalaModule {
  	def moduleDeps = Seq(dictionary, utilities)
    def scalaVersion = "2.12.4"
  }
}