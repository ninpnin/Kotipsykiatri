import mill._
import mill.scalalib._

object psykiatri extends ScalaModule {
  def scalaVersion = "3.1.3"

  //org.scala-lang.modules"  "scala-swing"  "2.1.1"
  def moduleDeps = Seq(dictionary, convo)
  def ivyDeps = Agg(
    ivy"org.scala-lang.modules::scala-swing:3.0.0"
  )

  def mainClass = Some("gui.kotipsykiatri")

  object utilities extends ScalaModule {
    def scalaVersion = "3.1.3"
  }

  object dictionary extends ScalaModule {
  	def moduleDeps = Seq(utilities)
    def scalaVersion = "3.1.3"
  }

  object stems extends ScalaModule {
  	def moduleDeps = Seq(utilities, dictionary)
    def scalaVersion = "3.1.3"
  }

  object convo extends ScalaModule {
  	def moduleDeps = Seq(dictionary, utilities)
    def scalaVersion = "3.1.3"
  }
}