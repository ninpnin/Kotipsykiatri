import mill._
import mill.scalalib._

object psykiatri extends ScalaModule {
  def scalaVersion = "2.12.4"

  object utilities extends ScalaModule {
    def scalaVersion = "2.12.4"
  }

  object dictionary extends ScalaModule {
  	def moduleDeps = Seq(utilities)
    def scalaVersion = "2.12.4"

  }

  object stems extends ScalaModule {
  	def moduleDeps = Seq(utilities, dictionary)
    def scalaVersion = "2.12.4"

  }

}