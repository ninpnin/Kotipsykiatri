package utilities

/**
  * Created by vaino on 4.9.2017.
  */
import scala.util.Random

object rng extends Random {

  //Arpoo todennäköisyydellä p true ja 1 - p false
  def lottery(p: Double): Boolean = p > this.nextDouble()

}
