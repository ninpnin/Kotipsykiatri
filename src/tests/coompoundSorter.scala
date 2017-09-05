package tests

import scala.io.Source
import java.io.PrintWriter

import dictionary.wordInflector
import utilities.letterCollection

//Tämä tiedosto lajittelee yhdys.txt:n yhdyssanat jokaisen kirjaimen omaan yhdyssanatiedostoon.
object coompoundSorter extends App {

  val aakkoset = letterCollection.aakkoset.map(_.toUpper)

  def dropp(t: String) = t.split("&")
  
  def j() = {
    val source = Source.fromFile("yhdys.txt").getLines().toVector
    
    for (kirjain <- aakkoset) {
      println(kirjain)
      val filtered = source.filter { x => x.head.toLower == kirjain.toLower }
      val f1 = filtered.map(x=> x +wordInflector.partOfSpeech( dropp(x)(1) ).getOrElse("SX01")  )
      val p = new PrintWriter("sanastot/" + kirjain + "/yhdyssanat.txt")
      try for(n<-f1) p.println(n) finally p.close()
    }
  }
  
  this.j()
}