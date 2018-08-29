package tests

import dictionary.dictionaryFinder
import dictionary.encoder
import scala.io.StdIn.readLine

object koodaa extends App {

  def koodaaLause(s: String) = {
    val lause = dictionaryFinder.getSentence(s)
    var string = ""
    var string2 = "{"
    for (i <- lause.wordList) {
      string += encoder.encode(Vector(i.taivutusmuoto.getOrElse(0)))
      string2 += i.perusmuoto.getOrElse("None") +  ","
    }
    string + string2.dropRight(1) + "}"
  }
  
  var a = ""
  
  while (a != "lopeta") {
    a = readLine("kirjoita lause")
    println(koodaaLause(a))
  }
  
}