package tests

import dictionary.dictionaryFinder
import convo.SpeechGenerator
import convo.Conversation
import scala.io.StdIn.readLine

object puhujaTesti extends App {
  var a = ""
  
  val keskustelu = new Conversation()
  val munPuhuja = new SpeechGenerator("ewew","",keskustelu)
  
  def t1() = {
    a = readLine("kirjoita lause")
    val kaannetty = munPuhuja.kaannaLause(dictionaryFinder.getSentence(a))
    println(kaannetty)
    
  }
  def t2() = {
      val read = readLine("lause tähän : ")
      val lause = dictionaryFinder.getSentence(read)
      val ehto = readLine("ehto tähän : ")
      println(munPuhuja.rate(lause, ehto))
  }
  
  while (a != "lopeta") {
    t2()
    
  }

}