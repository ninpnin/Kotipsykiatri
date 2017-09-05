package tests

import dictionary.dictionaryFinder
import convo.reactionAdder
import scala.io.StdIn.readLine

object oppijaTesti2 extends App {

  while (true) {
    val rivi1 = readLine()
    val l1 = dictionaryFinder.getSentence(rivi1)
    val rivi2 = readLine()
    val l2 = dictionaryFinder.getSentence(rivi2)
    println(reactionAdder.opiVastauksesta(l1, l2))
  }
  
}