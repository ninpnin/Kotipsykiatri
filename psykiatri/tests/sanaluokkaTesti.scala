package tests

import convo.Word
import dictionary.DictionaryFinder

object testi1 extends App {
  
  val lause1string = readLine("Kirjoita testilause tähän.")
  val lauseensanat = lause1string.filter(x => !(Vector('.',',','[',']').contains(x) )).toLowerCase.split(" ")
  val aika1 = System.currentTimeMillis()
  val lauseenvektori = Vector.tabulate[Word](lauseensanat.size)(x => DictionaryFinder.getWord(lauseensanat(x)))
  val aika2 = System.currentTimeMillis()
  for (i <- lauseenvektori) {
    println(i)
  }
  println("Aikaa sanojen hakemiseen kului " + (aika2-aika1)/1000.0 + " sekuntia.")
  
}