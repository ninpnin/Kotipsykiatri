package convo

import scala.collection.mutable.Buffer
import scala.io.Source
import dictionary.dictionaryFinder
import utilities.rng.lottery

class Conversation {
  
  var loytynyt = false

  // Keskustelun kaikki lauseet järjestyksessä.
  // Boolean-arvo kertoo kuka lauseen on sanonut;
  // käyttäjä true, kone false
  private val lauseet = Buffer[(Sentence,Boolean)]()

  lazy val aiheTiedosto = "puhujandata/aiheet.txt"
  lazy val mahdollisetAiheet = Source.fromFile("puhujandata/aiheet.txt").getLines().toVector.map(x => x.split(","))
  lazy val aiheet = Buffer[String]()
  lazy val banni = Vector("olla","ei","asia","voida","tehdä","NONE","mennä","käydä")


  //Aloita keskustelu lauseella "Moi, olen Kalle Koneaivo".
  this.appendSentence("moi, olen Kalle Koneaivo",false)


  def getTopics(l: Sentence) = {
    val sanat = (l.sanaluokka("verbi") ++ l.sanaluokka("substantiivi") ++ l.sanaluokka("erisnimi")).map { x => x.perusmuoto.getOrElse("NONE") }
    for (perusmuoto <- sanat) {
      if (!banni.contains(perusmuoto)) aiheet += perusmuoto
      for (i <- mahdollisetAiheet) {
         if (i.contains(perusmuoto)) {
            for (j <- i) aiheet += j
         }
      }
    }
    if (aiheet.size>30) for (i <- 0 until 5) aiheet.remove(0)
  }

  def appendSentence(s: String, kayttaja: Boolean): Unit = if (s!="") {
    val tuple = (dictionaryFinder.getSentence(s.replaceAll("[^A-Za-z0-9._,_?_ä_ö_Ä_Ö ]", "")),kayttaja)
    this.appendSentence(tuple._1, tuple._2)
    if (kayttaja) {
      if (loytynyt) opiV() else reactionAdder.opiAiheista(tuple._1, this.mahdollisetAiheet)
      reactionAdder.aiheet(tuple._1)
    }
  }

  def appendSentence(l: Sentence, b: Boolean): Unit = {
    if (b || lottery(0.5)) this.getTopics(l)
    val tuple = (l,b)
    lauseet += tuple
  }
  
  def newestSentenceText: String = lauseet.last._1.toString()  // palauttaa lauseen tekstin
  def newestSentenceTuple: (Sentence,Boolean) = lauseet.last // palauttaa (Lause,Boolean) -tuplen
  
  
  def opiV(): Unit = {   //kutsutaan Oppija-olion opiVastauksesta-metodia kahdesta viimeisestä lauseesta (ihminen ja kone)
    if (lauseet.size > 3) {
      val index2 = lauseet.lastIndexWhere(x => x._2)
      val index1 = if (!lauseet(index2-1)._2) index2 - 1 else 609
      if (index1 >= 0 && index2 >= 0 && index2 > index1) {
        val l1 = lauseet(index1)._1
        val l2 = lauseet(index2)._1
        println(l2.noneCount, l2.length)
        if ( l2.noneCount < 2 && l2.noneCount < l2.length) {
          val causality = reactionAdder.opiVastauksesta(l1, l2)
          reactionAdder.addCausality( causality )
        }
      }
    }
  }
  
}