package convo

import scala.io.Source
import dictionary.encoder
import java.io.PrintWriter
import dictionary.{Word, Sentence}
import utilities.Config.vocabPath

object reactionAdder {

  //Näytetäänkö debuggausviestit konsolissa sitä mukaa kun
  //ohjelma luo lauseita ja tallentaa asioita tiedostoihin
  var debuggaus = false
  
  val filepath1 = vocabPath + "startingFiles/causalities.txt"
  val filepath2 = vocabPath + "startingFiles/association.txt"
  val filepath3 = vocabPath + "startingFiles/sentences.txt"

  private val soossit1 = Source.fromFile(filepath1).getLines().toBuffer
  private val soossit2 = Source.fromFile(filepath2).getLines().toBuffer
  private val soossit3 = Source.fromFile(filepath3).getLines().toBuffer

  //Lisää kausaliteetti tiedostoon
  def addCausality(s: String): Unit = {
    if (s.nonEmpty) {
      if (debuggaus)
        println("Lisättiin kausaliteetti : " + s)
      soossit1 += s
      this.printInFile1()
    }
  }

  //Lisää assosiaatio tiedostoon
  def addAssociation(s: String): Unit = {
    if (s.nonEmpty) {
      if (debuggaus) println("Lisättiin assosiaatio : " + s)
      soossit2 += s
      this.printInFile2()
    }
  }

  def opiAiheista(l: Sentence, v: Vector[Array[String]]): Unit =  {
    val pm = l.infinitives
    if (pm.intersect(v.flatten).nonEmpty) {
      val rivienMaara = v.count(x => !x.intersect(pm).isEmpty)
      if (rivienMaara > 1) {
          val rivi = formaatti(l)
          soossit3 += rivi
          if (debuggaus) println("Lisätty aihelause : " + rivi)
      }
    }
    this.printInFile3()
  }


  // *EI KÄYTETÄ OHJELMASSA

  def opiVastauksesta(l1: Sentence, l2: Sentence): String = {
    var counter = 0
    var common = false  // onko yleistetty esim 0xxx(S0)->0xxx(S0,Kysymyskerkki)
    val klooni1 = l1.clone()
    val klooni2 = l2.clone()
    val sanat1 = klooni1.wordList
    val sanat2 = klooni2.wordList
    val pm2 = sanat2.map( x => x.perusmuoto.getOrElse(x.teksti) )
    val causality1 = causality(klooni1,klooni2)  // yksittäinen
    for (i<- sanat1) {
      val i1 = i.perusmuoto.getOrElse(i.teksti)
      if (pm2.contains(i1) && !i.sanaluokka.contains("verbi") && i.sanaluokka.isDefined) {
        val kirjain: String = (i.sanaluokka.get match {
          case "substantiivi"| "nomini" => "S"
          case "adjektiivi" => "A"
          case _ => "M"
        }) + counter
        for (j <- (sanat1 ++ sanat2).filter { x => x.perusmuoto.getOrElse(x.teksti) == i1 }) {
          j.perusmuoto = Some(kirjain)
        }
        common = true
        counter += 1
      }
    }
    if (pm2.contains("se")) {
      val seVastine = viittaus(klooni1,sanat2(pm2.indexWhere { _== "se" }))
      if (seVastine.isDefined) {
        val se = seVastine.get
        val kirjain: String = (se.sanaluokka.get match {
          case "substantiivi"| "nomini" => "S"
          case "adjektiivi" => "A"
          case _ => "M"
        }) + counter
        se.perusmuoto = Some(kirjain)
        common = true
        counter += 1
      }
    }
    if (common) causality1 + "\n" + causality(klooni1,klooni2) else causality1  //yleinen
  }

  // luo formaatin mukaisen kausaliteetin kahdesta lauseesta
  def causality(l1: Sentence, l2: Sentence): String = formaatti(l1) + "->" + formaatti(l2)

  private def formaatti(l: Sentence) : String = { // luo formaatin mukaisen kausaliteetin yhdestä lauseesta
    val wordList = l.wordList

    def inflectionCode(word: Word) = {
      if (word.taivutusmuoto.isDefined)
        encoder.encode(Vector(word.taivutusmuoto.get))
      else "xxxx"
    }

    val inflections = wordList.map(inflectionCode(_)).reduceLeft(_ + _)

    def textify(s: Word) = s.perusmuoto.getOrElse(s.teksti)
    val infinitives = wordList.map(textify(_)).reduceLeft(_ + "," + _)
    inflections + "{" + infinitives + "}" // 8xxx0xxx + { + mennä,sinne + }
  }

  private def viittaus(l: Sentence, sana: Word) = {    // SE
    val muoto = sana.taivutusmuoto.getOrElse(0)
    val sanat = l.sanaluokka("substantiivi").filter { x => x.taivutusmuoto.getOrElse(0)/ 20 == muoto / 20 }
    val sanat1 = sanat.map(x=> x.taivutusmuoto.getOrElse(0))
    if (sanat.nonEmpty) {
      if ( sanat1.contains(muoto) )
        Some(sanat(sanat1.indexWhere(_ == muoto) ))
      else if (sanat1.contains(0))
        Some(sanat(sanat1.indexWhere { x => x > 3 && x <9 }) )
      else
        sanat.headOption
    } else None

  }

  //private def viittaus2(l: Sentence, sana: Word) = l.sanaluokka("erisnimi").headOption

  def aiheet(l: Sentence) = {
    val topics = aiheet1(l)
    if (topics.nonEmpty) addAssociation(topics)
  }

  // yleisluontoisia sanoja jotka eivät liity aiheina mihinkään, joten ne sivuutetaan
  val banni = Vector("olla","ei","asia","voida","tehdä","mennä","käydä",
      "tuoda","jaksaa","muistaa","pystyä","haluta","itse", "oppia", "saada",
      "asia", "tulla", "tykätä", "pitää", "taitaa", "käyttää", "osata", "hyvä",
      "kuulua", "ottaa"
  )

  private def aiheet1(l: Sentence) = {
    val filteredTopics = (l.sanaluokka("substantiivi") ++ l.sanaluokka("verbi") ++
      l.sanaluokka("partisiippi")++ l.sanaluokka("erisnimi"))
      .map(_.perusmuoto.getOrElse("NONE"))
      .filter(!banni.contains(_))

    //Luo formaatin mukainen rivi listasta
    if (filteredTopics.size > 1)
      filteredTopics.reduceLeft(_ + "," + _)
    else ""
  }

  //Funktiot puhujandatan (sisältäen uudet ehdot) tulostamiseksi uudestaan tiedostoon
  private def printInFile1() = {
    val printteri1 = new PrintWriter(filepath1)
    try
      for (i<- soossit1)
        printteri1.println(i)
    finally
      printteri1.close()
  }

  private def printInFile2() = {
    val printteri2 = new PrintWriter(filepath2)
    try
      for (i<- soossit2) if (i.nonEmpty) printteri2.println(i)
    finally
      printteri2.close()
  }

  private def printInFile3() = {
    val printteri = new PrintWriter(filepath3)
    try
      for (i<- soossit3) if (i.nonEmpty) printteri.println(i)
    finally
      printteri.close()
  }
}