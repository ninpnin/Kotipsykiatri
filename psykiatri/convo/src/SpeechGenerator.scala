package convo

import scala.collection.mutable.Buffer
import scala.io.Source
import dictionary.DictionaryFinder._
import dictionary.encoder.{simpleDecode,decode}
import dictionary.DictionaryFinder
import dictionary.encoder
import dictionary.{Word, Sentence}

import utilities.rng
import utilities.rng.lottery
import utilities.Config.vocabPath

class SpeechGenerator(assosiaatioTiedosto: String, aihioTiedosto: String, keskustelu: Conversation) {
  
  var debugging = false
  def debugOn() = {
    this.debugging = true
    ReactionAdder.debuggaus = true
  }
  def debugOff() = {
    this.debugging = false
    ReactionAdder.debuggaus = false
  }

  var state = 0
  // Idle = 0
  // Passiivinen 1
  // Aiheet 2
  // Kysymykseen vastaaminen = 3

  def createSentence: Option[Sentence] = {
    // metodi palauttaa Nonen, jos state on muu kuin 1, 2 tai 3
    val lastSentence = keskustelu.newestSentenceTuple._1
    if (debugging)
      println("Generoi vastaus lauseeseen: " + lastSentence)
    this.state match {
      case 1 => {
        //Luo kausaliteettilause jos mahdollista
        var newSentence = createCausalitySentence(vocabPath + "puhujandata/aihiot1.txt", lastSentence)
        if (newSentence.isEmpty)
          newSentence = createCausalitySentence(vocabPath + "puhujandata/causalities.txt", lastSentence)
        this.state = 0
        //Jos ei kausaliteettia, luo aihelause todennäköisyydellä 0.6
        if (newSentence.isEmpty && lottery(0.6))
          newSentence = aihevastaus(vocabPath + "puhujandata/lauseet.txt")

        keskustelu.loytynyt = newSentence.isDefined
        if (newSentence.isDefined) {
          newSentence
        } else {
          this.state = 2
          Some(this.yleisvastaus(vocabPath + "puhujandata/yleisvastaukset.txt"))
        }
      }
      case 2 => {
        this.state = 0
        val reply = aihevastaus(vocabPath + "puhujandata/lauseet.txt")
        keskustelu.loytynyt = reply.isDefined
        reply
      }
      case 3 => {
        this.state = 0
        val non = lause3(vocabPath + "puhujandata/causalities.txt", lastSentence)
        keskustelu.loytynyt = non.isDefined
        if (non.isEmpty) Some(yleisvastaus(vocabPath + "puhujandata/yleiskvastaukset.txt")) else non
      }
      case _ => None
    }
  }

  private def createCausalitySentence(tiedosto: String, sentence: Sentence) = {
    val causalities = Source.fromFile(tiedosto)
      .getLines()
      .filter(x => x != null && x != "")
      .toVector
    sentenceFromConditions(causalities, sentence)
  }

  private def lause3(tiedosto: String,l: Sentence) = {  // filteröi muut kuin kysymykseen vastaukset
    val vektori = ( Source.fromFile(tiedosto).getLines()++Source.fromFile(vocabPath + "puhujandata/aihiot1.txt").getLines() ).filter(x => x!="" && x.split("->")(0).contains("Kysymysmerkki")).toVector
    println("vektori.size : " + vektori.size)
    sentenceFromConditions(vektori,l)
  }

  private def sentenceFromConditions(a: Vector[String], l: Sentence) = {
    val koko = a.size
    var palautus: Option[Sentence] = None

    var counter = 0
    val sanatO = l.wordList
    val sanat = sanatO.map(x=> x.perusmuoto.getOrElse(x.teksti))

    val bufferi = Buffer[String]()
    while (bufferi.size < 4 && counter < koko) {
      val rivi = a(counter)
      val current = rivi.substring(rivi.indexOf('{')+1,rivi.indexOf('}')).split(",")
      // ehdon perusmuodot

      val vaaditutSanat = current.filter { x => !checkF(x)}
      val vaaditut = vaaditutSanat.length
      val yhteiset = current.count(x => sanat.contains(x) )

      if (yhteiset + 1 >= vaaditut && yhteiset >= 1) {
        if (yhteiset >= vaaditut || yhteiset>=2 || !vaaditutSanat.contains("olla")) {
          var t = 0
          val currentSize = current.size

          if (rivi.indexOf('{') > 0) {

            val koodit = rivi.take(rivi.indexOf('{')).grouped(4).toVector
            val perusmuodot = current.toVector
            val tuplet = koodit zip perusmuodot
            for (i<-tuplet) {
              val vektors = decode(i._1)
              if (checkF(i._2)) {
                  val sanaluokka = i._2(0) match {
                    case 'S' => "substantiivi"
                    case 'A' => "adjektiivi"
                    case _ => "muut"
                  }
                  val sanago = l.sanaluokka(sanaluokka).map(x=> x.taivutusmuoto.getOrElse(0))
                  if (sanago.intersect(vektors).nonEmpty) t += 1 else t -=2
              } else {
                  val sana = sanatO.find(x => x.perusmuoto == Some(i._2) )
                  if (sana.isDefined)
                    if (vektors.contains(sana.get.taivutusmuoto.getOrElse(0)) || vektors.isEmpty)
                      t += 1
              }
            }
          } else t = currentSize
          if ( (t>= currentSize -1 && currentSize >1 ) || t >= currentSize) {
            bufferi += rivi
            if (debugging) { println("Löydettiin sopiva ehto", rivi) }
          }
        }
      }
      counter += 1
    }
    if (bufferi.nonEmpty) {
      val alternatives = rng.shuffle(bufferi)
      /*if (lottery(0.5) && false ) palautus = Some( lause1bCOPY(a.head,l) ) else {
      }*/
      val bestMatch = alternatives.maxBy { x => rate(l, x.split("->")(0)) }
      if (rng.nextInt(58) / 100.0 < rate(l, bestMatch.split("->")(0) ))
        palautus = Some(lause1bCOPY(bestMatch,l))
    }
    palautus
  }

  // Kuinka hyvin lause vastaa ehtoa
  def rate(l: Sentence, ehto: String): Double = {
    var rating = 0.0
    val indeksi = ehto.indexOf("{")

    val ehtozip = ehto.take(indeksi).grouped(4).toVector.map(x=> encoder.decode(x)) zip ehto.drop(indeksi+1).dropRight(1).split(",")

    val sanat = l.wordList
    val perusmuodot = l.infinitives

    for (i <- ehtozip) {

      if (perusmuodot.contains(i._2)) {
        val sanat1 = sanat.filter( x => x.perusmuoto== Some(i._2)).map(x=> x.taivutusmuoto.getOrElse(0))
        if (sanat1.intersect(i._1).nonEmpty || (i._1.isEmpty && sanat1.nonEmpty) ) rating += 1.0
      } else if (checkF(i._2)){
        val sanaluokka = i._2(0) match {
          case 'A' => "adjektiivi"
          case 'S' => "substantiivi"
          case _ => {
            if (sanat.count( x => x.sanaluokka.isEmpty && x.recognized ) > 0) rating += 0.7
            "hdhjdsjdshds"
          }
        }
        val sanat1 = sanat.filter( x => x.sanaluokka== Some(sanaluokka)).map(x=> x.taivutusmuoto.getOrElse(0))
        if (sanat1.intersect(i._1).nonEmpty) rating += 0.9
      } else {
        val sanaluokka = DictionaryFinder.getWord(i._2).sanaluokka.getOrElse("fdshfdshdsf")
        val sanat1 = sanat.filter( x => x.sanaluokka== Some(sanaluokka)).map(x=> x.taivutusmuoto.getOrElse(0))
        if (sanat1.intersect(i._1).nonEmpty) rating += 0.4
      }

    }

    //
    rating / (ehtozip.size+0.5)
  }

  val numerot = ('0' to '9').toVector


 def lause1bCOPY(s: String, l: Sentence): Sentence = {
    val sanat = Buffer[Word]()
    val split = s.split("->")
    val s0 = cleanHelper(split(0)) // poista mahdolliset suffiksit ehdosta
    val s1 = split(1)
    val lsanat = l.wordList

    val koodit0 = s0.take(s0.indexOf("{")).grouped(4).toVector.map { x => encoder.decode(x) }
    val sa0 = s0.drop(s0.indexOf("{")+1).dropRight(1).split(",")
    val koodit1 = s1.take(s1.indexOf("{")).grouped(4).toVector.map { x => encoder.simpleDecode(x) }
    val sa1 = s1.drop(s1.indexOf("{")+1).dropRight(1).split(",")

    // Tarkista onko geneerisiä relaaatioita
    // ts onko lauseessa johon vastataan sana jota pitää käyttää vastauksessa

    // ostin X:n -> minkä merkkisen X:n
    // ostin läppärin -> minkä merkkisen läppärin

    if (sa0.count(checkF(_))>0) {
      for (i <- sa0 zip koodit0) {
        if (checkF(i._1) && sa1.contains(i._1)) {
          var ab = ""
          val sanaluokka = i._1(0) match {
            case 'S' => "substantiivi"
            case 'A' => "adjektiivi"
            case _ => "muut"
          }
          if (sanaluokka == "muut") {
            ab = lsanat.find(x =>x.sanaluokka == None && x.perusmuoto!= None).get.perusmuoto.get
          } else {
            ab = lsanat.find(x =>x.sanaluokka == Some(sanaluokka) && i._2.contains(x.taivutusmuoto.getOrElse(99))).get.perusmuoto.get
            //println(ab)
          }
          sa1(sa1.indexOf(i._1)) = ab
        }
      }

    }

    // Luo lause-olion sanat
    for (i <- sa1 zip koodit1) {
        if (i._1!="")
          sanat += new Word(i._1,i._2)
    }
    if (debugging) {println("Käytettiin ehtoa : " + s0); println("Sopivuus : " +rate(l,s0))}
    new Sentence(sanat)
  }


  def checkF(t: String): Boolean = if (t.size == 2) { // tarkistaa formaatin
    if (('0' to '9').contains(t(1))) true else false  // (yleistetty ehto)
  } else false                                        // esim auto on "S1" koska se on substantiivi
                                                      // numero erottaa yleistettävät sanat toisistaan

  def matchTime(l: Sentence): Int = {
    var a = 1
    val filtered = l.wordList.filter(x => x.sanaluokka==Some("verbi"))
    for (i<- filtered) {
      if (i.taivutusmuoto.getOrElse(609) >= 8 && i.taivutusmuoto.getOrElse(420)<=13) a = 2
    }
    if (filtered.map(x => x.taivutusmuoto).contains(23) || filtered.map(x => x.taivutusmuoto).contains(24)) a = 3
    a
  }  // toteuta
  def matchNumber(l: Sentence): Int = 1  // toteuta


  private def sisSLX(x: String) =
    keskustelu.newestSentenceTuple._1
      .wordList
      .map(n=> n.sanaluokka)
      .count(x=> x == Some(x)) // sisältää sanaluokan X
  private def PMX(x: String) = keskustelu.newestSentenceTuple._1.wordList.map(n => n.perusmuoto).count(x=> x ==Some(x))
  private def sisPMX(x: String) = PMX(x) >=1

  def readFromData(s: String) = {

    def o(i: String): Word = {
      if (i.contains('(')) {
        new Word("")
      } else {
        new Word("")
      }
      new Word("")
    }

    val a = s.split("+").map(x=> o(x)).toBuffer
  }

  def checkState(): Unit = {    // tarkistaa keskustelun perusteella mikä tila on
    if (this.state==0) {
      val uusin = keskustelu.newestSentenceTuple
      if (uusin._2) {
        this.state = 1
        if (uusin._1.infinitives.contains("Kysymysmerkki")) {
          this.state = 3
        }
      }
    }
  }

  def kaannaLause(l: Sentence) = {
    val klooni = l.clone()
    val words = klooni.wordList
    if (l.infinitives.contains("ei")) {
      val kieltosana = words.find { x =>  x.perusmuoto==Some("ei") }.get
      val verbit = klooni.sanaluokka("verbi").filter { x => x.perusmuoto!=Some("ei") }
      if (verbit.nonEmpty) {
        val verbi = verbit.head
        var taivutusmuoto = kieltosana.taivutusmuoto.get
        if (taivutusmuoto == 0) taivutusmuoto = 3
        words(words.indexWhere { x => x.perusmuoto.contains("ei") }) = new Word(verbi.perusmuoto.get,taivutusmuoto)
        words.remove(words.indexWhere(x => x==verbi))
      }
    } else {
      val verbit = klooni.sanaluokka("verbi")
      if (verbit.nonEmpty) {
        val verbi = verbit( verbit.indexWhere(x => x.taivutusmuoto.get > 0 && x.taivutusmuoto.get <23  ) )
        val ei = verbi.taivutusmuoto.get match {
          case 1 | 8 | 15 => "en"
          case 2 | 9 | 16 => "et"
          case 4 | 11 | 18 => "emme"
          case 5 | 12 | 19 => "ette"
          case 6 | 13 | 20 => "eivät"
          case _ => "ei"
        }

        val negation = new Word(ei) { perusmuoto = Some("ei") }

      }
    }
    new Sentence(words)
  }

  val lauseet = Source.fromFile(vocabPath + "puhujandata/lauseet.txt").getLines().toBuffer

  private def aihevastaus(tiedosto: String): Option[Sentence] = {
    val soos = lauseet.map(x => x.drop(x.indexOf("{")+1).dropRight(1).split(","))
    val topics = keskustelu
      .aiheet
      .intersect(soos.flatten)
      .filter(!ReactionAdder.banni.contains(_))

    if (topics.nonEmpty) {
      val indeksi = soos.indexOf(soos.maxBy(x => x.intersect(topics).length) )
      val rivi = lauseet(indeksi)
      lauseet.remove(indeksi)
      val sanat = Buffer[Word]()
      val rivizip = rivi.dropRight(rivi.indexOf("{")-1).grouped(4)
        .toVector zip rivi.drop(rivi.indexOf("{")+1).dropRight(1).split(",")
      for (i <- rivizip)
        sanat += new Word(i._2,simpleDecode(i._1))

      if (debugging) println("Löydettiin yhteisiä aiheita : " + topics)
      Some( new Sentence(sanat) )
    } else None
  }

  private def yleisvastaus(tiedosto: String): Sentence = {  // geneerinen vastaus
    if (debugging) println("Ei löytynyt sopivia ehtoja. Arvottiin yleisvastaus.")
    val soos = Source.fromFile(tiedosto).getLines.toVector
    val rivi = soos(rng.nextInt(soos.size))
    DictionaryFinder.getSentence(rivi)
  }

  // Poista suffiksit ehdoista
  // {se+pä, kiva} => {se, kiva}
  private def cleanHelper(s: String) = {
    val words = s.split(',').toVector.map(_.split('+').head)
    words.reduceLeft(_ + "," + _)
  }
  def cleanCondition(s: String) = {
    val lists = s.split('{')
    for (i <- 1 until lists.length) {
      val current = lists(i).split('}')
      lists(i) = cleanHelper(current.head) + "}" + current.last
    }

    lists.reduceLeft(_ + "{" + _)
  }
}

object SpeechGenerator {
  
}