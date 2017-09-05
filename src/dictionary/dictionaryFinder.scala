package dictionary

import scala.io.Source

import convo.Sentence
import convo.Word
import utilities.letterCollection.{numerot, aakkoset}

object dictionaryFinder {

  //Sanojen j�lkiliitteet, jotka sivuutetaan tunnistaessa
  //Esimerkiksi olisi-pa
  val suffixes4 = Vector("kaan","k��n")
  val suffixes3 = Vector("han","h�n","pas","p�s","kin")
  val suffixes2 = Vector("pa","p�","ko","k�","go","g�","ni","si")

  //Luo lauseolio merkkijonosta. Ylimm�n tason funktio.
  def getSentence(s: String) = {
    val texts = s.replace("?", " ? ").replace(",", " , ").split(" ").filter { x => x!="" }
    val a = texts.map(getWord(_)).toBuffer
    new Sentence(a)
  }

  //Luo sanaolio merkkijonosta
  def getWord(text: String): Word = {
    var sana: Option[Word] = getPunctuation(text)
    if (sana.isEmpty) {
      if (suffixes4.contains(text.takeRight(4)) && text.length > 5) { // tarkistetaan onko loppuliitteit�
        sana = getWordWithoutSuffix(text.dropRight(4))
        if (sana.isDefined) sana.get.suffix = Some(text.takeRight(4))
      } else if (suffixes3.contains(text.takeRight(3)) && text.length > 4) { // tarkistetaan onko loppuliitteit�
        sana = getWordWithoutSuffix(text.dropRight(3))
        if (sana.isDefined) sana.get.suffix = Some(text.takeRight(3))
      } else if (suffixes2.contains(text.takeRight(2)) && text.length >3) {
        sana = getWordWithoutSuffix(text.dropRight(2))
        if (sana.isDefined) sana.get.suffix = Some(text.takeRight(2))
      }
      if (sana.isEmpty) sana = getWordWithoutSuffix(text)
    }
    sana.getOrElse(new Word(text))
  }

  //Apufunktioita sanan etsimist� varten
  private def getPunctuation(text: String): Option[Word] = {
    if (numerot.contains(text(0))) {
      Some(new Word(text){ perusmuoto=Some(text)})
    } else if (text(0)=='?' || text == "Kysymysmerkki") {
      Some(new Word("?") )
    } else if (text(0)==',' || text == "Pilkku"){
      Some(new Word(","))
    } else {
      None
    }
  }
  
  private def getWordWithoutSuffix(text: String): Option[Word] = {
    val alkukirjain = text.head.toUpper
    var sana: Option[Word] = if (alkukirjain == text(0)) haeErisnimi(text) else getPronoun(text)
    //Hae yhdyssana jos sana on tarpeeksi pitk�
    if (text.length > 9) sana = getCompound(text)
    //korvataan silmukkarakenteella.
    if (sana.isEmpty)
      sana = getOthers(text)
    if (sana.isEmpty)
      sana = getPredicate(text)
    if (sana.isEmpty)
      sana = getNominal(text,s("sanastot/"+alkukirjain+"/"+alkukirjain+"saannollisetNominit.txt"))//haePredikaatti(text)
    if  (sana.isEmpty)
      sana = getParticipe(text)
    if (sana.isEmpty)
      sana = getOthers(text)
    if (sana.isEmpty)
      sana = getCompound(text)
    if (sana.isEmpty)
      sana = haeErisnimi(alkukirjain + text.drop(1))
    sana
  }

  private def getPronoun(sana: String): Option[Word] = {
    var s: Option[Word] = None
    val a = Source.fromFile("sanastot/yleiset/pronominit.txt").getLines().toVector
    var counter = 0
    while (s.isEmpty && counter < a.size) {
      val rivi = a(counter).split(",")
      if (rivi != null && rivi.nonEmpty) {
        for (i <- 0 until rivi.size) {
          if (rivi(i) == sana) {
            val imuoto = i % 12 + (20 * (i/12))
            s = Some(new Word(sana) { perusmuoto = Some(rivi(0))
              taivutusmuoto = Some(imuoto)})
          }
        }
      }
      counter += 1
    }
    s
  }

  private def s(lukutiedosto: String) = Source.fromFile(lukutiedosto).getLines().toVector

  private def getNominal(sana: String, tiedostonrivit: Vector[String]): Option[Word] = {
    val sananpituus = sana.length
    var found: Option[Word] = None
    val koko = tiedostonrivit.length
    var counter = 0
    val mappi = Map('n'->Vector(0,1,6),'a'->Vector(0,3,4,5,7,8,10),'�'->Vector(0,3,4,5,7,8,10),'e'->Vector(0,9),'i'->Vector(0,11))
    val vektori = if (mappi.keys.toVector.contains(sana.last)) mappi(sana.last) else Vector(0)
    while (counter < koko && found.isEmpty) {
      val rivi = tiedostonrivit(counter)
      if (rivi != null && rivi.length - 5 <= sananpituus) {
          val rivi = tiedostonrivit(counter)
          found = getNominalByClass(sana,rivi,vektori)
      }
      counter += 1
    }
    found
  }//???  // konjunktiot (ett�, mutta), persoonapronominit (min�, sinua)

  def getNominalByClass(sana: String, rivi: String, vektori: Vector[Int]) = {
    var found: Option[Word] = None
    val sanake = createNominal(rivi)
      for (i <- vektori) {
        if (found.isEmpty) {
          if (sanake.taivuta(i, true) == sana) {
            val sanaluokka = rivi(rivi.length-4).toString match {
              case "A" => "adjektiivi"
              case _ => "substantiivi"
            }
            val sanago = new Word(sana)
            sanago.perusmuoto = Some(sanake.sana)
            sanago.taivutusmuoto = Some(i+20)
            sanago.sanaluokka = Some(sanaluokka)
            found = Some(sanago)
          }
          if (sanake.taivuta(i, false) == sana) {
            val sanaluokka = rivi(rivi.length-4).toString match {
              case "A" => "adjektiivi"
              case _ => "substantiivi"
            }
            val sanago = new Word(sana)
            sanago.perusmuoto = Some(sanake.sana)
            sanago.taivutusmuoto = Some(i)
            sanago.sanaluokka = Some(sanaluokka)
            found = Some(sanago)
          }
        }
      }
    found
  }

  private def getParticipe(haettava: String): Option[Word] = {
    val sananpituus = haettava.length
    val alkukirjain = haettava(0).toUpper
    var found: Option[Word] = None
    val tiedosto = Source.fromFile("sanastot/"+alkukirjain+"/"+alkukirjain+"saannollisetVerbit.txt")
    val tiedostonrivit = tiedosto.getLines().toVector//.filter { x => x(0)==sana(0) }
    val koko = tiedostonrivit.size
    var counter = 0
    val vektori = (0 to 11).toVector
    while (counter < koko && found.isEmpty) {
      val rivi = tiedostonrivit(counter)
      if (rivi!=null && rivi.length - 4 <= sananpituus) {

          val rivi = tiedostonrivit(counter)
          val sanake: Verb = createVerb(rivi)
          val t1 = haettava.takeRight(3)
          if (t1=="nyt" || t1=="nut") {
            if (sanake.taivuta(23)==haettava) {
              val sanago = new Word(haettava)
                sanago.perusmuoto = Some(sanake.sana)
                sanago.taivutusmuoto = Some(23)
                sanago.sanaluokka = Some("verbi")
                found = Some(sanago)
            }
          } else if (t1 == "eet"){
            if (sanake.taivuta(24)==haettava) {
              val sanago = new Word(haettava)
                sanago.perusmuoto = Some(sanake.sana)
                sanago.taivutusmuoto = Some(24)
                sanago.sanaluokka = Some("verbi")
                found = Some(sanago)
            }
          } else {
            val ma = Kotus10(sanake.taivuta(25),'X')
            for (i <- vektori) {
              if (ma.taivuta(i, true) == haettava) {
                val sanago = new Word(haettava)
                sanago.perusmuoto = Some(sanake.sana)
                sanago.taivutusmuoto = Some(i+20)
                sanago.sanaluokka = Some("partisiippi")
                found = Some(sanago)
              }
              if (ma.taivuta(i, false) == haettava) {
                val sanago = new Word(haettava)
                sanago.perusmuoto = Some(sanake.sana)
                sanago.taivutusmuoto = Some(i)
                sanago.sanaluokka = Some("partisiippi")
                found = Some(sanago)
              }
            }
          }
      }
      counter += 1
    }
    found
  }

  private def getPredicate(haettava: String): Option[Word] = {
    val sananpituus = haettava.length
    val alkukirjain = haettava(0).toUpper
    var found: Option[Word] = haeVerbi(haettava)
    val tiedosto = Source.fromFile("sanastot/"+alkukirjain+"/"+alkukirjain+"saannollisetVerbit.txt")
    val tiedostonrivit = tiedosto.getLines().toVector
    val koko = tiedostonrivit.size
    var counter = 0
    val mappi = Map('n'->Vector(1,7,8,14,15),'e'->Vector(3,4,5,11,12,18,19,22),'i'->Vector(3,10,17,22),'t'->Vector(2,6,9,13,20))
    val vektori = if (mappi.keys.toVector.contains(haettava.last)) mappi(haettava.last) else Vector(0,3,22)
    while (counter < koko && found.isEmpty) {
      val rivi = tiedostonrivit(counter)
      if (rivi!=null && sananpituus >= rivi.length-5) found = getVerb(haettava,rivi,vektori)
      counter += 1
    }
    found
  }

  def getVerb(sana: String, rivi: String, vektori: Vector[Int]) = {
    var found: Option[Word] = None
    val sanake = createVerb(rivi)
      for (i <- vektori) {
        if (sanake.taivuta(i) == sana && found.isEmpty) {
          val sanago = new Word(sana)
          sanago.perusmuoto = Some(sanake.sana)
          sanago.taivutusmuoto = Some(i)
          sanago.sanaluokka = Some("verbi")
          found = Some(sanago)
        }
      }
    found
  }

  private def haeVerbi(haettava: String): Option[Word] = {  // ep�s��nn�lliset verbit
    var found: Option[Word] = None
    val b = Source.fromFile("sanastot/yleiset/verbit.txt").getLines().toVector
    var counter = 0
    while (found.isEmpty && counter < b.size) {
      val rivi = b(counter).split(",")
      if (rivi!=null) {
        for (i <- 0 until rivi.size) {
          if (rivi(i) == haettava) {
            found = Some(new Word(haettava) {perusmuoto = Some(rivi(0))
              taivutusmuoto=Some(i)
              sanaluokka = Some("verbi")})
          }
        }
      }
      counter += 1
    }
    found
  }

  private def haeErisnimi(text: String): Option[Word] = {
    var found: Option[Word] = None
    val alkukirjain = text(0)
    val soossit = Source.fromFile("sanastot/erisnimet.txt").getLines.toVector.filter(_(0) == alkukirjain)
    val size = soossit.size
    var counter = 0
    while (counter < size && found.isEmpty) {
      val rivi = soossit(counter)
      val sanake = createNominal(rivi)
      for (i <- 0 until 11) {
        if (text == sanake.taivuta(i, true)) {
          found = Some(new Word(text) { perusmuoto = Some(rivi.dropRight(4))
          sanaluokka = Some("erisnimi")
          taivutusmuoto = Some(i + 20)})
        } else if (text==sanake.taivuta(i, false)) {
          found = Some(new Word(text) { perusmuoto= Some(rivi.dropRight(4))
          sanaluokka = Some("erisnimi")
          taivutusmuoto = Some(i)})

        }
      }
      counter += 1
    }
    found
  }

  def getCompound(t: String) = {
    val alkukirjain = t(0).toUpper
    val source = Source.fromFile("sanastot/" + alkukirjain +"/yhdyssanat.txt").getLines().toVector
    getCompoundByInitial(t,source)
  }
  def getCompoundByInitial(text: String, vektori: Vector[String]): Option[Word] = {
    var found: Option[Word] = None
    val vektorinKoko = vektori.size
    var counter = 0
    while (counter < vektorinKoko && found.isEmpty) {
      val rivi = vektori(counter).split("&")
      if (text.take(rivi(0).length) == rivi(0)) {
        val loppuosa = text.drop(rivi(0).length)
        var semiFound: Option[Word] = None
        val luokka = rivi(1).takeRight(2).toInt
        if (luokka >= 1 && luokka < 52) {
          semiFound = getNominalByClass(loppuosa, rivi(1), (0 to 11).toVector)
        } else if (luokka > 51 && luokka < 99) {
          semiFound = getVerb(loppuosa, rivi(1), (0 to 23).toVector)
        }

        if (semiFound.isDefined) {
          found = Some(new Word(text) {
              perusmuoto = Some(text.take(rivi(0).length) + "" + semiFound.get.perusmuoto.get)
              taivutusmuoto = semiFound.get.taivutusmuoto
              sanaluokka = semiFound.get.sanaluokka
            }
          )
        }
      }
      counter += 1
    }
    found
  }

  private def getAdjective(sana: String): Option[Word] = None//???  // my�s partisiipit, esim osaava

  private def getOthers(sana: String): Option[Word] = {
    val kirjain = sana(0).toUpper
    var s: Option[Word] = None
    if (aakkoset.contains(kirjain.toLower)) {
      val a = Source.fromFile("sanastot/" + kirjain + "/" + kirjain + "epasaannolliset.txt").getLines().toVector
      var counter = 0
      while (s == None && counter < a.size) {
        val rivi = a(counter).dropRight(4)
        if (rivi == sana) {
          s = Some(new Word(rivi){perusmuoto = Some(rivi)})
        }
        counter += 1
      }
    }
    s
  }

  def getInfinitive(t: String): String = getWord(t).perusmuoto.getOrElse("homovitu")
  private def createNominal(text: String): Nominal = {
    val tiedot = text.takeRight(4)
    val perusmuoto = text.dropRight(4)
    tiedot.takeRight(2) match {  //
      case "02" => Kotus2(perusmuoto,tiedot(1))
      case "03" => Kotus3(perusmuoto,tiedot(1))
      case "05" => Kotus5(perusmuoto,tiedot(1))
      case "06" => Kotus6(perusmuoto,tiedot(1))
      case "09" => Kotus9(perusmuoto,tiedot(1))
      case "10" => Kotus10(perusmuoto,tiedot(1))
      case "12" => Kotus12(perusmuoto,'X')
      case "38" => Kotus38(perusmuoto,'X')
      case "39" => Kotus39(perusmuoto,'X')
      case "40" => Kotus40(perusmuoto,'X')
      case "48" => Kotus48(perusmuoto,tiedot(1))
      case _ => Kotus1(perusmuoto,tiedot(1))
    }
  }
  private def createVerb(text: String): Verb = {
    val tiedot = text.takeRight(4)
    val perusmuoto = text.dropRight(4)
    tiedot.takeRight(2) match {  //
      case "53" => Kotus53(perusmuoto,tiedot(1))
      case "62" => Kotus62(perusmuoto,tiedot(1))
      case "67" => Kotus67(perusmuoto,tiedot(1))
      case "73" => Kotus73(perusmuoto,tiedot(1))
      case _ => Kotus52(perusmuoto,tiedot(1))
    }
  }
}