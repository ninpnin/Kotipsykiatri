package dictionary

import scala.io.Source

import dictionary.Sentence
import dictionary.Word
import utilities.letterCollection.{numerot, aakkoset}
import utilities.Config.vocabPath

object DictionaryFinder {

  //Sanojen jälkiliitteet, jotka sivuutetaan tunnistaessa
  //Esimerkiksi olisi-pa
  val suffixes4 = Vector("kaan","kään")
  val suffixes3 = Vector("han","hän","pas","päs","kin")
  val suffixes2 = Vector("pa","pä","ko","kö","go","gö","ni","si")

  //Luo lauseolio merkkijonosta. Ylimmän tason funktio.
  def getSentence(s: String) = {
    val texts = s.replace("?", " ? ").replace(",", " , ").split(" ").filter { x => x!="" }
    val a = texts.map(getWord(_)).toBuffer
    new Sentence(a)
  }

  //Luo sanaolio merkkijonosta
  def getWord(text: String): Word = {
    var sana: Option[Word] = getPunctuation(text)
    if (sana.isEmpty) {
      if (suffixes4.contains(text.takeRight(4)) && text.length > 5) { // tarkistetaan onko loppuliitteitä
        sana = getWordWithoutSuffix(text.dropRight(4))
        if (sana.isDefined) sana.get.suffix = Some(text.takeRight(4))
      } else if (suffixes3.contains(text.takeRight(3)) && text.length > 4) { // tarkistetaan onko loppuliitteitä
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

  //Apufunktioita sanan etsimistä varten
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
    //Hae yhdyssana jos sana on tarpeeksi pitkä
    if (text.length > 9) sana = getCompound(text)
    //korvataan silmukkarakenteella.
    if (sana.isEmpty)
      sana = getOthers(text)
    if (sana.isEmpty)
      sana = getPredicate(text)
    if (sana.isEmpty)
      sana = getNominal(text,s(vocabPath + "sanastot/"+alkukirjain+"/"+alkukirjain+"saannollisetNominit.txt"))//haePredikaatti(text)
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
    val a = Source.fromFile(vocabPath + "sanastot/yleiset/pronominit.txt").getLines().toVector
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
    val mappi = Map('n'->Vector(0,1,6),'a'->Vector(0,3,4,5,7,8,10),'ä'->Vector(0,3,4,5,7,8,10),'e'->Vector(0,9),'i'->Vector(0,11))
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
  }//???  // konjunktiot (että, mutta), persoonapronominit (minä, sinua)

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
    val tiedosto = Source.fromFile(vocabPath + "sanastot/"+alkukirjain+"/"+alkukirjain+"saannollisetVerbit.txt")
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
    val tiedosto = Source.fromFile(vocabPath + "sanastot/"+alkukirjain+"/"+alkukirjain+"saannollisetVerbit.txt")
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

  private def haeVerbi(haettava: String): Option[Word] = {  // epäsäännölliset verbit
    var found: Option[Word] = None
    val b = Source.fromFile(vocabPath + "sanastot/yleiset/verbit.txt").getLines().toVector
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
    val soossit = Source.fromFile(vocabPath + "sanastot/erisnimet.txt").getLines.toVector.filter(_(0) == alkukirjain)
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
    val source = Source.fromFile(vocabPath + "sanastot/" + alkukirjain +"/yhdyssanat.txt").getLines().toVector
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

  private def getAdjective(sana: String): Option[Word] = None//???  // myös partisiipit, esim osaava

  private def getOthers(sana: String): Option[Word] = {
    val kirjain = sana(0).toUpper
    var s: Option[Word] = None
    if (aakkoset.contains(kirjain.toLower)) {
      val a = Source.fromFile(vocabPath + "sanastot/" + kirjain + "/" + kirjain + "epasaannolliset.txt").getLines().toVector
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

  def getInfinitive(t: String): String = getWord(t).perusmuoto.getOrElse("undefined")

  def getStems(t: String) = {
    val code = t.takeRight(4)
    code(0) match {
      case 'V' => createVerb(t).stems
      case _ => createNominal(t).stems
    }
  }

  private def createNominal(text: String): Nominal = {
    val tiedot = text.takeRight(4)
    val stem = text.dropRight(4)
    val code = tiedot.takeRight(2)
    val gradation = tiedot(1)
    matchNominal(code, stem, gradation)
  }

  private def createVerb(text: String): Verb = {
    val tiedot = text.takeRight(4)
    val stem = text.dropRight(4)
    val code = tiedot.takeRight(2)
    val gradation = tiedot(1)
    matchVerb(code, stem, gradation)
  }

  // Match code in format ['SX01' in valoSX01] to correct case classes 
  private def matchNominal(code: String, stem: String, gradation: Char): Nominal = code match {  //
    case "02" => Kotus2(stem, gradation)
    case "03" => Kotus3(stem, gradation)
    case "05" => Kotus5(stem, gradation)
    case "06" => Kotus6(stem, gradation)
    case "09" => Kotus9(stem, gradation)
    case "10" => Kotus10(stem, gradation)
    case "12" => Kotus12(stem,'X')
    case "38" => Kotus38(stem,'X')
    case "39" => Kotus39(stem,'X')
    case "40" => Kotus40(stem,'X')
    case "48" => Kotus48(stem, gradation)
    case _ => Kotus1(stem, gradation)
  }

  private def matchVerb(code: String, stem: String, gradation: Char): Verb = code match {  //
    case "53" => Kotus53(stem, gradation)
    case "62" => Kotus62(stem, gradation)
    case "67" => Kotus67(stem, gradation)
    case "73" => Kotus73(stem, gradation)
    case _ => Kotus52(stem, gradation)
  }
}