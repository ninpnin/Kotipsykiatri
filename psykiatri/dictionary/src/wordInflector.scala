package dictionary

import utilities.letterCollection.aakkoset
import utilities.Config.vocabPath

import scala.io.Source

object wordInflector {

  //Ylätason funktio sanan perusmuodon sana- ja taivutusluokan hakemiseen. Nelimerkkinen koodi.
  def partOfSpeech(t: String): Option[String] = {
    val initial = t.head.toUpper
    var palautus: Option[String] = None
    var index = 0
    val rimpsu = vocabPath + "sanastot/" + initial + "/" + initial   // jokaisen käytettävän tiedoston alku
    if (aakkoset.contains(initial.toLower)) {
      val filenames = Vector(rimpsu + "saannollisetVerbit.txt", rimpsu + "saannollisetNominit.txt",
          rimpsu+"epasaannolliset.txt",
          vocabPath + "sanastot/erisnimet.txt")
      while (palautus.isEmpty && index < 4) {
        palautus = this.posByInitial(t, filenames(index))
        index += 1
      }
      if (palautus.isEmpty)
        palautus = posCompound(t, vocabPath + "sanastot/" + initial + "/yhdyssanat.txt")
    }
    palautus
  }

  //Apufunktio sanaluokan hakemiseen. Tarkistaa löytyykö tiedostosta samaa perusmuotoa ja palauttaa sanaluokan.
  private def posByInitial(t: String,tiedosto: String) = {
    var palautus: Option[String] = None
    var index = 0
    val wordList = Source.fromFile(tiedosto).getLines().toVector
    val wordCount = wordList.length
    while (palautus.isEmpty && index < wordCount) {
      val infinitive = wordList(index).dropRight(4)
      if (infinitive == t) palautus = Some(wordList(index).takeRight(4))
      index += 1
    }
    palautus
  }

  private def posCompound(t: String,tiedosto: String) = {
    var palautus: Option[String] = None
    var index = 0
    val wordList = Source.fromFile(tiedosto).getLines().toVector
    val wordCount = wordList.length
    while (palautus.isEmpty && index < wordCount) {
      val infinitive = wordList(index).dropRight(4).filter(_!='&')
      if (infinitive == t) palautus = Some(wordList(index).split('&').last)
      index += 1
    }
    if (palautus.isDefined) {
      val code = palautus.get.takeRight(4)
      println(palautus.get.dropRight(4))
      partOfSpeech(palautus.get.dropRight(4))
      Some(code)
    } else None
  }

  def inflect(s: String, muoto: Int): String = {
    if (punctuation(s).isEmpty) {
      var yleinen = commonVerb(s, muoto)
      if (yleinen.isEmpty) yleinen = commonPronoun(s, muoto)
      if (yleinen.isEmpty) {
        val a = partOfSpeech(s)
        if (a.isDefined) inflect(s, muoto, a.get) else s
      } else yleinen.get
    } else punctuation(s).get
  }
  
  val merkit = Map("Pilkku"->",","Kysymysmerkki" -> "?")
  private def punctuation(text: String): Option[String] =
    if (merkit.keys.toVector.contains(text)) Some(merkit(text)) else None
  
  private def inflect(s: String, muoto: Int, tiedot: String): String = {
    val sanaluokka = tiedot.takeRight(2).toInt
    val g = tiedot(1)
    if (sanaluokka == 0 || sanaluokka >= 99)
      s //Ei taivutusta
    else if (sanaluokka>=1 && sanaluokka <= 51)  // nominal
      sanaluokka match {
        case 1 => Kotus1(s,g).inflect(muoto)
        case 2 => Kotus2(s,g).inflect(muoto)
        case 3 => Kotus3(s,g).inflect(muoto)
        case 5 => Kotus5(s,g).inflect(muoto)
        case 6 => Kotus6(s,g).inflect(muoto)
        case 9 => Kotus9(s,g).inflect(muoto)
        case 10 | 11 => Kotus10(s,g).inflect(muoto)
        case 12 => Kotus12(s,g).inflect(muoto)
        case 38 => Kotus38(s,g).inflect(muoto)
        case 39 | 40 => Kotus39(s,g).inflect(muoto)
        case 48 => Kotus48(s,g).inflect(muoto)
        case _ => Kotus1(s,g).inflect(muoto)
      }
    else                                   // verb
      sanaluokka match {
        case 52 => Kotus52(s,g).taivuta(muoto)
        case 53 => Kotus53(s,g).taivuta(muoto)
        case 62 => Kotus62(s,g).taivuta(muoto)
        case 67 => Kotus67(s,g).taivuta(muoto)
        case 73 => Kotus73(s,g).taivuta(muoto)
        case _ => Kotus52(s,g).taivuta(muoto)
      }
  }
  
  private def commonVerb(s: String, inflection: Int): Option[String] = {
    var a: Option[String] = None
    val b = Source.fromFile(vocabPath + "sanastot/yleiset/verbit.txt").getLines().toVector
    for (rivi <- b) {
      val split = rivi.split(",")
      if (split.head == s && inflection < split.length )
        a = Some(split(inflection))
    }
    a
  }
  
  private def commonPronoun(s: String,muoto: Int): Option[String] = {
    var a: Option[String] = None
    val b = Source.fromFile(vocabPath + "sanastot/yleiset/pronominit.txt").getLines().toVector
    for (rivi <- b) {
      val split = rivi.split(",")
      if (split.head == s && muoto < split.length )
        a = Some(split(muoto))
    }
    a
  }
}