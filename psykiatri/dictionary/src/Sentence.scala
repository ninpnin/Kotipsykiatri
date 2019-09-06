package dictionary

import scala.collection.mutable.Buffer

class Sentence(val wordList: Buffer[Word]) {

  override def toString(): String = {
    var str = ""
    for (word <- this.wordList) {
      if (str.nonEmpty && (word.teksti != "," && word.teksti != "?")) str += " "
      if (str.isEmpty)
        str += word.teksti.capitalize
      else
        str += word.teksti
    }
    if (str.last!='?') str + "." else str
  }

  def infinitives: Buffer[String] = this.wordList.map(x => x.perusmuoto.getOrElse("None"))
  def sanaluokka(sl: String) = {
    if (sl != "nomini" && sl != "nominit") {
        this.wordList.filter(x => x.sanaluokka.getOrElse("None") == sl)
    } else {
      this.wordList.filter(x => x.sanaluokka == Some("substantiivi") || x.sanaluokka == Some("adjektiivi"))
    }
  }
  def slMuoto(sl: String, muoto: Int) = this.sanaluokka(sl).filter(x => x.taivutusmuoto.getOrElse(609) == muoto)
  def slMuoto(sl: String, muodot: Vector[Int]) = this.sanaluokka(sl).filter(x => muodot.contains(x.taivutusmuoto.getOrElse(609)))

  override def clone: Sentence = {
    val bufferi = this.wordList.map(_.clone)
    new Sentence(bufferi)
  }

  // Kuinka montaa sanaa ei tunnistettu
  def noneCount = wordList.count( x => !x.recognized )
  def length = wordList.length
}