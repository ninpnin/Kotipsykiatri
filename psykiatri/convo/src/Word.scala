package convo

import dictionary.wordInflector

class Word(text: String) {

  var sanaluokka: Option[String] = None      // None tarkoittaa, ettei muuttujan arvoa
  var perusmuoto: Option[String] = None      // voi vielä päätellä. Esim sanaluokkaa ei
  var taivutusmuoto: Option[Int] = None      // tiedetä.
  var suffix: Option[String] = None

  if (text == "?") perusmuoto = Some("Kysymysmerkki") else if (text==",") perusmuoto = Some("Pilkku")

  def teksti: String = if (suffix.isEmpty) text else text + suffix.get
  def recognized: Boolean = this.perusmuoto.isDefined

  override def toString() = "T : " + teksti + ",S " + sanaluokka + ", " + perusmuoto + ", " + taivutusmuoto

  def this(s: String, muoto: Int) = {
    this(wordInflector.inflect(s, muoto))
    this.perusmuoto = Some(s)
    this.taivutusmuoto = Some(muoto)
  }

  override def clone: Word = {
    val clonedWord = new Word(this.text)
    clonedWord.perusmuoto = this.perusmuoto
    clonedWord.taivutusmuoto = this.taivutusmuoto
    clonedWord.sanaluokka = this.sanaluokka
    clonedWord.suffix = this.suffix
    clonedWord
  }
}
