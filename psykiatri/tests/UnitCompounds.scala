package tests

import org.scalatest.FlatSpec
import convo.Word

class UnitCompounds extends FlatSpec {

  "Compound word" should "be recognized" in {
    val a = dictionary.DictionaryFinder.getCompoundByInitial("urheiluauton", Vector("urheilu&autoSX01"))
    assert(a.isDefined)
    assert(Some("urheiluauto") == a.getOrElse(new Word("")).perusmuoto)
    assert(Some(1) == a.getOrElse(new Word("")).taivutusmuoto)
  }

  "Normal word" should "be inflected" in {
    val jalkapallo = new Word("pallo", 5)
    assert(jalkapallo.teksti() == "pallosta")
  }

  "Compound word" should "be inflected" in {
    val jalkapallo = new Word("jalkapallo", 5)
    assert(jalkapallo.teksti() == "jalkapallosta")
  }
}