package tests

import dictionary.{Kotus1,Kotus5,Kotus6,Kotus10,Kotus9,Kotus12,Kotus38,Kotus39}
import dictionary.dictionaryFinder.getWord

import org.scalatest.FlatSpec

/**
  Unit tests on consonant gradation and nominal declension
 */

class UnitNominals extends FlatSpec {

  "Pata" should "be pada-n" in {
    val a = Kotus1("pata",'F')
    assert("pada" == a.heikko)
  }
  "Auto" should "be auto-n" in {
    val a = Kotus1("auto",'X')
    assert("auto" == a.heikko)
  }
  "Vattu" should "be vatu-n" in {
    val a = Kotus1("vattu",'C')
    assert("vatu" == a.heikko)
  }
  "Risti" should "be risti-n" in {
    val a = Kotus5("risti",'X')
    assert("risti" == a.heikko)
  }
  "Arviointi" should "be arvioinni-n" in {
    val a = Kotus5("arviointi",'J')
    assert("arvioinni" == a.heikko)
  }

  "Auto" should "be autoa" in {
    val a = Kotus1("auto",'X')
    assert("autoa" == a.taivuta(3, false))
  }
  "Arkku" should "be arkkuihin" in {
    val a = Kotus1("arkku",'A')
    assert("arkkuihin" == a.taivuta(6, true))
  }
  "Risti" should "be ristiä" in {
    val a = Kotus1("risti",'X')
    assert("ristiä" == a.taivuta(3, false))
  }
  "Takki" should "be takit" in {
    val a = Kotus1("takki",'A')
    assert("takit" == a.taivuta(0, true))
  }
  "Paperi" should "be papereista" in {
    val a = Kotus6("paperi",'X')
    assert("papereista" == a.taivuta(5, true))
  }
  "Runkkari" should "be runkkariksi" in {
    val a = Kotus6("runkkari",'A')
    assert("runkkariksi" == a.taivuta(11, false))
  }
  "Rauta" should "be raudan" in {
    val a = Kotus9("rauta",'F')
    assert("raudan" == a.taivuta(2, false))
  }

  "Elämä" should "be elämässä" in {
    val a = Kotus10("elämä",'X')
    assert("elämässä" == a.taivuta(4, false))
  }

  "Kulkija" should "be kulkijoita" in {
    val a = Kotus12("kulkija",'X')
    assert("kulkijoita" == a.taivuta(3, true))
  }

  "Keltainen" should "be keltaista" in {
    val a = Kotus38("keltainen",'X')
    assert("keltaista" == a.taivuta(3, false))
  }

  "Kannabis" should "be kannabista" in {
    val a = Kotus39("kannabis",'X')
    assert("kannabista" == a.taivuta(3, false))
  }


  //
  //Sanojen tunnistamisen testaaminen

  "Autossa" should "be auto, 5" in {
    val text = "autossa"
    val word = getWord(text)
    assert("auto" == word.perusmuoto.getOrElse(""))
    assert(4 == word.taivutusmuoto.getOrElse(-1))
  }

  "Kulkijaa" should "be kulkija, 3" in {
    val text = "kulkijaa"
    val word = getWord(text)
    assert("kulkija" == word.perusmuoto.getOrElse(""))
    assert(3 == word.taivutusmuoto.getOrElse(-1))
  }
}