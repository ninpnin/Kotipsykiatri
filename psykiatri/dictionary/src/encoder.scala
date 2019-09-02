package dictionary

import scala.collection.mutable.Buffer

object encoder {  // koodaa tallennusdataa , joka koostuu
  // neljän merkin lohkoista kuten 13xx, 735x tai xxxx.
  // numerot ja kirjaimet kuvastavat taivutusmuotoa. Useampi
  // merkki vastaa useampaa muotoa, x-merkit ovat tyhjiä.
  
  // lause kuvataan formaatissa seuraaavasti:
  // 1xxx3xxx{ostaa,leipä} = ostan leipää
  // ensin siis sanojen verran muotoja vastaavia blokkeja,
  // minkä jälkeen tulevat muotoja vastaavat sanat hakasulkeissa
  
  // blokki xxxx tarkoittaa mitä tahansa muotoa

  private val base33 = (('0' to '9') ++ ('a' to 'x')).toVector

  def encode(muodot: Vector[Int]): String = {
    val muodot1 = muodot.take(4).sortBy { x => x }
    var encodedStr = ""
    for (index <- 0 until muodot1.length) {
      val luku = 31 & muodot1(index)
      encodedStr += base33(luku)
    }
    for (i <- 0 until (4 - encodedStr.length))
      encodedStr += 'x'
    //Palauta valmis rimpsu
    encodedStr
  }

  def decode(s: String): Vector[Int] = {

    val lukuja = s.count { x => x!='x' }
    val b = Array.ofDim[Int](lukuja)
    for (i<- 0 until  lukuja) {
      val indeksi = base33.indexOf(s.replaceAll("x", "")(i))
      b(i) = indeksi
    }

    b.toVector.sortBy { x => x }
  }

  def simpleDecode(s: String): Int = if (s(0) == 'x') 0 else base33.indexOf(s(0))
  
  
}