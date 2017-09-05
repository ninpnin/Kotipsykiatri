package dictionary

import utilities.letterCollection.{takaVokaalit, etuVokaalit}

object grammarHelper {

  def vokaali(vokaali: String, sana: String): String = etuTaiTaka(vokaali, !onkoTaka(sana))

  val astevaihtelut = Map("A" -> ("kk","k"),
      "B" -> ("pp","p"),
      "C" -> ("tt","t"),
      "D" -> ("k",""),
      "E" -> ("p" -> "v"),
      "F" -> ("t","d"),
      "G" -> ("nk","ng"),
      "H" -> ("mp","mm"),
      "I"-> ("lt","ll"),
      "J"-> ("nt","nn"),
      "K"-> ("rt","rr"),
      "L"-> ("k","j"),
      "M"-> ("k","v")
  )
  
  def etuTaiTaka(vokaali: String, etu: Boolean) = {
     var a = -1
     for (i <- 0 until 3) {
       if (takaVokaalit(i) == vokaali || etuVokaalit(i) == vokaali) a = i
     }
     if (a > -1) {
       if (etu) etuVokaalit(a) else takaVokaalit(a)
     } else {
       vokaali
     }
  }

  //Tarkistetaan, onko sanassa yhtään takavokaalia
  //Eli kuuluuko päätteissä olla takavokaali
  def onkoTaka(sana: String): Boolean = {
    val overlap = takaVokaalit
      .flatMap(_.toCharArray)
      .intersect(sana.toCharArray)
    overlap.nonEmpty
  }

  //Muodostetaan sanan heikko vartalo esim peti -> pedi (-n, -stä etc.)
  def heikko(sana: String, mahdolliset: Map[String,String]): String = {
    var sana1 = sana
    val pituus = sana.length
    if (pituus >= 3) {
      var substring = sana.takeRight(2)
      for (i<-mahdolliset.filter(x => x._1.size ==1)) {
        if (substring.contains(i._1)) {
          sana1 = sana.dropRight(2) + substring.replace(i._1,i._2)
        }
      }
      substring = sana.takeRight(3)
      for (i<-mahdolliset.filter(x => x._1.size ==2)) {
        if (substring.contains(i._1)) {
          sana1 = sana.dropRight(3) + substring.replace(i._1,i._2)
        }
      }      
    }
    sana1
  }

  //Muodostetaan sanan vahva vartalo esim hakata -> hakkaa (-n, -t etc.)
  def vahva(sana: String, mahdolliset: Map[String,String]): String = {
    var sana1 = sana
    val pituus = sana.size
    if (pituus >= 3) {
      var substring = sana.takeRight(2)
      for (i<-mahdolliset.filter(x => x._1.size ==1)) {
        if (substring.contains(i._1)) {
          sana1 = sana.dropRight(2) + substring.replace(i._1,i._2)
        }
      }
      substring = sana.takeRight(3)
      for (i<-mahdolliset.filter(x => x._1.size ==2)) {
        if (substring.contains(i._1)) {
          sana1 = sana.dropRight(3) + substring.replace(i._1,i._2)
        }
      }
    }
    sana1
  }

  //Tee sanaan astevaihtelu
  def gradation(kirjain: String, sana: String): String = {
    //Muokkaa sanaa vain jos astevaihtelu on käytössä, ts. koodi != X
    if (kirjain != "X")
      if (astevaihtelut.keys.toVector.contains(sana)) {
        val muunnos = astevaihtelut(kirjain)
        sana.reverse.replaceFirst(muunnos._1.reverse, muunnos._2.reverse).reverse
      } else {
        vahva(sana,astevaihtelut.values.toMap)
      }
    else sana
  }
  
  def inverse(kirjain: String,sana: String): String = {
    if (kirjain == "Y") {
      vahva(sana,astevaihtelut.values.map(x=> (x._2,x._1)).filter(_._1!="").toMap)
    } else if (kirjain == "D") {
      sana + "k"
    } else if (kirjain != "X") {
      val gradation = astevaihtelut(kirjain)
      val inverseG = (gradation._2,gradation._1)
      sana.reverse.replaceFirst(inverseG._1.reverse, inverseG._2.reverse).reverse
    } else sana
  }
  
}