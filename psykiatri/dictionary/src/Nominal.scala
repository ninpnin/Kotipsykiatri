package dictionary

abstract class Nominal(val sana: String, astevaihtelu: Char) {
    def inflect(form: Int) = this.taivuta(form % 20, form >= 20)
    def taivuta(nominalCase: Int, plural: Boolean): String
    val a = grammarHelper.vokaali("a", sana)
}

case class Kotus1(s: String, g: Char) extends Nominal(s,g) { // KOTUS #1 tyyppi VALO
  
  val etu = s
  val edu = if (g!='X') grammarHelper.gradation(g.toString, s) else sana
  
  def taivuta(nominalCase: Int, plural: Boolean) = {
    if (!plural) {
      nominalCase match {
        case 1 | 2 => edu+"n" // gen. ja akk. ovat samat. 
        case 3 => etu+a
        case 4 => edu+"ss"+a
        case 5 => edu+"st"+a
        case 6 => etu+sana.last+"n"
        case 7 => edu+"ll"+a
        case 8 => edu+"lt"+a
        case 9 => edu+"lle"
        case 10 => etu+"n"+a
        case 11 => edu+"ksi"
        case _ => etu
       }
    } else {
      nominalCase match {
        case 2 => etu+"jen"
        case 3 => etu+"j"+a
        case 4 => edu+"iss"+a
        case 5 => edu+"ist"+a
        case 6 => etu+"ihin"
        case 7 => edu+"ill"+a
        case 8 => edu+"ilt"+a
        case 9 => edu+"ille"
        case 10 => etu+"in"+a
        case 11 => edu+"iksi"
        case _ => edu+"t"
       }
    }
  }
}

case class Kotus2(s: String, g: Char) extends Nominal(s,g) {  //KOTUS TYPE PALVELU

  val palvelu: String = sana

  def taivuta(nominalCase: Int, plural: Boolean): String = if (!plural) {
      nominalCase match {
        case 1 | 2 => palvelu+"n" // gen. ja akk. ovat samat. 
        case 3 => palvelu+a
        case 4 => palvelu+"ss"+a
        case 5 => palvelu+"st"+a
        case 6 => palvelu+sana.last+"n"
        case 7 => palvelu+"ll"+a
        case 8 => palvelu+"lt"+a
        case 9 => palvelu+"lle"
        case 10 => palvelu+"n"+a
        case 11 => palvelu+"ksi"
        case _ => palvelu
       }
    } else {
      nominalCase match {
        case 2 => palvelu+"jen"
        case 3 => palvelu+"j"+a
        case 4 => palvelu+"iss"+a
        case 5 => palvelu+"ist"+a
        case 6 => palvelu+"ihin"
        case 7 => palvelu+"ill"+a
        case 8 => palvelu+"ilt"+a
        case 9 => palvelu+"ille"
        case 10 => palvelu+"in"+a
        case 11 => palvelu+"iksi"
        case _ => palvelu+"t"
       }
    }
}

case class Kotus3(s: String, g: Char) extends Nominal(s,g) { //KOTUS TYPE VALTIO
  val valtio: String = s

  def taivuta(nominalCase: Int, plural: Boolean): String = if (!plural) {
      nominalCase match {
        case 1 | 2 => valtio+"n" // gen. ja akk. ovat samat. 
        case 3 => valtio+a
        case 4 => valtio+"ss"+a
        case 5 => valtio+"st"+a
        case 6 => valtio+sana.last+"n"
        case 7 => valtio+"ll"+a
        case 8 => valtio+"lt"+a
        case 9 => valtio+"lle"
        case 10 => valtio+"n"+a
        case 11 => valtio+"ksi"
        case _ => valtio
       }
    } else {
      nominalCase match {
        case 2 => valtio+"iden"
        case 3 => valtio+"ita"+a
        case 4 => valtio+"iss"+a
        case 5 => valtio+"ist"+a
        case 6 => valtio+"ihin"
        case 7 => valtio+"ill"+a
        case 8 => valtio+"ilt"+a
        case 9 => valtio+"ille"
        case 10 => valtio+"in"+a
        case 11 => valtio+"iksi"
        case _ => valtio+"t"
       }
    }
}

case class Kotus5(s: String, g: Char) extends Nominal(s,g) {
  // KOTUS #5 tyyppi RISTI
  //sana = takki , heikko = taki
  val takk = sana.dropRight(1)
  val takki = if ('i' == sana.last) sana else sana+"i"
  val taki = if (g!='X') grammarHelper.gradation(g.toString, takki) else s
  val tak = taki.dropRight(1)

  def taivuta(nominalCase: Int, plural: Boolean): String = if (!plural) {
      nominalCase match {
        case 1 | 2 => taki+"n" // gen. ja akk. ovat samat. 
        case 3 => takki+a
        case 4 => taki+"ss"+a
        case 5 => taki+"st"+a
        case 6 => takki+sana.last+"n"
        case 7 => taki+"ll"+a
        case 8 => taki+"lt"+a
        case 9 => taki+"lle"
        case 10 => takki+"n"+a
        case 11 => taki+"ksi"
        case _ => takki
       }
    } else {
      nominalCase match {
        case 2 => takki+"ien"
        case 3 => takk+"ej"+a
        case 4 => tak+"eiss"+a
        case 5 => tak+"eist"+a
        case 6 => takk+"eihin"
        case 7 => tak+"eill"+a
        case 8 => tak+"eilt"+a
        case 9 => tak+"eille"
        case 10 => takk+"ein"+a
        case 11 => tak+"eiksi"
        case _ => taki+"t"
       }
    }
}

case class Kotus6(s: String, g: Char) extends Nominal(s,g) {  // KOTUS TYPE PAPERI

  val paperi = if (s.last!='i') s+"i" else s
  val paper = paperi.dropRight(1)

  def taivuta(nominalCase: Int, plural: Boolean): String = {

    if (!plural) {
      nominalCase match {
        case 1 | 2 => paperi+"n"
        case 3 => paperi+a
        case 4 => paperi+"ss"+a
        case 5 => paperi+"st"+a
        case 6 => paperi+"in"
        case 7 => paperi+"ll"+a
        case 8 => paperi+ "lt"+a
        case 9 => paperi+"lle"
        case 10 => paperi+"n"+a
        case 11 => paperi+"ksi"
        case _ => paperi
      }
    } else {
      nominalCase match {
        case 2 => paperi+"en"
        case 3 => paper+"eit"+a
        case 4 => paper+"eiss"+a
        case 5 => paper+"eist"+a
        case 6 => paper+"eihin"
        case 7 => paper+"eill"+a
        case 8 => paper+ "eilt"+a
        case 9 => paper+"eille"
        case 10 => paper+"eihin"+a
        case 11 => paper+"eiksi"
        case _ => paperi+"t"
      }
    }
  }
}

case class Kotus9(s: String, g: Char) extends Nominal(s,g) {

  //ESIMERKKI HIETA
  val hieta = s
  val hieda = if (g!='X') grammarHelper.gradation(g.toString, s) else s

  val hiet = s.dropRight(1)
  val hied = hieda.dropRight(1)

  def taivuta(nominalCase: Int, plural: Boolean): String = if (!plural) {
      nominalCase match {
        case 1 | 2 => hieda+"n" // gen. ja akk. ovat samat. Kï¿½ytï¿½nnï¿½ssï¿½ kaikissa nï¿½in.
        case 3 => hieta+a
        case 4 => hieda+"ss"+a
        case 5 => hieda+"st"+a
        case 6 => hieta+hieta.last+"n"
        case 7 => hieda+"ll"+a
        case 8 => hieda+ "lt"+a
        case 9 => hieda+"ll"+"e"
        case 10 => hieta+"n"+a
        case 11 => hieda+"ksi"
        case _ => hieta
      }
    } else {
      nominalCase match {
        case 2 => hiet+"ojen"
        case 3 => hiet+"oj"+a
        case 4 => hied+"oiss"+a
        case 5 => hied+"oist"+a
        case 6 => hiet+"oihin"
        case 7 => hied+"oill"+a
        case 8 => hied+ "oilt"+a
        case 9 => hied+"oille"
        case 10 => hiet+"oin"+a
        case 11 => hied+"oiksi"
        case _ => hieda+"t"
      }
    }
}

case class Kotus10(s: String, g: Char) extends Nominal(s,g) {

  val huopa = s
  val huova = if (g!='X') grammarHelper.gradation(g.toString, s) else s

  val huop = huopa.dropRight(1)
  val huov = huova.dropRight(1)

  def taivuta(nominalCase: Int, plural: Boolean) = {

    if (!plural) {
      nominalCase match {
        case 1 | 2 => huova+"n" // gen. ja akk. ovat samat. Kï¿½ytï¿½nnï¿½ssï¿½ kaikissa nï¿½in.
        case 3 => huopa+a
        case 4 => huova+"ss"+a
        case 5 => huova+"st"+a
        case 6 => huopa+huopa.last+"n"
        case 7 => huova+"ll"+a
        case 8 => huova+ "lt"+a
        case 9 => huova+"lle"
        case 10 => huopa+"n"+a
        case 11 => huova+"ksi"
        case _ => huopa        // Vï¿½ï¿½rï¿½ indeksi palauttaa nominatiivin
      }
    } else {
      nominalCase match {
        case 2 => huop+"ien"
        case 3 => huop+"i"+a
        case 4 => huov+"iss"+a
        case 5 => huov+"ist"+a
        case 6 => huop+"iin"
        case 7 => huov+"ill"+a
        case 8 => huov+ "ilt"+a
        case 9 => huov+"ille"
        case 10 => huop+"in"+a
        case 11 => huov+"iksi"
        case _ => huova+"t"
      }
    }
  }
}

case class Kotus12(s: String, g: Char) extends Nominal(s,g) {
  
  //ESIMERKKI KULKIJA
  val kulkija = s
  val kulkij = kulkija.dropRight(1)

  def taivuta(nominalCase: Int, plural: Boolean) = {

    if (!plural) {
      nominalCase match {
        case 1 | 2 => kulkija+"n" // gen. ja akk. ovat samat. Kï¿½ytï¿½nnï¿½ssï¿½ kaikissa nï¿½in.
        case 3 => kulkija+a
        case 4 => kulkija+"ss"+a
        case 5 => kulkija+"st"+a
        case 6 => kulkija+a+"n"
        case 7 => kulkija+"ll"+a
        case 8 => kulkija+ "lt"+a
        case 9 => kulkija+"ll"+"e"
        case 10 => kulkija+"n"+a
        case 11 => kulkija+"ksi"
        case _ => kulkija
      }
    } else {
      nominalCase match {
        case 2 => kulkij+"oiden"
        case 3 => kulkij+"oit"+a
        case 4 => kulkij+"oiss"+a
        case 5 => kulkij+"oist"+a
        case 6 => kulkij+"oihin"
        case 7 => kulkij+"oill"+a
        case 8 => kulkij+ "oilt"+a
        case 9 => kulkij+"oille"
        case 10 => kulkij+"oin"+a
        case 11 => kulkij+"oiksi"
        case _ => kulkij+"t"
      }
    }
  }
}

case class Kotus38(s: String, g: Char) extends Nominal(s,g) {

  val nainen = s
  val nais = s.take(s.size-3)+"s"

  def taivuta(nominalCase: Int, plural: Boolean) = {

    if (!plural) {
      nominalCase match {
        case 1 | 2 => nais+"en"
        case 3 => nais+"t"+a
        case 4 => nais+"ess"+a
        case 5 => nais+"est"+a
        case 6 => nais+"iin"
        case 7 => nais+"ell"+a
        case 8 => nais+ "elt"+a
        case 9 => nais+"elle"
        case 10 => nais+"en"+a
        case 11 => nais+"eksi"
        case _ => nainen
      }
    } else {
      nominalCase match {
        case 2 => nais+"ten"
        case 3 => nais+"i"+a
        case 4 => nais+"iss"+a
        case 5 => nais+"ist"+a
        case 6 => nais+"iin"
        case 7 => nais+"ill"+a
        case 8 => nais+ "ilt"+a
        case 9 => nais+"ille"
        case 10 => nais+"in"+a
        case 11 => nais+"iksi"
        case _ => nais+"et"
      }
    }
  }
}

case class Kotus39(s: String, g: Char) extends Nominal(s,g) {  //VASTAUS
  
  val vastaus = s
  val vastauks = s.dropRight(1)+"ks"

  def taivuta(nominalCase: Int, plural: Boolean) = {

    if (!plural) {
      nominalCase match {
        case 1 | 2 => vastauks+"en"
        case 3 => vastaus+"t"+a
        case 4 => vastauks+"ess"+a
        case 5 => vastauks+"est"+a
        case 6 => vastauks+"een"
        case 7 => vastauks+"ell"+a
        case 8 => vastauks+ "elt"+a
        case 9 => vastauks+"elle"
        case 10 => vastauks+"en"+a
        case 11 => vastauks+"eksi"
        case _ => vastaus
      }
    } else {
      nominalCase match {
        case 2 => vastaus+"ten"
        case 3 => vastauks+"i"+a
        case 4 => vastauks+"iss"+a
        case 5 => vastauks+"ist"+a
        case 6 => vastauks+"iin"
        case 7 => vastauks+"ill"+a
        case 8 => vastauks+ "ilt"+a
        case 9 => vastauks+"ille"
        case 10 => vastauks+"in"+a
        case 11 => vastauks+"iksi"
        case _ => vastauks+"et"
      }
    }
  }
}

case class Kotus40(s: String, g: Char) extends Nominal(s,g) {
  val valmius = s
  val valmiu = s.dropRight(1)

  def taivuta(nominalCase: Int, plural: Boolean) = {

    if (!plural) {
      nominalCase match {
        case 1 | 2 => valmiu+"den"
        case 3 => valmiu+"tt"+a
        case 4 => valmiu+"dess"+a
        case 5 => valmiu+"dest"+a
        case 6 => valmiu+"teen"
        case 7 => valmiu+"dell"+a
        case 8 => valmiu+ "delt"+a
        case 9 => valmiu+"delle"
        case 10 => valmiu+"ten"+a
        case 11 => valmiu+"deksi"
        case _ => valmius
      }
    } else {
      nominalCase match {
        case 2 => valmiu+"ksien"
        case 3 => valmiu+"ksi"+a
        case 4 => valmiu+"ksiss"+a
        case 5 => valmiu+"ksist"+a
        case 6 => valmiu+"ksiin"
        case 7 => valmiu+"ksill"+a
        case 8 => valmiu+ "ksilt"+a
        case 9 => valmiu+"ksille"
        case 10 => valmiu+"ksin"+a
        case 11 => valmiu+"ksiksi"
        case _ => valmiu+"det"
      }
    }
  }
}

case class Kotus48(s: String, g: Char) extends Nominal(s,g) {

  val laite = s
  val laitte = if (g == 'D') grammarHelper.inverse("D", s.dropRight(1))+s.last else if (g != 'X') grammarHelper.inverse(g.toString, s) else s

  def taivuta(nominalCase: Int, plural: Boolean): String = if (!plural) {
      nominalCase match {
        case 1 | 2 => laitte+"en" // gen. ja akk. ovat samat. Käytännössä kaikissa näin.
        case 3 => laite+"tt"+a
        case 4 => laitte+"ess"+a
        case 5 => laitte+"est"+a
        case 6 => laitte+"eseen"
        case 7 => laitte+"ell"+a
        case 8 => laitte+"elt"+a
        case 9 => laitte+"elle"
        case 10 => laitte+"en"+a
        case 11 => laitte+"eksi"
        case _ => laite        // Väärä indeksi palauttaa nominatiivin
      }
    } else {
      nominalCase match {
        case 2 => laitte+"iden"
        case 3 => laitte+"it"+a
        case 4 => laitte+"iss"+a
        case 5 => laitte+"ist"+a
        case 6 => laitte+"isiin"
        case 7 => laitte+"ill"+a
        case 8 => laitte+ "ilt"+a
        case 9 => laitte+"ille"
        case 10 => laitte+"in"+a
        case 11 => laitte+"iksi"
        case _ => laitte+"et"
      }
    }
  }
