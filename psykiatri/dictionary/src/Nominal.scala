package dictionary

abstract class Nominal(val sana: String, astevaihtelu: Char) {
    def taivuta(sijamuoto: Int, plural: Boolean): String
}

case class Kotus1(s: String, g: Char) extends Nominal(s,g) { // KOTUS #1 tyyppi VALO

  val weakStem = if (g!='X') grammarHelper.gradation(g.toString, s) else sana
  val a = grammarHelper.vokaali("a", sana)

  def taivuta(sijamuoto: Int, plural: Boolean) = {

    if (!plural) {
      val stem = sijamuoto match {
        case 1 | 2 | 4 | 5 | 7 | 8 | 9 | 11 => weakStem
        case _ => sana
      }
      val endings = Vector("", "n", "n", a, "ss"+a, "st"+a, s.last+"n", "ll"+a, "lt"+a, "lle", "n"+a, "ksi")
      stem + endings(sijamuoto)
    } else {
      val stem = sijamuoto match {
        case 0 | 4 | 5 | 7 | 8 | 9 | 11 => weakStem
        case _ => sana
      }
      val endings = Vector("t", "t", "jen", "j"+a, "iss"+a, "ist"+a, "ihin", "ill"+a, "ilt"+a, "ille", "in"+a, "iksi")
      stem + endings(sijamuoto)
    }
  }
}

case class Kotus2(s: String, g: Char) extends Nominal(s,g) {  //KOTUS TYPE PALVELU

  val vahva: String = sana
  val heikko: String = sana

  def taivuta(sijamuoto: Int, plural: Boolean): String = if (!plural) {
      sijamuoto match {
        case 1 | 2 => heikko + "n" // gen. ja akk. ovat samat. Kï¿½ytï¿½nnï¿½ssï¿½ kaikissa nï¿½in.
        case 3 => sana + grammarHelper.vokaali("a", sana)
        case 4 => heikko + "ss" + grammarHelper.vokaali("a", sana)
        case 5 => heikko + "st" + grammarHelper.vokaali("a", sana)
        case 6 => sana + sana.last + "n"
        case 7 => heikko + "ll" + grammarHelper.vokaali("a", sana)
        case 8 => heikko +  "lt" + grammarHelper.vokaali("a", sana)
        case 9 => heikko + "ll" + "e"
        case 10 => sana + "n" + grammarHelper.vokaali("a", sana)
        case 11 => heikko + "ksi"
        case _ => sana        // Vï¿½ï¿½rï¿½ indeksi palauttaa nominatiivin
      }
    } else {
      sijamuoto match {
        case 2 => sana + "jen"
        case 3 => sana + "j"+ grammarHelper.vokaali("a", sana)
        case 4 => heikko + "iss" + grammarHelper.vokaali("a", sana)
        case 5 => heikko + "ist" + grammarHelper.vokaali("a", sana)
        case 6 => sana + "ihin"
        case 7 => heikko + "ill" + grammarHelper.vokaali("a", sana)
        case 8 => heikko +  "ilt" + grammarHelper.vokaali("a", sana)
        case 9 => heikko + "ill" + "e"
        case 10 => sana + "in" + grammarHelper.vokaali("a", sana)
        case 11 => heikko + "iksi"
        case _ => heikko + "t"
      }
    }
}

case class Kotus3(s: String, g: Char) extends Nominal(s,g) { //KOTUS TYPE VALTIO
  val vahva: String = s
  val heikko: String = s

  def taivuta(sijamuoto: Int, plural: Boolean): String = if (!plural) {
      sijamuoto match {
        case 1 | 2 => heikko + "n" // gen. ja akk. ovat samat. Kï¿½ytï¿½nnï¿½ssï¿½ kaikissa nï¿½in.
        case 3 => sana + "t" + grammarHelper.vokaali("a", sana)
        case 4 => heikko + "ss" + grammarHelper.vokaali("a", sana)
        case 5 => heikko + "st" + grammarHelper.vokaali("a", sana)
        case 6 => sana + sana.last + "n"
        case 7 => heikko + "ll" + grammarHelper.vokaali("a", sana)
        case 8 => heikko +  "lt" + grammarHelper.vokaali("a", sana)
        case 9 => heikko + "ll" + "e"
        case 10 => sana + "n" + grammarHelper.vokaali("a", sana)
        case 11 => heikko + "ksi"
        case _ => sana        // Vï¿½ï¿½rï¿½ indeksi palauttaa nominatiivin
      }
    } else {
      sijamuoto match {
        case 2 => sana + "iden"
        case 3 => sana + "it"+ grammarHelper.vokaali("a", sana)
        case 4 => heikko + "iss" + grammarHelper.vokaali("a", sana)
        case 5 => heikko + "ist" + grammarHelper.vokaali("a", sana)
        case 6 => sana + "ihin"
        case 7 => heikko + "ill" + grammarHelper.vokaali("a", sana)
        case 8 => heikko +  "ilt" + grammarHelper.vokaali("a", sana)
        case 9 => heikko + "ill" + "e"
        case 10 => sana + "in" + grammarHelper.vokaali("a", sana)
        case 11 => heikko + "iksi"
        case _ => heikko + "t"
      }
    }
}

case class Kotus5(s: String, g: Char) extends Nominal(s,g) {
  // KOTUS #5 tyyppi RISTI
  //sana = takki , heikko = taki
  val vahva = if ('i' == sana.last) sana else sana + "i"

  val heikko = if (g!='X') grammarHelper.gradation(g.toString, vahva) else s

  def taivuta(sijamuoto: Int, plural: Boolean) = {

    if (!plural) {
      sijamuoto match {
        case 1 | 2 => heikko + "n"
        case 3 => vahva + grammarHelper.vokaali("a", sana)
        case 4 => heikko + "ss" + grammarHelper.vokaali("a", sana)
        case 5 => heikko + "st" + grammarHelper.vokaali("a", sana)
        case 6 => vahva + "in"
        case 7 => heikko + "ll" + grammarHelper.vokaali("a", sana)
        case 8 => heikko +  "lt" + grammarHelper.vokaali("a", sana)
        case 9 => heikko + "ll" + "e"
        case 10 => vahva + "n" + grammarHelper.vokaali("a", sana)
        case 11 => heikko + "ksi"
        case _ => s
      }
    } else {
      sijamuoto match {
        case 2 => vahva + "en"
        case 3 => vahva.dropRight(1) + "ej"+ grammarHelper.vokaali("a", sana)
        case 4 => heikko.dropRight(1) + "eiss" + grammarHelper.vokaali("a", sana)
        case 5 => heikko.dropRight(1) + "eist" + grammarHelper.vokaali("a", sana)
        case 6 => sana.dropRight(1) + "eihin"
        case 7 => heikko.dropRight(1) + "ll" + grammarHelper.vokaali("a", sana)
        case 8 => heikko.dropRight(1) +  "lt" + grammarHelper.vokaali("a", sana)
        case 9 => heikko.dropRight(1) + "eill" + "e"
        case 10 => vahva.dropRight(1) + "in" + grammarHelper.vokaali("a", sana)
        case 11 => heikko + "eiksi"
        case _ => heikko + "t"
      }
    }
  }
}

case class Kotus6(s: String, g: Char) extends Nominal(s,g) {  // KOTUS TYPE PAPERI

  val vahva = if (s.last!='i') s + "i" else s

  def taivuta(sijamuoto: Int, plural: Boolean): String = {

    if (!plural) {
      sijamuoto match {
        case 1 | 2 => vahva + "n"
        case 3 => vahva + grammarHelper.vokaali("a", sana)
        case 4 => vahva + "ss" + grammarHelper.vokaali("a", sana)
        case 5 => vahva + "st" + grammarHelper.vokaali("a", sana)
        case 6 => vahva + "in"
        case 7 => vahva + "ll" + grammarHelper.vokaali("a", sana)
        case 8 => vahva +  "lt" + grammarHelper.vokaali("a", sana)
        case 9 => vahva + "ll" + "e"
        case 10 => vahva + "n" + grammarHelper.vokaali("a", sana)
        case 11 => vahva + "ksi"
        case _ => s
      }
    } else {
      sijamuoto match {
        case 2 => vahva + "en"
        case 3 => vahva.dropRight(1) + "eit"+ grammarHelper.vokaali("a", sana)
        case 4 => vahva.dropRight(1) + "eiss" + grammarHelper.vokaali("a", sana)
        case 5 => vahva.dropRight(1) + "eist" + grammarHelper.vokaali("a", sana)
        case 6 => sana.dropRight(1) + "eihin"
        case 7 => vahva.dropRight(1) + "eill" + grammarHelper.vokaali("a", sana)
        case 8 => vahva.dropRight(1) +  "eilt" + grammarHelper.vokaali("a", sana)
        case 9 => vahva.dropRight(1) + "eill" + "e"
        case 10 => vahva.dropRight(1) + "eihin" + grammarHelper.vokaali("a", sana)
        case 11 => vahva.dropRight(1) + "eiksi"
        case _ => vahva + "t"
      }
    }
  }
}

case class Kotus9(s: String, g: Char) extends Nominal(s,g) {
  val vahva = s
  val heikko = if (g!='X') grammarHelper.gradation(g.toString, s) else s

  def taivuta(sijamuoto: Int, plural: Boolean): String = if (!plural) {
      sijamuoto match {
        case 1 | 2 => heikko + "n" // gen. ja akk. ovat samat. Kï¿½ytï¿½nnï¿½ssï¿½ kaikissa nï¿½in.
        case 3 => sana + grammarHelper.vokaali("a", sana)
        case 4 => heikko + "ss" + grammarHelper.vokaali("a", sana)
        case 5 => heikko + "st" + grammarHelper.vokaali("a", sana)
        case 6 => sana + sana.last + "n"
        case 7 => heikko + "ll" + grammarHelper.vokaali("a", sana)
        case 8 => heikko +  "lt" + grammarHelper.vokaali("a", sana)
        case 9 => heikko + "ll" + "e"
        case 10 => sana + "n" + grammarHelper.vokaali("a", sana)
        case 11 => heikko + "ksi"
        case _ => sana        // Vï¿½ï¿½rï¿½ indeksi palauttaa nominatiivin
      }
    } else {
      sijamuoto match {
        case 2 => sana.dropRight(1) + "ojen"
        case 3 => sana.dropRight(1) + "oj"+ grammarHelper.vokaali("a", sana)
        case 4 => heikko.dropRight(1) + "oiss" + grammarHelper.vokaali("a", sana)
        case 5 => heikko.dropRight(1) + "oist" + grammarHelper.vokaali("a", sana)
        case 6 => sana.dropRight(1) + "oihin"
        case 7 => heikko.dropRight(1) + "oill" + grammarHelper.vokaali("a", sana)
        case 8 => heikko.dropRight(1) +  "oilt" + grammarHelper.vokaali("a", sana)
        case 9 => heikko.dropRight(1) + "oill" + "e"
        case 10 => sana.dropRight(1) + "oin" + grammarHelper.vokaali("a", sana)
        case 11 => heikko.dropRight(1) + "oiksi"
        case _ => heikko + "t"
      }
    }
}

case class Kotus10(s: String, g: Char) extends Nominal(s,g) {
  val heikko = if (g!='X') grammarHelper.gradation(g.toString, s) else s

  def taivuta(sijamuoto: Int, plural: Boolean) = {

    if (!plural) {
      sijamuoto match {
        case 1 | 2 => heikko + "n" // gen. ja akk. ovat samat. Kï¿½ytï¿½nnï¿½ssï¿½ kaikissa nï¿½in.
        case 3 => sana + grammarHelper.vokaali("a", sana)
        case 4 => heikko + "ss" + grammarHelper.vokaali("a", sana)
        case 5 => heikko + "st" + grammarHelper.vokaali("a", sana)
        case 6 => sana + sana.last + "n"
        case 7 => heikko + "ll" + grammarHelper.vokaali("a", sana)
        case 8 => heikko +  "lt" + grammarHelper.vokaali("a", sana)
        case 9 => heikko + "ll" + "e"
        case 10 => sana + "n" + grammarHelper.vokaali("a", sana)
        case 11 => heikko + "ksi"
        case _ => sana        // Vï¿½ï¿½rï¿½ indeksi palauttaa nominatiivin
      }
    } else {
      sijamuoto match {
        case 2 => sana.dropRight(1) + "ien"
        case 3 => sana.dropRight(1) + "i"+ grammarHelper.vokaali("a", sana)
        case 4 => heikko.dropRight(1) + "iss" + grammarHelper.vokaali("a", sana)
        case 5 => heikko.dropRight(1) + "ist" + grammarHelper.vokaali("a", sana)
        case 6 => sana.dropRight(1) + "iin"
        case 7 => heikko.dropRight(1) + "ill" + grammarHelper.vokaali("a", sana)
        case 8 => heikko.dropRight(1) +  "ilt" + grammarHelper.vokaali("a", sana)
        case 9 => heikko.dropRight(1) + "ill" + "e"
        case 10 => sana.dropRight(1) + "in" + grammarHelper.vokaali("a", sana)
        case 11 => heikko.dropRight(1) + "iksi"
        case _ => heikko + "t"
      }
    }
  }
}

case class Kotus12(s: String, g: Char) extends Nominal(s,g) {
  val vahva = s
  val heikko = s

  def taivuta(sijamuoto: Int, plural: Boolean) = {

    if (!plural) {
      sijamuoto match {
        case 1 | 2 => heikko + "n" // gen. ja akk. ovat samat. Kï¿½ytï¿½nnï¿½ssï¿½ kaikissa nï¿½in.
        case 3 => sana + grammarHelper.vokaali("a", sana)
        case 4 => heikko + "ss" + grammarHelper.vokaali("a", sana)
        case 5 => heikko + "st" + grammarHelper.vokaali("a", sana)
        case 6 => sana + sana.last + "n"
        case 7 => heikko + "ll" + grammarHelper.vokaali("a", sana)
        case 8 => heikko +  "lt" + grammarHelper.vokaali("a", sana)
        case 9 => heikko + "ll" + "e"
        case 10 => sana + "n" + grammarHelper.vokaali("a", sana)
        case 11 => heikko + "ksi"
        case _ => sana        // Vï¿½ï¿½rï¿½ indeksi palauttaa nominatiivin
      }
    } else {
      sijamuoto match {
        case 2 => sana.dropRight(1) + "oiden"
        case 3 => sana.dropRight(1) + "oit"+ grammarHelper.vokaali("a", sana)
        case 4 => heikko.dropRight(1) + "oiss" + grammarHelper.vokaali("a", sana)
        case 5 => heikko.dropRight(1) + "oist" + grammarHelper.vokaali("a", sana)
        case 6 => sana.dropRight(1) + "oihin"
        case 7 => heikko.dropRight(1) + "oill" + grammarHelper.vokaali("a", sana)
        case 8 => heikko.dropRight(1) +  "oilt" + grammarHelper.vokaali("a", sana)
        case 9 => heikko.dropRight(1) + "oill" + "e"
        case 10 => sana.dropRight(1) + "oin" + grammarHelper.vokaali("a", sana)
        case 11 => heikko.dropRight(1) + "oiksi"
        case _ => heikko + "t"
      }
    }
  }
}

case class Kotus38(s: String, g: Char) extends Nominal(s,g) {

  val muuttunut = s.take(s.size-3)+"s"

  def taivuta(sijamuoto: Int, plural: Boolean) = {

    if (!plural) {
      sijamuoto match {
        case 1 | 2 => muuttunut + "en"
        case 3 => muuttunut + "t"+ grammarHelper.vokaali("a", sana)
        case 4 => muuttunut + "ess" + grammarHelper.vokaali("a", sana)
        case 5 => muuttunut + "est" + grammarHelper.vokaali("a", sana)
        case 6 => muuttunut + "iin"
        case 7 => muuttunut + "ell" + grammarHelper.vokaali("a", sana)
        case 8 => muuttunut +  "elt" + grammarHelper.vokaali("a", sana)
        case 9 => muuttunut + "elle"
        case 10 => muuttunut + "en" + grammarHelper.vokaali("a", sana)
        case 11 => muuttunut + "eksi"
        case _ => s
      }
    } else {
      sijamuoto match {
        case 2 => muuttunut + "ten"
        case 3 => muuttunut + "i"+ grammarHelper.vokaali("a", sana)
        case 4 => muuttunut + "iss" + grammarHelper.vokaali("a", sana)
        case 5 => muuttunut + "ist" + grammarHelper.vokaali("a", sana)
        case 6 => muuttunut + "iin"
        case 7 => muuttunut + "ill" + grammarHelper.vokaali("a", sana)
        case 8 => muuttunut +  "ilt" + grammarHelper.vokaali("a", sana)
        case 9 => muuttunut + "ille"
        case 10 => muuttunut + "in" + grammarHelper.vokaali("a", sana)
        case 11 => muuttunut + "iksi"
        case _ => muuttunut + "et"
      }
    }
  }
}

case class Kotus39(s: String, g: Char) extends Nominal(s,g) {  //VASTAUS
  val muuttunut = s.dropRight(1)+"ks"

  def taivuta(sijamuoto: Int, plural: Boolean) = {

    if (!plural) {
      sijamuoto match {
        case 1 | 2 => muuttunut + "en"
        case 3 => s + "t"+ grammarHelper.vokaali("a", sana)
        case 4 => muuttunut + "ess" + grammarHelper.vokaali("a", sana)
        case 5 => muuttunut + "est" + grammarHelper.vokaali("a", sana)
        case 6 => muuttunut + "een"
        case 7 => muuttunut + "ell" + grammarHelper.vokaali("a", sana)
        case 8 => muuttunut +  "elt" + grammarHelper.vokaali("a", sana)
        case 9 => muuttunut + "elle"
        case 10 => muuttunut + "en" + grammarHelper.vokaali("a", sana)
        case 11 => muuttunut + "eksi"
        case _ => s
      }
    } else {
      sijamuoto match {
        case 2 => s + "ten"
        case 3 => muuttunut + "i"+ grammarHelper.vokaali("a", sana)
        case 4 => muuttunut + "iss" + grammarHelper.vokaali("a", sana)
        case 5 => muuttunut + "ist" + grammarHelper.vokaali("a", sana)
        case 6 => muuttunut + "iin"
        case 7 => muuttunut + "ill" + grammarHelper.vokaali("a", sana)
        case 8 => muuttunut +  "ilt" + grammarHelper.vokaali("a", sana)
        case 9 => muuttunut + "ille"
        case 10 => muuttunut + "in" + grammarHelper.vokaali("a", sana)
        case 11 => muuttunut + "iksi"
        case _ => muuttunut + "et"
      }
    }
  }
}

case class Kotus40(s: String, g: Char) extends Nominal(s,g) {
  val drop = s.dropRight(1)

  def taivuta(sijamuoto: Int, plural: Boolean) = {

    if (!plural) {
      sijamuoto match {
        case 1 | 2 => drop + "den"
        case 3 => drop + "tt"+ grammarHelper.vokaali("a", sana)
        case 4 => drop + "dess" + grammarHelper.vokaali("a", sana)
        case 5 => drop + "dest" + grammarHelper.vokaali("a", sana)
        case 6 => drop + "teen"
        case 7 => drop + "dell" + grammarHelper.vokaali("a", sana)
        case 8 => drop +  "delt" + grammarHelper.vokaali("a", sana)
        case 9 => drop + "delle"
        case 10 => drop + "ten" + grammarHelper.vokaali("a", sana)
        case 11 => drop + "deksi"
        case _ => s
      }
    } else {
      sijamuoto match {
        case 2 => drop + "ksien"
        case 3 => drop + "ksi"+ grammarHelper.vokaali("a", sana)
        case 4 => drop + "ksiss" + grammarHelper.vokaali("a", sana)
        case 5 => drop + "ksist" + grammarHelper.vokaali("a", sana)
        case 6 => drop + "ksiin"
        case 7 => drop + "ksill" + grammarHelper.vokaali("a", sana)
        case 8 => drop +  "ksilt" + grammarHelper.vokaali("a", sana)
        case 9 => drop + "ksille"
        case 10 => drop + "ksin" + grammarHelper.vokaali("a", sana)
        case 11 => drop + "ksiksi"
        case _ => drop + "det"
      }
    }
  }
}

case class Kotus48(s: String, g: Char) extends Nominal(s,g) {

  val vahva = s
  val heikko = if (g == 'D') grammarHelper.inverse("D", s.dropRight(1)) + s.last else if (g != 'X') grammarHelper.inverse(g.toString, s) else s

  def taivuta(sijamuoto: Int, plural: Boolean): String = if (!plural) {
      sijamuoto match {
        case 1 | 2 => heikko + "en" // gen. ja akk. ovat samat. Kï¿½ytï¿½nnï¿½ssï¿½ kaikissa nï¿½in.
        case 3 => sana +"tt"+ grammarHelper.vokaali("a", sana)
        case 4 => heikko + "ess" + grammarHelper.vokaali("a", sana)
        case 5 => heikko + "est" + grammarHelper.vokaali("a", sana)
        case 6 => heikko + "eseen"
        case 7 => heikko + "ell" + grammarHelper.vokaali("a", sana)
        case 8 => heikko + "elt" + grammarHelper.vokaali("a", sana)
        case 9 => heikko + "elle"
        case 10 => heikko + "en" + grammarHelper.vokaali("a", sana)
        case 11 => heikko + "eksi"
        case _ => sana        // Vï¿½ï¿½rï¿½ indeksi palauttaa nominatiivin
      }
    } else {
      sijamuoto match {
        case 2 => heikko + "iden"
        case 3 => heikko + "it"+ grammarHelper.vokaali("a", sana)
        case 4 => heikko + "iss" + grammarHelper.vokaali("a", sana)
        case 5 => heikko + "ist" + grammarHelper.vokaali("a", sana)
        case 6 => heikko + "isiin"
        case 7 => heikko + "ill" + grammarHelper.vokaali("a", sana)
        case 8 => heikko +  "ilt" + grammarHelper.vokaali("a", sana)
        case 9 => heikko + "ille"
        case 10 => heikko + "in" + grammarHelper.vokaali("a", sana)
        case 11 => heikko + "iksi"
        case _ => heikko + "et"
      }
    }
  }
