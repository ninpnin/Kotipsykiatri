package dictionary

abstract class Verb(val sana: String, gradation: Char) {

  // 0 I infinitiivi
  // 1-6 aktiivimuodot preesensissä
  // 7 preesensin passiivi
  // 8-13 aktiivimuodot imperfektissä
  // 14 imperfektin passiivi
  // 15-20 aktiivimuodot konditionaalissa
  // 21 konditionaalin passiivi
  // 22 yksikön imperatiivi
  // 23 yksikön mennyt partisiippi
  // 24 monikon mennyt partisiippi
  // 25 tekemä

  def taivuta(muoto: Int): String

}

case class Kotus52(s: String, g: Char) extends Verb(s,g) {  // KOTUS TYPE SANOA
  val mahdolliset = Map("k"-> "" ,"t"-> "d","kk"->"k","pp"->"p","tt"->"t","nk"->"ng","mp"->"mm")

  val heikko = if (g!='X') grammarHelper.gradation(g.toString, s.dropRight(1)) else s.dropRight(1)

  def taivuta(muoto: Int) = {
    muoto match {
      case 1 => heikko + "n"
      case 2 => heikko + "t"
      case 3 => s.dropRight(1) + sana(sana.size-2)
      case 4 => heikko + "mme"
      case 5 => heikko + "tte"
      case 6 => s.dropRight(1) +  "v" + grammarHelper.vokaali("a", sana) + "t"
      case 7 => heikko + "t" +grammarHelper.vokaali("a", sana) * 2 +"n"
      case 8 => heikko + "in"
      case 9 => heikko + "it"
      case 10 => s.dropRight(1) + "i"
      case 11 => heikko + "imme"
      case 12 => heikko + "itte"
      case 13 => s.dropRight(1) +  "iv" + grammarHelper.vokaali("a", sana) + "t"
      case 14 => heikko + "ttiin"
      case 15 => s.dropRight(1) + "isin"
      case 16 => s.dropRight(1) + "isit"
      case 17 => s.dropRight(1) + "isi"
      case 18 => s.dropRight(1) + "isimme"
      case 19 => s.dropRight(1) + "isitte"
      case 20 => s.dropRight(1) + "isiv" + grammarHelper.vokaali("a", sana) + "t"
      case 21 => heikko + "tt" + grammarHelper.vokaali("a", sana) + "isiin"
      case 22 => heikko
      case 23 => s.dropRight(1) +"n" + grammarHelper.vokaali("u", sana)+"t"
      case 24 => s.dropRight(1) +"neet"
      case 25 => s.dropRight(1) + "m" + grammarHelper.vokaali("a", sana)
      case _ => sana
    }
  }
}

case class Kotus53(s: String, g: Char) extends Verb(s,g) {  // KOTUS TYPE MUISTAA
  val mahdolliset = Map("k"-> "" ,"t"-> "d","kk"->"k","pp"->"p","tt"->"t","rt"->"rr")

  val heikko = if (g!='X') grammarHelper.gradation(g.toString(), s.dropRight(1)) else s.dropRight(1)

  def taivuta(muoto: Int) = {
    muoto match {
      case 1 => heikko + "n"
      case 2 => heikko + "t"
      case 3 => sana
      case 4 => heikko + "mme"
      case 5 => heikko + "tte"
      case 6 => sana.dropRight(1) +  "v" + grammarHelper.vokaali("a", sana) + "t"
      case 7 => heikko.dropRight(1) + "et" +grammarHelper.vokaali("a", sana) * 2 +"n"
      case 8 => heikko.dropRight(1) + "in"
      case 9 => heikko.dropRight(1) + "it"
      case 10 => sana.dropRight(2) + "i"
      case 11 => heikko.dropRight(1) + "imme"
      case 12 => heikko.dropRight(1) + "itte"
      case 13 => sana.dropRight(2) +  "iv" + grammarHelper.vokaali("a", sana) + "t"
      case 14 => heikko.dropRight(1) + "ettiin"
      case 15 => sana.dropRight(1) + "isin"
      case 16 => sana.dropRight(1) + "isit"
      case 17 => sana.dropRight(1) + "isi"
      case 18 => sana.dropRight(1) + "isimme"
      case 19 => sana.dropRight(1) + "isitte"
      case 20 => sana.dropRight(1) + "isiv" + grammarHelper.vokaali("a", sana) + "t"
      case 21 => heikko.dropRight(1) + "ett" + grammarHelper.vokaali("a", sana) + "isiin"
      case 22 => heikko
      case 23 => sana.dropRight(1) +"n" + grammarHelper.vokaali("u", sana)+"t"
      case 24 => sana.dropRight(1) +"neet"
      case 25 => sana.dropRight(1) + "m" + grammarHelper.vokaali("a", sana)
      case _ => sana
    }
  }
}

case class Kotus62(s: String, g: Char) extends Verb(s,g) {  // KOTUS TYPE VOIDA

  val stem = s.dropRight(2)

  def taivuta(muoto: Int) = {
    muoto match {
      case 1 => stem + "n"
      case 2 => stem + "t"
      case 3 => stem
      case 4 => stem + "mme"
      case 5 => stem + "tte"
      case 6 => stem +  "v" + s.last + "t"
      case 7 => sana.dropRight(1) + s.last * 2 +"n"
      case 8 => stem + "n"
      case 9 => stem + "t"
      case 10 => stem
      case 11 => stem + "mme"
      case 12 => stem + "tte"
      case 13 => stem + "v" + s.last + "t"
      case 14 => stem + "tiin"
      case 15 => stem + "sin"
      case 16 => stem + "sit"
      case 17 => stem + "si"
      case 18 => stem + "simme"
      case 19 => stem + "sitte"
      case 20 => stem + "siv" + s.last + "t"
      case 21 => stem + "t" + s.last + "isiin"
      case 22 => stem
      case 23 => stem +"n" + grammarHelper.vokaali("u", sana)+"t"
      case 24 => stem +"neet"
      case 25 => stem + "m" + s.last
      case _ => sana
    }
  }

}

case class Kotus67(s: String, g: Char) extends Verb(s,g) {  // KOTUS TYPE TULLA

  val heikko = s.dropRight(2)
  val vahva = if (g=='D') { s.dropRight(4) + "k" + s.substring(s.length-4, s.length-2)
    } else if(g!='X') { grammarHelper.inverse(g.toString,s.dropRight(2))
    } else s.dropRight(2)

  def taivuta(muoto: Int) = {
    muoto match {
      case 1 => vahva + "en"
      case 2 => vahva + "et"
      case 3 => vahva + "ee"
      case 4 => vahva + "emme"
      case 5 => vahva + "ette"
      case 6 => vahva +  "iv" + grammarHelper.vokaali("a", sana) + "t"
      case 7 => sana +sana.last +"n"
      case 8 => vahva + "in"
      case 9 => vahva + "it"
      case 10 => vahva + "i"
      case 11 => vahva + "imme"
      case 12 => vahva + "itte"
      case 13 => vahva +  "iv" + grammarHelper.vokaali("a", sana) + "t"
      case 14 => heikko + "tiin"
      case 15 => vahva + "isin"
      case 16 => vahva + "isit"
      case 17 => vahva + "isi"
      case 18 => vahva + "isimme"
      case 19 => vahva + "isitte"
      case 20 => vahva + "isiv" + grammarHelper.vokaali("a", sana) + "t"
      case 21 => heikko + "t" + s.last + "isiin"
      case 22 => vahva + "e"
      case 23 => heikko +"l" + grammarHelper.vokaali("u", sana)+"t"
      case 24 => heikko +"leet"
      case 25 => vahva + "em" + grammarHelper.vokaali("a", sana)
      case _ => sana
    }
  }
}

case class Kotus73(s: String, g: Char) extends Verb(s,g) {  // KOTUS TYPE SALATA

  val heikko = s.dropRight(2)
  val vahva = if (g=='D') s.dropRight(3) + s.last else if (g!='X') grammarHelper.inverse(g.toString,s.dropRight(2)) else s.dropRight(2)

  def taivuta(muoto: Int) = {
    muoto match {
      case 1 => vahva + s.last + "n"
      case 2 => vahva + s.last + "t"
      case 3 => vahva + s.last
      case 4 => vahva + s.last + "mme"
      case 5 => vahva + s.last + "tte"
      case 6 => vahva + s.last + "v" + s.last + "t"
      case 7 => s + s.last + "n"
      case 8 => vahva + "sin"
      case 9 => vahva + "sit"
      case 10 => vahva + "si"
      case 11 => vahva + "simme"
      case 12 => vahva + "sitte"
      case 13 => vahva + "siv" + s.last + "t"
      case 14 => vahva + "ttiin"
      case 15 => vahva + "isin"
      case 16 => vahva + "isit"
      case 17 => vahva + "isi"
      case 18 => vahva + "isimme"
      case 19 => vahva + "isitte"
      case 20 => vahva + "isiv" + s.last + "t"
      case 21 => heikko + "tt" + s.last + "isiin"
      case 22 => vahva + s.last
      case 23 => heikko +"nn" + grammarHelper.vokaali("u", sana) +"t"
      case 24 => heikko +"nneet"
      case 25 => vahva + s.last +"m" + s.last
      case _ => sana
    }
  }
}

object Verb {
  
}


