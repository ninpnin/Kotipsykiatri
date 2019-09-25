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
  val a = grammarHelper.vokaali("a", sana)
  val u = grammarHelper.vokaali("u", sana)

  val stems: Set[String]

  def taivuta(muoto: Int): String

}

case class Kotus52(s: String, g: Char) extends Verb(s,g) {  // KOTUS TYPE SANOA
  val mahdolliset = Map("k"-> "" ,"t"-> "d","kk"->"k","pp"->"p","tt"->"t","nk"->"ng","mp"->"mm")

  val heikko = if (g!='X') grammarHelper.gradation(g.toString, s.dropRight(1)) else s.dropRight(1)

  val avautu = s.dropRight(1)
  val avaudu = heikko

  val stems: Set[String] = Set(avautu, avaudu)

  def taivuta(muoto: Int) = {
    muoto match {
      case 1 => avaudu+"n"
      case 2 => avaudu+"t"
      case 3 => avautu+u
      case 4 => avaudu+"mme"
      case 5 => avaudu+"tte"
      case 6 => avautu+"v"+a+"t"
      case 7 => avaudu+"t"+a+a +"n"
      case 8 => avaudu+"in"
      case 9 => avaudu+"it"
      case 10 => avautu+"i"
      case 11 => avaudu+"imme"
      case 12 => avaudu+"itte"
      case 13 => avautu+"iv"+a+"t"
      case 14 => avaudu+"ttiin"
      case 15 => avautu+"isin"
      case 16 => avautu+"isit"
      case 17 => avautu+"isi"
      case 18 => avautu+"isimme"
      case 19 => avautu+"isitte"
      case 20 => avautu+"isiv"+a+"t"
      case 21 => avaudu+"tt"+a+"isiin"
      case 22 => avaudu
      case 23 => avautu+"n"+u+"t"
      case 24 => avautu+"neet"
      case 25 => avautu+"m"+a
      case _ => sana
    }
  }
}

case class Kotus53(s: String, g: Char) extends Verb(s,g) {  // KOTUS TYPE MUISTAA
  val mahdolliset = Map("k"-> "" ,"t"-> "d","kk"->"k","pp"->"p","tt"->"t","rt"->"rr")

  val asettaa = s
  val asetta = s.dropRight(1)
  val asett = s.dropRight(2)
  val aseta = if (g!='X') grammarHelper.gradation(g.toString(), s.dropRight(1)) else s.dropRight(1)
  val aset = aseta.dropRight(1)

  val stems: Set[String] = Set(asett, aset)

  def taivuta(muoto: Int) = {
    muoto match {
      case 1 => aseta+"n"
      case 2 => aseta+"t"
      case 3 => asettaa
      case 4 => aseta+"mme"
      case 5 => aseta+"tte"
      case 6 => asetta+"v"+a+"t"
      case 7 => aset+"et"+a+a +"n"
      case 8 => aset+"in"
      case 9 => aset+"it"
      case 10 => asett+"i"
      case 11 => aset+"imme"
      case 12 => aset+"itte"
      case 13 => asett+"iv"+a+"t"
      case 14 => aset+"ettiin"
      case 15 => asetta+"isin"
      case 16 => asetta+"isit"
      case 17 => asetta+"isi"
      case 18 => asetta+"isimme"
      case 19 => asetta+"isitte"
      case 20 => asetta+"isiv"+a+"t"
      case 21 => aset+"ett"+a+"isiin"
      case 22 => aseta
      case 23 => asetta +"n"+u +"t"
      case 24 => asetta +"neet"
      case 25 => asetta+"m"+a
      case _ => asettaa
    }
  }
}

case class Kotus62(s: String, g: Char) extends Verb(s,g) {  // KOTUS TYPE VOIDA

  val voi = s.dropRight(2)
  val void = s.dropRight(1)

  val stems: Set[String] = Set(voi, void)

  def taivuta(muoto: Int) = {
    muoto match {
      case 1 => voi+"n"
      case 2 => voi+"t"
      case 3 => voi
      case 4 => voi+"mme"
      case 5 => voi+"tte"
      case 6 => voi+"v"+a+"t"
      case 7 => void+a+a +"n"
      case 8 => voi+"n"
      case 9 => voi+"t"
      case 10 => voi
      case 11 => voi+"mme"
      case 12 => voi+"tte"
      case 13 => voi+"v"+a+"t"
      case 14 => voi+"tiin"
      case 15 => voi+"sin"
      case 16 => voi+"sit"
      case 17 => voi+"si"
      case 18 => voi+"simme"
      case 19 => voi+"sitte"
      case 20 => voi+"siv"+a+"t"
      case 21 => voi+"t"+a+"isiin"
      case 22 => voi
      case 23 => voi +"n"+u +"t"
      case 24 => voi +"neet"
      case 25 => voi+"m"+a
      case _ => sana
    }
  }

}

case class Kotus67(s: String, g: Char) extends Verb(s,g) {  // KOTUS TYPE TULLA

  val huidel = s.dropRight(2)

  val huitel = if (g=='D') { s.dropRight(4)+"k"+s.substring(s.length-4, s.length-2)
    } else if(g!='X') { grammarHelper.inverse(g.toString,s.dropRight(2))
    } else s.dropRight(2)

  val stems = Set(huidel, huitel)

  def taivuta(muoto: Int) = {
    muoto match {
      case 1 => huitel+"en"
      case 2 => huitel+"et"
      case 3 => huitel+"ee"
      case 4 => huitel+"emme"
      case 5 => huitel+"ette"
      case 6 => huitel+"iv"+a+"t"
      case 7 => huidel+"l"+a+a+"n"
      case 8 => huitel+"in"
      case 9 => huitel+"it"
      case 10 => huitel+"i"
      case 11 => huitel+"imme"
      case 12 => huitel+"itte"
      case 13 => huitel+"iv"+a+"t"
      case 14 => huidel+"tiin"
      case 15 => huitel+"isin"
      case 16 => huitel+"isit"
      case 17 => huitel+"isi"
      case 18 => huitel+"isimme"
      case 19 => huitel+"isitte"
      case 20 => huitel+"isiv"+a+"t"
      case 21 => huidel+"t"+a+"isiin"
      case 22 => huitel+"e"
      case 23 => huidel +"l"+u+"t"
      case 24 => huidel +"leet"
      case 25 => huitel+"em"+a
      case _ => sana
    }
  }
}

case class Kotus73(s: String, g: Char) extends Verb(s,g) {  // KOTUS TYPE SALATA

  val hangata = s
  val hanga = s.dropRight(2)
  val hanka = if (g=='D') s.dropRight(3)+a else if (g!='X') grammarHelper.inverse(g.toString,s.dropRight(2)) else s.dropRight(2)

  val stems = Set(hanga, hanka)
  
  def taivuta(muoto: Int) = {
    muoto match {
      case 1 => hanka+a+"n"
      case 2 => hanka+a+"t"
      case 3 => hanka+a
      case 4 => hanka+a+"mme"
      case 5 => hanka+a+"tte"
      case 6 => hanka+a+"v"+a+"t"
      case 7 => hangata+a+"n"
      case 8 => hanka+"sin"
      case 9 => hanka+"sit"
      case 10 => hanka+"si"
      case 11 => hanka+"simme"
      case 12 => hanka+"sitte"
      case 13 => hanka+"siv"+a+"t"
      case 14 => hanga+"ttiin"
      case 15 => hanka+"isin"
      case 16 => hanka+"isit"
      case 17 => hanka+"isi"
      case 18 => hanka+"isimme"
      case 19 => hanka+"isitte"
      case 20 => hanka+"isiv"+a+"t"
      case 21 => hanga+"tt"+a+"isiin"
      case 22 => hanka+a
      case 23 => hanga +"nn"+u+"t"
      case 24 => hanga +"nneet"
      case 25 => hanka+a +"m"+a
      case _ => hangata
    }
  }
}

object Verb {
  
}


