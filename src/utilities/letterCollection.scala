package utilities

/**
  * Created by vaino on 31.8.2017.
  */


object letterCollection {

  val aakkoset: Vector[Char] = ('a' to 'z').toVector ++ Vector('ä','ö')
  val numerot: Set[Char] = ('0' to '9').toSet

  val takaVokaalit = Vector("a","o","u")
  val etuVokaalit = Vector("ä","ö","y")


}
