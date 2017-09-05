package tests

import scala.collection.mutable.Buffer
import scala.io.Source
import scala.collection.mutable.Map
import dictionary.Kotus1
import dictionary.Nominal
import dictionary.Verb
import dictionary.Kotus3
import dictionary.Kotus38
import dictionary.Kotus48
import dictionary.Kotus5
import dictionary.Kotus40
import dictionary.Kotus12
import dictionary.Kotus6
import dictionary.Kotus2
import dictionary.Kotus10
import dictionary.Kotus39
import dictionary.Kotus9
import dictionary.Kotus53
import dictionary.Kotus52
import dictionary.Kotus73
import dictionary.Kotus67
import java.io.PrintWriter

object hnTunnistaja extends App {

  val aakkoset = {
    val buffer = Buffer[Char]()

    for (i <- 97 until 97+24) {
      buffer += i.toChar
    }
    buffer += 'ä'
    buffer += 'ö'
    buffer.toVector
  }
  val isot = aakkoset.map { x => x.toUpper }
  
  val numerot = Range(48,48+10).toVector.map { x => x.toChar }
  
  def checkFormat(s: String) = {
    /*var a = true
    if (s!= null) {
      if (s.size>4) {
        if (!(numerot.contains(s.last) && numerot.contains( s(s.size-2) )) ) {
          a = false
        } else if (!(isot.contains(s(s.size-3)) && isot.contains( s(s.size-4) ))) {
          a = false
        } else if (!aakkoset.contains(s(0).toLower)) {
          a = false
        }
      } else {
        a = false
      }
    } else {
      a = false
    }
    a*/
    if (s!= null) {
      if (s.size>4) {
        if (!(numerot.contains(s.last) && numerot.contains( s(s.size-2) )) ) {
          false
        } else if (!(isot.contains(s(s.size-3)) && isot.contains( s(s.size-4) ))) {
          false
        } else if (!aakkoset.contains(s(0).toLower)) {
          false
        } else {
          true
        }
      } else {
        false
      }
    } else {
      false
    }
  }
  
  def keraaHn(printti: String) = {
    
    var filteredPm0: Vector[String] = Vector()
    var filteredPm: Vector[String] = Vector()
    var filteredKaikki: Buffer[(String,String)] = Buffer()
    val homonyymit = Buffer[(String,String)]()
    for (kirjain0 <- aakkoset) {
      println("kirjain0 : " + kirjain0)
      val rimpsu = "sanastot/" + kirjain0.toUpper + "/" + kirjain0.toUpper
      filteredPm0 = Source.fromFile(rimpsu + "saannollisetVerbit.txt").getLines().toVector ++ Source.fromFile(rimpsu + "epasaannolliset.txt").getLines().toVector ++ Source.fromFile(rimpsu + "saannollisetNominit.txt").getLines().toVector
      for (kirjain1 <- aakkoset) {
        println("kirjain1 : " + kirjain1)
        filteredPm = filteredPm0.filter { x => x(1) == kirjain1 }
        
        for (rivi <- filteredPm) {
          if (checkFormat(rivi)) {
            val perusmuoto = rivi.dropRight(4)
            val luokka = rivi.takeRight(2).toInt
            val tiedot = rivi.takeRight(4)
            if (Vector(1,2,3,5,6,9,10,12,38,40,48).contains(luokka)) {
              val sanake: Nominal = tiedot.takeRight(2) match {  //
                case "02" => Kotus2(perusmuoto,tiedot(1))
                case "03" => Kotus3(perusmuoto,tiedot(1))
                case "05" => Kotus5(perusmuoto,tiedot(1))
                case "06" => Kotus6(perusmuoto,tiedot(1))
                case "09" => Kotus9(perusmuoto,tiedot(1))
                case "10" => Kotus10(perusmuoto,tiedot(1))
                case "12" => Kotus12(perusmuoto,'X')
                case "38" => Kotus38(perusmuoto,'X')
                case "39" => Kotus39(perusmuoto,'X')
                case "40" => Kotus40(perusmuoto,'X')
                case "48" => Kotus48(perusmuoto,tiedot(1))
                case _ => Kotus1(perusmuoto,tiedot(1))
              }
              for (muoto <- 0 until 11) {
                filteredKaikki += ((perusmuoto, sanake.taivuta(muoto,false)))
              }
              for (muoto <- 0 until 1) {
                filteredKaikki += ((perusmuoto, sanake.taivuta(muoto,true)))
              }
            } else if (Vector(52,53,67,73).contains(luokka)) {
              val tiedot = rivi.takeRight(4)
              val sanago = rivi.dropRight(4)
              val sanake: Verb = tiedot.takeRight(2) match {  //
                case "53" => Kotus53(sanago,tiedot(1))
                case "67" => Kotus67(sanago,tiedot(1))
                case "73" => Kotus73(sanago,tiedot(1))
                case _ => Kotus52(sanago,tiedot(1))
              }
              for (muoto <- 0 until 23) {
                filteredKaikki += ((sanago, sanake.taivuta(muoto)))
              }
            } else {
              filteredKaikki += ((rivi.dropRight(4),rivi.dropRight(4)))
            }
          }
        }
        for (i <- filteredKaikki.filter(x => !filteredKaikki.filter(y => y._1 != x._1 && y._2 == x._2).isEmpty) ) {
            homonyymit += i
        }
          
        filteredKaikki = Buffer[(String,String)]()
        
        
      }
    }
    val printteri = new PrintWriter(printti)
    try {
      for (i <- homonyymit.sortBy(x => x._2)) {
        printteri.println(i._1+ ", " + i._2)
      }
    } finally {
      printteri.close()
    }
  }
  
  def siisti(tiedosto: String) = {
    val a = Source.fromFile(tiedosto).getLines().toVector.distinct
    val p = new PrintWriter(tiedosto)
    try for (i<-a) p.println(i) finally p.close()
  }
  
  def laske(tiedosto: String) = {
    val a = Source.fromFile(tiedosto).getLines().toVector.map(x => x.take(x.indexOf(','))).distinct
    a.size
  }
  
  println(this.laske("homonyymit.txt"))
  
  //this.keraaHn("homonyymit.txt")
  
}