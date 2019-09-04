package tests

import scala.io.Source
import scala.collection.mutable.Buffer
import java.io.PrintWriter

object wordSorter extends App {

  val aakkoset = {
    val buffer = Buffer[Char]()

    for (i <- 97 until 97+24) {
      buffer += i.toChar
    }
    buffer += 'ä'
    buffer += 'ö'
    buffer.toVector
  }
  
  def sort(tiedosto: String) = {
    val soossit = Source.fromFile(tiedosto).getLines.toVector
    
    val verbit = soossit.filter { x => x(x.size-4) == 'V' }
    val nominit = soossit.filter { x => x(x.size-4) == 'A' || x(x.size-4) == 'S' || x(x.size-4) == 'E' }
    val muut = soossit.filter { x => !(x(x.size-4) == 'A' || x(x.size-4) == 'S' || x(x.size-4) == 'E' || x(x.size-4) == 'V') }
    println("verbien koko : "+ verbit.size)
    for (kirjain <- aakkoset) {
      val rimpsu = "sanastot/" + kirjain.toUpper+"/" + kirjain.toUpper
      val verbiP = new PrintWriter(rimpsu + "saannollisetVerbit.txt")
      val NominP = new PrintWriter(rimpsu + "saannollisetNominit.txt")
      val muutP = new PrintWriter(rimpsu + "epasaannolliset.txt")
      try {
        for (riviv <- verbit.filter(x=> x(0).toLower == kirjain)) verbiP.println(riviv)
        for (rivin <- nominit.filter(x=> x(0).toLower == kirjain)) NominP.println(rivin)
        for (rivim <- muut.filter(x=> x(0).toLower == kirjain)) muutP.println(rivim)
      } finally {
        verbiP.close()
        NominP.close()
        muutP.close()
      }
      
    }
    
  }
  
  this.sort("siistitytsanat.txt")
  
  
}