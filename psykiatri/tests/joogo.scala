package tests

import java.io.PrintWriter
import scala.io.Source

object joogo extends App {

  
    val soossit = Source.fromFile("kotus5.txt").getLines().filter( x => x!=null && x != "").toVector
  
    val range = Range('a','z').toVector.map( x => x.toChar ) :+ 'ä' :+ 'ö'
    for (i <- range) {
      val kirjain = i.toString().toUpperCase()
      println(kirjain) 
      //val a = new PrintWriter("sanastot/" + kirjain+ "/" + kirjain + "saannollisetVerbit.txt")
      val b = new PrintWriter("sanastot/" + kirjain + "/" + kirjain + "saannollisetNominit.txt")
      //val c = new PrintWriter("sanastot/" + kirjain+ "/" + kirjain + "epasaannolliset.txt")
      try {
        //a.println("testi")
        for (i <- soossit.filter { x => x(0) == i.toString.toUpperCase()(0) || x(0) == i }) {
          b.println(i + "05")
        }
        //c.println("testi")
      } finally {
        //a.close()
        b.close()
        //c.close()
      }
      
    }
}