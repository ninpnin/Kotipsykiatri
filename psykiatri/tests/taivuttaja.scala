package tests

import dictionary._
import scala.io.StdIn.readLine

object taivuttaja extends App {
  
  val sana = readLine("Kirjoita sana tähän : ")
  val kirjain = readLine("Kirjoita astevaihtelu : ")
  val sana1 = Kotus73(sana,kirjain.head.toUpper)
  for (i<- 0 until 26) {
    println(sana1.taivuta(i))
  }
  
}