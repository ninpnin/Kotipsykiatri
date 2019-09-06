package stems

import scala.io.Source
import dictionary._

object Stemifier extends App {
	println("Hallo! Ich bin der Stemifier.")

	val inputFile = "a.txt"
	val outputFile = "b.txt"

	println("Ich werde die folgende Datei lesen " + inputFile +
			", sie in den Stamm und die Endung teilen, und " +
			"die bieden Teilen in die Datei " + outputFile + " speichern.")

	val valo: Nominal = Kotus1("valo", 'X')
	println(valo.stems)


	val words = Vector("valoa", "kansalle")


	for (word <- words) {
		println("Das Wort ist " + word)
	}

}