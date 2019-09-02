package dictionary

import Kotus1._

object Test extends App {
	println("Moi!")
	val word1 = Kotus1("valo", 'X')
	println("Yksikkö")
	for (i <- 0 to 11)
		println(word1.taivuta(i, false))
	println("Monikko")
	for (i <- 0 to 11)
		println(word1.taivuta(i, true))
}