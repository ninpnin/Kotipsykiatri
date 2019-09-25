package stems

import scala.io.Source
import dictionary._
import utilities.Config.vocabPath
import scala.collection.mutable.Buffer
import java.io.PrintWriter

object Stemifier extends App {
	println("Hallo! Ich bin der Stemifier.")

	val root = "psykiatri/stems/"
	val inputFile = root + "text.txt"
	val outputFile = root + "text-output.txt"

	val outputF = new PrintWriter(outputFile)

	println("Ich werde die folgende Datei lesen " + inputFile +
			", sie in den Stamm und die Endung teilen, und " +
			"die bieden Teilen in die Datei " + outputFile + " speichern.")

	val valo: Nominal = Kotus1("valo", 'X')
	println(valo.stems)

	val rows = Source.fromFile(inputFile)
				.getLines()
				.toVector

	println("rows " + rows.length)

	val chars = ('A' to 'Z').toVector ++ Vector('Ä', 'Ö')
	val vocabFiles = Buffer[String]()
	
	for (C <- chars) {
		val path = vocabPath + "sanastot/" + C + "/" + C + "saannolliset"
		vocabFiles.append(path + "Nominit.txt")
		vocabFiles.append(path + "Verbit.txt")
		vocabFiles.append(vocabPath + "sanastot/" + C + "/yhdyssanat.txt")
	}

	val allInfinitives = scala.collection.mutable.Set[String]()
	val allStems = scala.collection.mutable.Set[String]()
	for (f <- vocabFiles) {
		val vocabWords = Source.fromFile(f).getLines().toVector.map(x => x.replace("&", ""))
		val vocabStems = vocabWords.map(DictionaryFinder.getStems(_))
		for (stemCollection <- vocabStems) {

			//println(stemCollection.toVector.reduceLeft(_ + ", " + _))
			for (s <- stemCollection) {
				if (s.contains("havahtu")) {
					println(s)
				}
				allStems += s
			}
		}
		for (infinitive <- vocabWords) {
			allInfinitives += infinitive.dropRight(4)
		}
	}

	/*
	val vocabWords = Source.fromFile(f).getLines().toVector
	val vocabStems = vocabWords.map(DictionaryFinder.getStems(_))
	for (stemCollection <- vocabStems) {

		//println(stemCollection.toVector.reduceLeft(_ + ", " + _))
		for (s <- stemCollection) {
			if (s.contains("havahtu")) {
				println(s)
			}
			allStems += s
		}
	}
	for (infinitive <- vocabWords) {
		allInfinitives += infinitive.dropRight(4)
	}*/ 
	
	println(allStems.take(100))
	println(allStems.toVector.length)

	val allStemsVec = allStems.toVector.sorted.filter(_.length > 1)
	val allInfinitivesVec = allInfinitives.toVector.sorted

	val usedStems = scala.collection.mutable.Set[String]()
	val endings = scala.collection.mutable.Set[String]()
	val uniqueWords = scala.collection.mutable.Set[String]()
	
	val stemMap = scala.collection.mutable.Map[String, (String,String)]("olin" -> ("olin", ""))

	println(getStem("etelätuuli"))
	println(getStem("etelätuulta"))
	println(getStem("korttipakkaa"))
	println(getStem("korttipakasta"))
	println(getStem("kirkkoherran"))

	println(DictionaryFinder.getWord("havahtuu"))
	println(getStem("havahtuen"))

	println(allInfinitivesVec.contains("herra"))
	println(allInfinitivesVec.contains("kirkko"))
	println(allStemsVec.contains("havahtuen"))

	println(allStemsVec.contains("havaht"))

	val endingMap = scala.collection.mutable.Map[String,Int]()

	var index = 0
	for (row <- rows) {
		val words = cleanUp(row.toLowerCase()).split(" ").toVector.filter{ x => x.length > 0 && x.length < 14 }
		//println(words)
		for (word <- words) {
			uniqueWords += word
			//println("Das Wort ist " + word)
			val stemEnding = getStem(word)
			val stem = stemEnding._1
			val ending = stemEnding._2

			usedStems += stem
			endings += ending

			val endingCount = endingMap.get(ending).getOrElse(0) + 1
			endingMap += (ending -> endingCount)

			if (index < 1000) {
				outputF.print(stem)
				if (ending != "") {
					outputF.print("+"+ending)
				}
				outputF.print(" ")
			} else if (index == 1000) {
				outputF.close()
			}

			if (index % 100 == 0) {
				val stat1 = uniqueWords.size
				val stat2 = usedStems.size
				val stat3 = endings.size
				val stat4 = (stat2 + stat3).toDouble / stat1
				val stat5 = endingMap.toVector.filter(x => x._2 > 1).size
				println(stemEnding)
				println(stat1, stat2, stat3, stat4, stat5, index)

				println(ending + " -> " + endingCount)
				val top100 = endingMap.toVector.sortBy(x => -x._2).take(20)
				println(top100)
			}
			index += 1
		}
	}
	
	def getStem(s: String): (String,String) = {
		if (stemMap.keySet.contains(s)) {
			//println("Dynamic!")
			return stemMap(s)
		}
		val inits = s.inits.toVector
		var bestStem = ""

		val intersection = inits.intersect(allInfinitivesVec)
		if (!intersection.isEmpty) {
			val bestCommon = intersection.maxBy(_.length)
			val bestLen = bestCommon.length
			if (bestLen > 2 && s.length - bestLen > 2) {

				val longest = intersection.maxBy(_.length)
				//println(s, longest)
				val rest = s.drop(longest.length)
				val subWord = getStem(rest)

				val inflected = subWord._1 != rest
				val infinitive = allInfinitivesVec.contains(rest)

				if (inflected || infinitive) {
					bestStem = longest + subWord._1
				}
			}
			
		}

		for (stem <- allStemsVec) {
			if (stem(0) == s(0)) {
				val intersection = Vector(stem).intersect(inits)
				if (!intersection.isEmpty) {
					val potentialStem = intersection.maxBy(_.length)
					if (bestStem.length < potentialStem.length) bestStem = potentialStem
				}
			}
			
		}
		val returnValue = if (bestStem.length >= 3) {
			(bestStem, s.drop(bestStem.length))
		} else {
			(s, "")
		}
		stemMap += (s -> returnValue)
		returnValue
	}

	val valoStems = DictionaryFinder.getStems("tahallinenAX38")
	println("valoStems " + valoStems)
	def cleanUp(s: String) = {
		s.toLowerCase()
		.replaceAll("[^A-Za-z0-9äöÄÖ\n ]", "")//.replaceAll("[^A-Za-z0-9äöÄÖ.,\n ]", "")
				.replace(".", " .")
				.replace(",", " ,")
	}


}