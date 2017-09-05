package gui

/**
 * @author Väinö
 */

import scala.swing._
import convo.Conversation
import convo.SpeechGenerator
import javax.imageio.ImageIO
import java.io.File
import scala.swing.event.EditDone
import scala.io.Source
import java.io.PrintWriter
import scala.util.Random
import scala.io.StdIn.readLine

import utilities.rng.shuffle

object kotipsykiatri extends SimpleSwingApplication {
  

  val tekstiPaneeli = new BoxPanel(Orientation.Vertical) {}

  val aihionOsoite = "puhujandata/aihiot1.txt"

  val keskustelu = new Conversation
  val puhuja = new SpeechGenerator("assosiaatiot.txt",aihionOsoite,keskustelu)

  // siirtää väliaikaistiedostojen datan puhujandata-kansion tiedostoihin
  def init(a: Map[String,String]) = {
    for (b <- a) {
      val s1 = Source.fromFile(b._1).getLines()
        .toVector ++ Source.fromFile(b._2).getLines().toVector
      val p1 = new PrintWriter(b._1)
      val p2 = new PrintWriter(b._2)
      try p1.println() finally p1.close()
      try {
        for (i <- shuffle(s1))
          if (i.nonEmpty) p2.println(i)
      } finally p2.close()
      println("alustettu : " + b._1 + " -> " + b._2)
    }
  }

  def debugOn() = {
    puhuja.debugOn()
    println("Debuggaus päällä!")
  }

  def debugOff() = puhuja.debugOff()

  def command(): Unit = readLine("Syötä komento : ") match {
    case "aiheet" => println("aiheet :" + keskustelu.aiheet.fold("")((a,b)  => a + " " + b))
    case _ => Unit
  }

  this.addButton(false)

  val tekstiP = new ScrollPane(tekstiPaneeli) {
    maximumSize = new Dimension(512,512)
  }

  val kuvanSijainti = "user1.png"
  val kuvaPaneeli = new BoxPanel(Orientation.Vertical) {        // kotipsykiatrin kuva/ikoni
    val bufferedImage = ImageIO.read(new File(kuvanSijainti))
    override def paintComponent(g:Graphics2D) = {
      if (null != bufferedImage) g.drawImage(bufferedImage, 0, 0, null)
    }
    maximumSize = new Dimension(512,512)
  }

  val textField = new TextField(28)
  textField.tooltip_=("Kirjoita viestisi tähän.")
  this.listenTo(textField)

  val reuna = new BorderPanel() {
    layout(kuvaPaneeli) = BorderPanel.Position.Center
    layout(tekstiP) = BorderPanel.Position.East
    layout(textField) = BorderPanel.Position.South
    maximumSize = new Dimension(800,520)
  }

  val frame = new MainFrame() {
    minimumSize = new Dimension(600,500)
    this.menuBar = new MenuBar {
      contents += new Menu("Assosiaatiot") {
        contents += new MenuItem(Action("Nollaa") {  } )
      }
      contents += new Menu("Lause-aihiot") {
        contents += new MenuItem(Action("Nollaa") {  } )
      }
      contents += new Menu("Debuggaus") {
        contents += new MenuItem(Action("päälle") { debugOn() } )
        contents += new MenuItem(Action("pois") { debugOff() } )
        contents += new MenuItem(Action("komento") { command() } )
      }
    }
  }

  frame.contents_=(reuna)
  def top = frame
  frame.size = new Dimension(700,550)

  def addButton(kayttaja: Boolean) = {
    val nappula = new Puhekupla(keskustelu.newestSentenceText,kayttaja)
    var ttip = ""
    for (j<-keskustelu.newestSentenceTuple._1.wordList) {
       ttip += j.toString() +  " "
    }
    nappula.tooltip = ttip
    nappula.preferredSize = new Dimension(300,60)
    val flow = new FlowPanel() {
      minimumSize = new Dimension(300,60)
      contents += nappula
    }
    tekstiPaneeli.contents += flow
  }

  this.init(Map("startingFiles/association.txt"->"puhujandata/aiheet.txt",
      "startingFiles/causalities.txt"->"puhujandata/causalities.txt",
      "startingFiles/sentences.txt"->"puhujandata/lauseet.txt"))
  
  new Thread(botThread).start()

  this.reactions += {
    case edit: EditDone => {
      var text = textField.text  // lisää feedin listaan
      if (text != "") {
        text = text(0).toLower +text.replace("?", "?.").replace("!", "!.").drop(1)  // jotta kysymyslauseet erotettaisiin myös
        val texts = text.split("\\.")
        for (i <- texts) {
          keskustelu.appendSentence(i,true)
          this.addButton(true)
        }
        textField.text = ""
        reuna.revalidate()
      }
    }
  }
}