package gui

/**
 * @author V�in�
 */

import scala.swing._
import convo.Conversation
import convo.SpeechGenerator
import javax.imageio.ImageIO
import java.io.File
import scala.swing.event.EditDone
import java.awt.Color

object sisalto extends SimpleSwingApplication {

  val textField1 = new TextField(28)  {
    maximumSize = new Dimension(600,100)
    tooltip_=("Kirjoita tunnistettava lause t�h�n.") 
  }
  this.listenTo(textField1)

  val textField2 = new TextField(28) {
    tooltip_=("Kirjoita reaktio t�h�n.") 
    maximumSize = new Dimension(600,100)

  }
  this.listenTo(textField1)

  val ylaPaneeli = new GridBagPanel() {
    val a = new Constraints
    a.grid = (2,1)
    val b = new Constraints
    b.grid = (2,3)
    val c = new Constraints
    c.grid = (2,0)
    val d = new Constraints
    d.grid = (2,2)
    val e = new Constraints
    e.grid = (1,1)
    layout(textField1  ) = a
    layout(textField2  ) = b
    layout(new Label("Lis�� t�h�n lause, johon reagoidaan")) = c
    layout(new Label("Lis�� t�h�n vaste")) = d
  }
  
  val reuna = new BorderPanel() {
    layout(ylaPaneeli) = BorderPanel.Position.Center
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
      contents += new Menu("Sanastot") {
        contents += new MenuItem(Action("Nollaa") {  } )
      }
    }
  }
  
  frame.contents_=(reuna)
  def top = frame
  frame.size = new Dimension(700,550)
    
  /*
  this.reactions += {
    case edit: EditDone => {
      var text = textField1.text  // lis�� feedin listaan
      if (text != "") {
        text = text.replace("?", "?.")  // jotta kysymyslauseet erotettaisiin my�s
        val texts = text.split("\\.")
        for (i<-texts) {
          keskustelu.lisaaLause(i,true)
          val flow = new FlowPanel() { 
            minimumSize = new Dimension(300,20)
            val nappula = new Button
            contents += nappula
            nappula.text = keskustelu.uusinLause()
            nappula.borderPainted =(false)
            
            nappula.background = Color.white
            var ttip = ""
            for (j<-keskustelu.uusinLausel()._1.words()) {
              ttip += j.toString() +  " "
            }
            nappula.tooltip = ttip
  
          }
          tekstiPaneeli.contents += flow
        }
        textField1.text = ""
        reuna.revalidate()
        //puhuja.checkState()
        puhuja.state=1 /*
        keskustelu.lisaaLause(puhuja.luoLause())
        val flow2 = new FlowPanel() {
          minimumSize = new Dimension(300,20)
          val nappula = new Button
          contents += nappula
          nappula.text= keskustelu.uusinLause()
          nappula.borderPainted =(false)
          nappula.background = Color.white
          
        }
        tekstiPaneeli.contents += flow2 
        reuna.revalidate()
        * 
        */
      }
    }
  }
  */
  
}