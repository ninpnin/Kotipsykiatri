package tests

import java.awt.Dimension

import gui.Puhekupla

import scala.swing.{BorderPanel, MainFrame, SimpleSwingApplication}

object visualTest extends SimpleSwingApplication {

  val frame = new MainFrame() {
    minimumSize = new Dimension(600,500)
    contents = new Puhekupla("kaldsaadsadsadsdasdasasdadsadsasdjaaaa kaldsaadsadsadsdasdasasdadsadsasdjaaaa kaldsaadsadsadsdasdasasdadsadsasdjaaaa", false)
  }
  
  val border = new BorderPanel()
  
  def top() = frame
  
}