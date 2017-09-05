package gui

import java.awt.Graphics2D
import java.awt.Font
import java.awt.Color
import java.awt.Dimension
import scala.swing.GridPanel

class Puhekupla(teksti: String, kayttaja: Boolean) extends GridPanel(Puhekupla.width,30) {

  private val sansSerif = new Font("SansSerif", Font.BOLD, Puhekupla.px)


  val grouped = teksti.grouped(30).toVector

  val tsize = teksti.length
  val gsize = grouped.length
  val hheight = (gsize +1) * (3 + Puhekupla.px)


  val offset = Puhekupla.offset

  val xoffset = Puhekupla.xoffset

  val x1 = if (!kayttaja) Puhekupla.x1 else Puhekupla.x2
  val y1 = Puhekupla.y

  override def paintComponent(g: Graphics2D) = {
    g.setColor(Color.white)
    g.fillPolygon(x1, y1, 3)
    g.setColor(Color.black)
    g.drawPolygon(x1, y1, 3)
    g.setColor(Color.white)
    g.fillRect(xoffset+10, 0+offset, Puhekupla.width-2*xoffset-10, hheight+offset)
    g.setFont(sansSerif)
    g.setColor(Color.black)
    for (i <- 0 until gsize) {
      val string = grouped(i)
      g.drawString(string, 15+xoffset, 20+offset+ (Puhekupla.px + 3) * i)
    }
  }

  minimumSize = new Dimension(300,this.hheight)
}

object Puhekupla {
  
  val width = 280
  val px = 12
  
  val offset = 0
  val xoffset = 30
  
  val x1 = Array(xoffset+10,xoffset,xoffset+10)
  val y = Array(15+offset,18+offset,21+offset)
  
  val x2 = Array(width-xoffset-1,width-xoffset+13,width-xoffset-1)
}