package gui

import convo.Sentence

object botThread extends Runnable {    // puhujan hallitsija

  def run() = {
    while (true) {
      kotipsykiatri.puhuja.checkState()
      if (kotipsykiatri.puhuja.state != 0) {
        val newSentence: Option[Sentence] = kotipsykiatri.puhuja.createSentence
        if (newSentence.isDefined) {
          kotipsykiatri.keskustelu.appendSentence(newSentence.get, false)
          kotipsykiatri.addButton(false)
          kotipsykiatri.reuna.revalidate()
        }
      }
      Thread.sleep(100)
    }
  }
  
}
