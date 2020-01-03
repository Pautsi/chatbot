import scala.collection.mutable.Buffer

// Käsittelee (pääasiassa) käyttäjältä saatuja viestejä

class Message(text: String) {
  
  val sentences = this.chopToBuffer(text)
  
  override def toString = text
  
  def chop() = this.chopToBuffer(text)
  
  // Pilkotaan viesti virkkeisiin, joihin voi reagoida
  def chopToBuffer(text: String): Buffer[String] = {
    var parts = text.trim
    if (!parts.contains(". ") && !parts.contains("? ") && !parts.contains("! ")) Buffer(parts)
    else {
      if (parts.contains(". ")) {
        val first = parts.take(text.indexOf(". ") + 1)
        val second = parts.takeRight(text.size - 2 - text.indexOf(". "))
        this.chopToBuffer(first) ++ this.chopToBuffer(second)
      } else if (parts.contains("? ")) {
        val first = parts.take(text.indexOf("? ") + 1)
        val second = parts.takeRight(text.size - 2 - text.indexOf("? "))
        this.chopToBuffer(first) ++ this.chopToBuffer(second)
      } else {
        val first = parts.take(text.indexOf("! ") + 1)
        val second = parts.takeRight(text.size - 2 - text.indexOf("! "))
        this.chopToBuffer(first) ++ this.chopToBuffer(second)
      }
    }
  }
  
  // Poistaa viestistä täytesanat, jotka on syötetty tekoälyn asetuksissa
  def deleteEmptyWords = {
    var out = text
    for (iw <- Personality.ignoreWords) {
      out = out.replace(iw, "")
      out = out.replace("  ", " ")
    }
    new Message(out)
  }
  
  // Pilkkoo annetun lauseen sanoiksi
  def toWords(in: String): Array[String] = {
    var out = in.filter(c => ("QWERTYUIOPASDFGHJKLZXCVBNMqwertyuiopasdfghjklzxcvbnm ").contains(c))
    out.trim.split(" ")
  }
  
  
  // Etsii viestistä nimen:
  // Yhden sanan viestissä sana tulkitaan nimeksi, kahden sanan viestissä ensimmäinen sana ja usean sanan viestissä ensimmäinen isolla alkukirjaimella alkava sana
  def findName: Option[String] = {
    if (toWords(text).size == 1) Some(toWords(text)(0))
    else if (toWords(text).size == 2) Some(toWords(text)(0))
    else {
      var possibleName = ""
      var newParagraphs = this.sentences.map(p => p.drop(1))
      var paragraphsThatContainCapital = newParagraphs.filter(_.exists(_.isUpper))
      if (paragraphsThatContainCapital.isEmpty) None
      else {
        var names = toWords(paragraphsThatContainCapital(0)).filter(w => w.exists(_.isUpper) && w != "I")
        if (names.size == 0) None
        else Some(names(0))
      }
    }
  }
  
  // Muuttaa viestin sisältämät persoonapronominit muodosta I/my/mine/me muotoon you/your/yours/you
  def translate: String = {
    if (this.toString == "") ""
    else {
      def changePronoun(w: String) = {
        if (w.toLowerCase == "i" || w.toLowerCase == "me") "you"
        else if (w.toLowerCase == "my") "your"
        else if (w.toLowerCase == "mine") "yours"
        else if (w.toLowerCase == "am") "are"
        else if (w.toLowerCase == "you") "I"
        else if (w.toLowerCase == "your") "my"
        else if (w.toLowerCase == "yours") "mine"
        else if (w.toLowerCase == "are") "am"
        else w
      }
    val small = text(0).toLower + text.drop(1)
    val words = toWords(small)
    val translatedWords = words.map(w => changePronoun(w))
    translatedWords.mkString(" ")
    }
  }
  
  // Etsii viestistä asetuksissa positiivisiksi ja negatiivisiksi määritellyt sanat ja päivittää luokan Notes laskuria
  def associate = {
    for (pos <- Personality.positive) {
      if (this.text.contains(pos)) Notes.positiveWords += 1
    }
    for (neg <- Personality.negative) {
      if (this.text.contains(neg)) Notes.negativeWords += 1
    }
  }

  
}