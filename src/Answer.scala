import scala.util.Random


// Muodostaa vastauksen

object Answer { 
  
    // Etsii vastauksista sen, joka sopii tekoälyn mielialaan
    def fitsMood(comment: String) = {
      val tone = comment.last.toString
      (Notes.mood > 0 && tone == "+") || (Notes.mood < 0 && tone == "-") || (Notes.mood == 0 && tone == "0")
    }
    def pickByMood(comments: Vector[String]) = {
      val commentsByMood = comments.filter(c => fitsMood(c))
      if (commentsByMood.isEmpty) {
        val shuffled = Random.shuffle(comments)
        shuffled(0)
      } else {
        val shuffled = Random.shuffle(commentsByMood)
        shuffled(0)
      }
    }
  
  // Muodostaa vastauksen 
  def generate(message: Message) = {
    val types = Personality.types.reverse
    var recognized = false
    var foundType = "else"
    var quote = ""
    // Käydään läpi eri tyyppiset tunnistettavat rakenteet
    for (someType <- types) {
      // Käydään läpi kunkin rakennetyypin tunnistettavat rakenteet
      val instances = Personality.recTypes(someType)
      for (instance <- instances) {
        val instance2 = instance.replace("-", "")
        if (message.toString.toLowerCase.contains(instance2)) {
          recognized = true
          foundType = someType
          // Poikkeustapaus: yhden merkin pituiset rakenteet
          if (instance.length == 1) {
            val newInstance = message.toString.toLowerCase.replace(instance, "<")  
            val split = message.toString.toLowerCase.split(newInstance)
            if (split.size >= 2) quote = split(1)
            else quote = ""
          // Päätteet merkittynä väliviivalla
          } else if (!instance.contains("-")) {
            val split = message.toString.toLowerCase.split(instance)
            if (split.size >= 2) quote = split(1)
            else quote = ""
          } else {
            val words = message.toWords(message.toString)
            val verbs = words.filter(_.contains(instance.drop(1)))
            val targetWord = verbs(0)
            val split = message.toString.toLowerCase.split(targetWord)
            if (split.size >= 2) quote = targetWord + split(1)
            else quote = targetWord
          }
        }
      }
    }
    
    // Tyyppi "else" tai "elseNow" silloin, kun muita rakenteita ei löydy
    if (foundType == "else") {
      val i = scala.util.Random.nextInt(2)
      if (i == 0) {
        foundType = "elseNow"
      }
    }
    
    val comment = pickByMood(Personality.storage(foundType)).dropRight(1)
    
    if (foundType == "else") {
      // Jos aiempia viestejä ei ole, ei regoida mihinkään viestiin 
      if (Notes.subMessages.size == 1) {
        pickByMood(Personality.storage("elseInCase")).dropRight(1)
      }
      else {
        quote = Notes.subMessages(0).translate
        Notes.deleteFirst
        comment.replace("_", quote).dropRight(1)
      }
    }
    // Vastaus silloin, kun tyyppi ei ole else
    else {
      if (foundType == "elseNow") {
        quote = Notes.subMessages.last.toString
      }
      // Normaalitapaus
      val quoteAsMessage = new Message(quote)
      comment.replace("_", quoteAsMessage.translate)
    }

  }
  
  // Virkkeen viimeistely
  def finish(sentence: String): String = {
    sentence(0).toUpper + sentence.drop(1) + "."
  }
  
}