import scala.collection.mutable.Buffer


/* Sisältää listan puheenvuoroista
* Pitää kirjaa käyttäjän syöttämistä viesteistä ja virkkeistä
* Pitää kirjaa siitä, mihin käyttäjän virkkeisiin ei ole vielä vastattu
* pitää kirjaa tekoälyn kokemasta tunnetilasta (mood)
* pitää kirjaa äyttäjän viestissä esiintyvistä positiivista ja negatiivisista sanoista
* 
*/

object Notes {
  
  var mood = 0
  var negativeWords = 0
  var positiveWords = 0
  var nameReceived = false
  var name: Option[String] = None
  var userName = {
    this.name match {
      case Some(username) => username
      case None => "Anonymous"
    }
  }
  var received = Buffer[String]()  
  var receivedMessages = Buffer[Message]()  // vastaanotetut viestit
  var sent = Buffer[String]()               // lähetetyt viestit
  var subMessages = Buffer[Message]()       // Virkkeet, joihin tekoäly ei vielä ole reagoinut
  
  // Lisätään uusi vastaanotettu viesti ja reagoidaan siihen tunnetasolla
  def addMessage(message: Message) = {
    reactEmotionallyToMessage(message)
    message.associate
    this.received += message.toString
    this.receivedMessages += message
  }
  
  def addResponse(answer: String) = this.sent += answer
  // Lisää annetut virkkeet kokoelmaan subMessages odottamaan vuoroaan
  def addSc(scs: Buffer[String]) = scs.foreach(s => subMessages = subMessages :+ new Message(s))
  def deleteLatest = subMessages = subMessages.dropRight(1)
  def deleteFirst = subMessages = subMessages.drop(1)
  
  
  // Muodostetaan reaktio luokan Answer avulla
  def reaction: String = {
    // Tervehdys
    if (!nameReceived) {
      val answer = Personality.greet(receivedMessages.last)
      deleteLatest
      answer
    }
    // Tavallinen viesti
    else {
      val answer = Answer.generate(subMessages.last.deleteEmptyWords)
      deleteLatest
      answer
    }
  }
  
  // Tunnistetaan viestistä tilanne, jossa käyttäjä puhuttelee tekoälyä suoraan ja muuteteaan mielialaa tarvittaessa
  def reactEmotionallyToMessage(m: Message) = {
    var line = m.toString.toLowerCase
    if (line.contains("you are")) {
      val definition = m.toWords(line.split("you are ")(1))(0)
      for (insult <- Personality.insults) {
        if (insult == definition) moodDown
      }
      for (praise <- Personality.praises) {
        if (praise == definition) moodUp
      }
    } else if (line.contains("you seem")) {
      val definition = m.toWords(line.split("you seem ")(1))(0)
      for (insult <- Personality.insults) {
        if (insult == definition) moodDown
      }
      for (praise <- Personality.praises) {
        if (praise == definition) moodUp
      }
    } else if (line.contains("you look")) {
      val definition = m.toWords(line.split("you look ")(1))(0)
      for (insult <- Personality.insults) {
        if (insult == definition) moodDown
      }
      for (praise <- Personality.praises) {
        if (praise == definition) moodUp
      }
    }
    
  }
  def moodUp = mood += 1
  def moodDown = mood -= 1
  
  // Antaa palautetta sen mukaan, mikä mieliala tekoälyllä on ja miten paljon positiivisia sanoja suhteessa negatiivisiin
  // tekoäly on kerännyt keskustelun lopussa
  def feedback = {
    def positiveIndex = (1.0 * positiveWords) / (1.0 * (positiveWords + negativeWords)) * 100.0
    if (mood > 0 && positiveIndex >= 69) "You really are the life of this party. Well done!"
    else if ((mood > 0) || (mood == 0 && positiveIndex > 69)) "This has been a pleasant conversation. Keep on going!"
    else if (mood == 0) "I had a fairly good time chatting with you. Maybe try to cheer up a bit?"
    else if (mood < 0 && positiveIndex > 69) "Well, one of us had a good time. Maybe pay attention to you manners a bit more?"
    else if (mood < 0 && positiveIndex >= 40) "This was dull. Could you manage a more cheerful tone?"
    else "Now excuse me, I think I saw my friend over there..."
  }
  
}