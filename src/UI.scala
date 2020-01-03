import scala.collection.mutable.Buffer
import java.io.FileWriter
import java.util.Calendar

object TextUI extends App {
  
  
  this.readPersonality
  
  var endCommandGiven = false
  val welcomeMessage = "Hello! I am " + Personality.myName + ". And you are?"
  val byeMessage = "Bye!"
  val endCommand = "stop"
  
  // Keskustelun tallentaminen arkistoon
  def save = {
    println("Type file name if you would like to save this conversation:")
    val fileName = readLine("" )
    if (!fileName.isEmpty) {
      this.addToArchive(fileName)
      println("Conversation saved.")
    }
  }
  
  
  
  // Luetaan tiedostosta tekoälyn asetukset
  def readPersonality = {
    //val source = scala.io.Source.fromFile("C:\\tekoAly\\aly1.txt")
    val source = scala.io.Source.fromFile("./chatbot_malliasetukset.txt")
    val lines = try source.mkString finally source.close()
    Personality.readPersonality(lines)
    
  }

  // Tallennetaan tämänhetkinen Conversation-olio arkistoon
  def addToArchive(name: String) = {
    val writer = new FileWriter(name, true)
    try {
      writer.write(System.getProperty("line.separator") + System.getProperty("line.separator") + Calendar.getInstance().getTime() + "\tAI:\t" + Personality.myName)
      for (line <- Conversation.lines) {
        writer.write(System.getProperty("line.separator") + line)
      }
    }
    finally writer.close
  }

  this.run()
  
  private def run() = {
    println(welcomeMessage)
    Conversation.addLine(Personality.myName + ":\t" + welcomeMessage)
    this.listen()
    while (endCommandGiven == false) {
      this.answer()
      this.listen()
    }
    println(Notes.feedback)
    this.save
    println(byeMessage)
  }
  
  // Antaa saadun viestin käsiteltäväksi metodille Message
  private def listen() = {
    val message = readLine("" )
    val rawText = new Message(message)
    Notes.addMessage(rawText)
    if (message == endCommand) this.endCommandGiven = true
    // Analysoidaan käyttäjän viesti luokassa Message
    Notes.addSc(rawText.chop)
    // Tallennetaan luokkaan Conversation
    if (message != endCommand) Conversation.addLine("You:\t" + message)
  }
  
  // Saa vastauksen muistiinpanojen kautta luokasta Answer
  private def answer() = {
    val answer = Notes.reaction
    println(answer)
    Notes.addResponse(answer)
    // Tallennetaan luokkaan Conversation
    Conversation.addLine("AI:\t" + answer)

  }
  
}
