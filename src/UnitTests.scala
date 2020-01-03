import org.junit.Test
import org.junit.Assert._
import scala.collection.mutable.Buffer

/**
 * Some simple unit tests.
 */
class UnitTests {
  
  // Test finding name and greeting before adding anything to Personality
  @Test def tellName() {
    val input = "My name is Bilbo Baggins."
    Notes.nameReceived = false
    Notes.receivedMessages = Buffer(new Message(input))
    val result = Notes.reaction
    assertEquals("Nice to meet you, Bilbo! ", result)
  }
  
  @Test def readTypesFromFile() {
    val source = scala.io.Source.fromFile("C:\\tekoAly\\aly1.txt")
    Personality.readPersonality(try source.mkString finally source.close())
    val result = Personality.types
    val expectedTypes = Vector("opinions", "feelings", "definition", "verb", "question")
    assertEquals(expectedTypes, result)
  }
  
  @Test def accurateAnswerByMood() {
    val input = "I know I don't look old, but I feel it in my heart"
    Notes.nameReceived = true
    Notes.mood = 0
    Notes.subMessages = Buffer(new Message(input))
    Personality.storage += "feelings" -> Vector("I also feel _.+", "Why do you feel _?0", "I don't get you, man.-")
    Personality.types = Vector("feelings")
    Personality.recTypes += "feelings" -> Vector("i feel")
    val result = Notes.reaction
    val expected = "Why do you feel it in your heart?"
    assertEquals(expected, result)
  }
  
  @Test def accurateAnswerWhenNoRecognizableType() {
    val input1 = "I need a holiday."
    val input2 = "A very long holiday."
    Notes.nameReceived = true
    Notes.subMessages = Buffer(new Message(input1), new Message(input2))
    Personality.storage += "feelings" -> Vector("I also feel _.+")
    Personality.storage += "else" -> Vector("Remember when you said _?0 ")
    Personality.types = Vector("feelings")
    Personality.recTypes += "feelings" -> Vector("i feel")
    val result = Notes.reaction
    val expected = "Remember when you said you need a holiday?"
    assertEquals(expected, result)
  }
  
  @Test def changeMood() {
    Notes.mood = 0
    Personality.praises = Vector("allright")
    val input = "You are allright."
    Notes.addMessage(new Message(input))
    assertTrue(Notes.mood == 1)
  }



}


