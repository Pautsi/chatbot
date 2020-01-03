import scala.collection.mutable.Buffer

// Kerää molempien osapuolien viestit

object Conversation {
  
  var lines = Buffer[String]()
  
  def addLine(line: String) = {
    lines += line
  }
  
  
  
}