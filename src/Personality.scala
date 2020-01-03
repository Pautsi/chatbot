import scala.collection.mutable.Buffer
import scala.collection.mutable.Map
import scala.util.Random


object Personality {
  
  /* Syötettävät tiedot sisältävät:
  * tunnistettavia rakenteita yleisellä tasolla (vaihtoehtoja tyyliin 20): tervehdys, huudahdus, loukkaus, mielenilmaus, adjektiivi, verbi jne
  * 	viestiin liittyviä tunnelatauksia: iloinen, surullinen, neutraali, mahdollisesti muita
  * 	rakenteet ensisijaisuusjärjestyksessä
  * tekoälyn vastauspankki
  * 	jokaisen vastauksen yhteydessä ilmoitetaan siihen liittyvä tunnelataus sympatia-antipatia-akselilla
  * 	vastaukset on yhdistetty rakenteisiin, joihin vastataan
  * Lista loukkauksista ja kehuista
  * 
  * Syötettävän kaavakkeen purku:
  * - taulukko, jonka avaimia syötettyjen rakenteiden nimet ovat ja jonka arvot ovat listoja näistä rakenteista
  * - taulukko, jonka avaimia ovat tunnetilat ja arvoja näihin tunnetiloihin liittyvät sanat
  * - taulukko, jonka avaimia syötettyjen rakenteiden nimet ovat ja jonka arvot ovat lista soveltuvia vastauksia tähän
  * 		vastauksen lopussa + indikoi sympatiaa, - antipatiaa ja 0 on neutraali
  */
  
  // Tekoälyn asetukset
  var myName = ""
  var greeting = ""
  var greetingError = ""
  var types = Vector[String]() 
  var recTypes = Map[String, Vector[String]]()  // Tunnistettavat rakenteet
  var insults = Vector[String]()
  var praises = Vector[String]()
  var storage = Map[String, Vector[String]]()  // Vastauspankki
  var positive = Vector[String]()
  var negative = Vector[String]()
  var ignoreWords = Vector[String]() // Täytesanat
  
  // Päivitetään asetukset luettavasta tiedostosta
  def readPersonality(sheet: String) = {
    def convertToVector(str: String) = str.dropRight(2).split(";").toVector
    val parameters = sheet.split("#")
    myName = parameters(1).dropRight(2)
    greeting = parameters(2).dropRight(2)
    greetingError = parameters(3).dropRight(2)
    types = convertToVector(parameters(4).drop(7))
    val patterns1 = parameters(5).dropRight(2).drop(10).split("\n")
    val patterns2 = patterns1.map(p => p.trim)
    val patternsAsVectors = patterns2.map(p => p.split(";").toVector)
    for (i <- 0 until patternsAsVectors.size) {
      recTypes += types(i) -> patternsAsVectors(i)
    }
    val answers1 = parameters(6).dropRight(2).drop(10).split("\n")
    val answers2 = answers1.map(a => a.trim)
    val answersAsVectors = answers2.map(a => a.split(";").toVector)
    for (i <- 0 until answersAsVectors.size - 3) {
      storage += types(i) -> answersAsVectors(i)
    }
    storage += "elseNow" -> answersAsVectors(answersAsVectors.size - 3)
    storage += "else" -> answersAsVectors(answersAsVectors.size - 2)
    storage += "elseInCase" -> answersAsVectors.last
    insults = convertToVector(parameters(7).drop(9))
    praises = convertToVector(parameters(8).drop(9))
    positive = convertToVector(parameters(9).drop(10))
    negative = convertToVector(parameters(10).drop(10))
    ignoreWords = parameters(11).drop(8).split(";").toVector
  }
  
  def symphathy(a: String) = a.last == '+'
  def antipathy(a: String) = a.last == '-'
  def neutral(a: String) = a.last == '0'
  def trimAnswer(a: String) = a.dropRight(1)
  
  
  //Apumetodi, joka hakee hakemistosta listan vastauksista
  def getLine(key: String): Vector[String] = {
    val options = storage.getOrElse(key, Buffer("Search in " + key + "failed.")).toVector
    options
  }
  
  // Alkutervehdys
  def greet(message: Message): String = {
    val phrase = greeting
    val reply = message.findName match {
      case Some(name) => {
        Notes.nameReceived = true
        Notes.name = Some(name)
        "Nice to meet you, " + name + "! " + phrase
      }
      case None => greetingError
    }
    reply
  }

  
}