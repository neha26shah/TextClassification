import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex

class Tokenizer() {
  val REGEX_TAGS= new Regex("<.*?>")
  val REGEX_SPECIAL_CHARACTERS = new Regex("")
  def printStopwords(): Unit={
    Tokenizer.stopwords.foreach(println)
  }
  def getStopwords(): ListBuffer[String] ={
    return Tokenizer.stopwords
  }
  def convertToLower(input_text:String): String ={
    return input_text.toLowerCase()
  }
  def removeTags(input_text:String): String={
    return REGEX_TAGS.replaceAllIn(input_text,"")
  }
  def removeSpecialCharacters(input_text:String): String={
    return REGEX_SPECIAL_CHARACTERS.replaceAllIn(input_text,"")
  }
  def trimDocument(input_text:String):String={
    return input_text
  }
  def removeStopwords(input_tokens:Array[String]): ListBuffer[String]={
    return ListBuffer()
  }

  def tokenize(input_text:String):Array[String]={
    val lower_case:String = convertToLower(input_text)
    //val trim_document:String = trimDocument(lower_case)
    //val tags_removed:String = removeTags(trim_document)
    //val removed_characters:String = removeSpecialCharacters(tags_removed)
    val tokens: Array[String] = lower_case.split(" ")
    //return removeStopwords(tokens)
    return tokens
  }
}
object Tokenizer{
  val stopwords: ListBuffer[String] = ListBuffer()
  val filename: String = "data/additional-stopwords.txt"
  Tokenizer.readStopwords(filename)
  def readStopwords(filename:String): Unit={
    for(line <- Source.fromFile(filename).getLines()){
      stopwords+=line
    }
  }
}

