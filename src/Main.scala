
object Main{
  def main(args:Array[String]):Unit={
    val classifier = new MultinomialNaiveBayes()
    classifier.train_model("/home/neha/Desktop/Semesters/Fall2015/NLP/NLP_HW1/large_movie_review_dataset/train")
    //println(classifier.getVocab())
    println(classifier.getClassTotalDocCounts())
    println(classifier.getClassTotalWordCounts())
    //println(classifier.getClassWordCounts())
    //println(classifier.getListClasses().toString())
    println(classifier.getPriors())
    println(classifier.countDocuments())
    println(classifier.predict("dad girl.",0.1))
    println(classifier.getAccuracy("/home/neha/Desktop/Semesters/Fall2015/NLP/NLP_HW1/large_movie_review_dataset/test",12))
  }
}