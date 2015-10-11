import collection.mutable.HashMap
import java.io.File
import scala.io.Source._

class MultinomialNaiveBayes {

  def train_model(directory_name:String): Unit={
    val classes_dir = (new File(directory_name)).listFiles.filter(_.isDirectory)
    MultinomialNaiveBayes.list_classes= classes_dir.map(_.getName)
    for(class_name<-MultinomialNaiveBayes.list_classes){
      MultinomialNaiveBayes.class_word_counts.put(class_name,new HashMap[String,Int]())
    }
    // Get all the classes in the folder.
    for(class_name<- classes_dir){
      val directory_struct = class_name.listFiles.filter(_.isFile)
      for(file<-directory_struct){
        val lines = fromFile(file).getLines().mkString
        val tokens = MultinomialNaiveBayes.tokenizer.tokenize(lines)
        update_dictionaries(tokens,class_name.getName)
      }
    }
    for(class_name<-MultinomialNaiveBayes.list_classes){
      val log_prior = math.log( MultinomialNaiveBayes.class_total_doc_counts.getOrElse(class_name,0).toDouble) - math.log( MultinomialNaiveBayes.total_documents.toDouble)
      MultinomialNaiveBayes.priors += (class_name->log_prior)
    }
  }

  def update_dictionaries(tokens:Array[String],class_name:String):Unit={
      MultinomialNaiveBayes.total_documents = MultinomialNaiveBayes.total_documents+1
      MultinomialNaiveBayes.class_total_doc_counts.put(class_name,MultinomialNaiveBayes.class_total_doc_counts.getOrElse(class_name,0)+1)
      for(token<-tokens){
        MultinomialNaiveBayes.vocab += token
        MultinomialNaiveBayes.class_total_word_counts.put(class_name,MultinomialNaiveBayes.class_total_word_counts.getOrElse(class_name,0)+1)
        val original_counts = MultinomialNaiveBayes.class_word_counts.getOrElse(class_name,new HashMap[String,Int]()).getOrElse(token,0)
        MultinomialNaiveBayes.class_word_counts.getOrElse(class_name,new HashMap[String,Int]()).put(token,original_counts+1)
      }
  }


  def unnormalized_log_posterior(tokens:Array[String],class_name:String,alpha:Float): Double ={
    var log_likelihood=0.0
    for(token<-tokens){
      if ((MultinomialNaiveBayes.vocab contains token )) {
        val item1 = MultinomialNaiveBayes.class_word_counts.getOrElse(class_name, new HashMap[String, Int]()).getOrElse(token, 0)
        val item2 = MultinomialNaiveBayes.class_total_word_counts.getOrElse(class_name,0)
        val item3 = MultinomialNaiveBayes.vocab.size
        val prob = (item1+ alpha)/(item2 + (item3*alpha))
        log_likelihood = log_likelihood + math.log(prob)
      }
    }
    return log_likelihood + MultinomialNaiveBayes.priors.getOrElse(class_name,0.0)
  }

  def predict(predictStatement:String,alpha:Float): String = {
    val tokens = MultinomialNaiveBayes.tokenizer.tokenize(predictStatement)
    var max_score= 0.0
    var max_label= ""
    for(class_name<-MultinomialNaiveBayes.list_classes){
      val class_score = unnormalized_log_posterior(tokens,class_name,alpha)
      if (max_label== "" || (max_score<=class_score)){
        max_score = class_score
        max_label = class_name
      }
    }
    return max_label
  }
}

object MultinomialNaiveBayes{
  var vocab:Set[String] = Set()
  val class_total_doc_counts= new HashMap[String,Int](){override def default(key:String) = 0}
  val class_total_word_counts= new HashMap[String,Int](){override def default(key:String) = 0}
  val class_word_counts= new HashMap[String,HashMap[String,Int]]()
  var list_classes = Array.empty[String]
  //Define the tokenizer here
  val tokenizer = new Tokenizer()
  var priors = new HashMap[String,Double]()
  var total_documents = 0
}
