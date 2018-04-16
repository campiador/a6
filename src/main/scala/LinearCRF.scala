object LinearCRF {
  val j = 6 // Number of features
  var m = 2

  type Label = String
  type Term = String
  type Sentence = Array[Term]

  val labels: List[Label] = List("NOUN", "VERB", "OTHER")

  /*

  FEATURE FUNCTIONS:

   */

  // VERB: is followed by "he/she/etc
  def f1(position: Int, yi: Label, yiminus1: Label, s: Sentence) : Double =
  {
    val x = s(position)
    if (yi.equals("VERB") && yiminus1.equals("OTHER") &&
      (x.equals("he") || x.equals("she") || x.equals("I") || x.equals("you") || x.equals("they") || x.equals("we"))
    )
    {
      return 1.0
    }
    return 0.0
  }

  // VERB: has "ed"
  def f2(position: Int, yi: Label, yiminus1: Label, s: Sentence) : Double =
  {
    val x = s(position)
    if (yi.equals("VERB") && x.endsWith("ed")) {
      return 1.0
    }
    return 0.0
  }

  // VERB: cannot come right after another VERB
  def f3(position: Int, yi: Label, yiminus1: Label, s: Sentence) : Double  =
  {
    val x = s(position)
    if (yi.equals("VERB") && yiminus1.equals("VERB")) {
      return 0.0
    }
    else {
      return 1.0
    }

  }

  //VERB: cannot come after "a" and "the"
  def f4(position: Int, yi: Label, yiminus1: Label, s: Sentence) : Double  =
  {
    if (position > 0) {
      val x_minusone = s(position - 1).toLowerCase
      if (x_minusone.equals("a") || x_minusone.equals("the")  ) {
        return 0.0
      } else
        return 1.0
    }
    return 0.0

  }

  // "To be" verbs are always VERB
  def f5(position: Int, yi: Label, yiminus1: Label, s: Sentence): Double =
  {
    val x = s(position)
    if (yi.equals("VERB") && (x.equals("were") || x.equals("is") || x.equals("are") || x.equals("was"))) {
      return 1.0
    }
    return 0.0

  }

////  NOUN: if term finishes with s, chances are it is a plural NOUN
//  def f5(position: Int, yi: Label, yiminus1: Label, s: Sentence) =
//  {
//    val x = s(position)
//    if (x.endsWith("s"))
//      1.0
//    else
//      0.0
//  }

  // term after salutation is NOUN
  def f6(position: Int, yi: Label, yiminus1: Label, s: Sentence) :Double =
  {
    if (position > 0) {
      val x_minusone = s(position - 1)
      if (yi.equals("NOUN") && (x_minusone.equals("Mr.") || x_minusone.equals("Ms."))) {
        return 1.0
      }
    }
    return 0.0
  }

  // if a term mid-sentence is capitalized, then it is a NOUN
  def f7(position: Int, yi: Label, yiminus1: Label, s: Sentence) : Double =
  {
    val x = s(position)
    if (yi.equals("NOUN") && !x.equals("I") &&
      x.capitalize(0).equals(x.charAt(0)) && position > 0) {
      return 1.0
    }
    return 0.0
  }

  // adjectives after "very" are OTHER
  def f8(position: Int, yi: Label, yiminus1: Label, s: Sentence) : Double =
  {
    if (position > 0) {
      val previous_term = s(position - 1)

      if (yi.equals("OTHER") && previous_term.equals("very")) {
        return 1.0
      }

    }
    return 0.0
  }





  import scala.math._

  import breeze.linalg._ //dense matrix, dense vector, sparse matrix etc.

  import breeze.optimize._

  def normalization(x: Array[Term], theta: DenseVector[Double]) = {
    // NEEDS WORK I do not quite understand this
    val features = for (y1 <- labels; y2 <- labels) yield {
      weightedFeatures(Array(y1, y2), x, theta)
    }

  }

  def weightedFeatures(y: Array[Label], x: Array[Term], theta: DenseVector[Double]) = {
    val sentence_length = x.length
    val features = for (i <- 0 until sentence_length) yield {
      exp(
          theta(0) * f1(i, y(i), y(i-1), x) +
          theta(1) * f2(i, y(i), y(i-1), x) +
          theta(2) * f3(i, y(i), y(i-1), x) +
          theta(3) * f4(i, y(i), y(i-1), x) +
          theta(4) * f5(i, y(i), y(i-1), x) +
          theta(5) * f6(i, y(i), y(i-1), x) +
          theta(6) * f7(i, y(i), y(i-1), x)
      )
    }
    features.reduceLeft(_*_)
  }

  def probability(y: Array[Label], x: Array[Term], theta: DenseVector[Double]) = {
    weightedFeatures(y, x, theta) / normalization(x, theta)
  }

// This function is actually log likelihood, which is the probablity fuction for all N training data
  def likelihood(theta: DenseVector[Double]) {

    val probabilities = for (sentence <- HelloWordApp.getTrainingData()) yield {
//      FIXME: what goes here for x and y
      // log(probability(y, x, theta))
    }
    probabilities.reduceLeft(_ + _)
  }
//
//  def likelihoodGradient(theta: DenseVector[Double]) {
//    DenseVector(0.0)
//  }
//
//  val objective = new DiffFunction[DenseVector[Double]] {
//    def calculate(x: DenseVector[Double]) = {
//      (likelihood(x), likelihoodGradient(x))
//    }
//  }
//


//  def g1(labels: Array[Label], terms: Array[Term]) = {
//
//  }


}