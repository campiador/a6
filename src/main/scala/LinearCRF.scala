object LinearCRF {
  val k = 3
  var m = 2

  type Label = String
  type Term = String

  val labels: List[Label] = List("NOUN", "VERB", "OTHER")


  def f1(yi: Label, yiminus1: Label, x1: Term, x2: Term, x3:Term) =
  {
    0.0
  }

  def f2(yi: Label, yiminus1: Label, x1: Term, x2: Term, x3:Term) =
  {
    0.0
  }

  import scala.math._

  import breeze.linalg._ //dense matrix, dense vector, sparse matrix etc.

  import breeze.optimize._

  def normalization(x: Array[Term], theta: DenseVector[Double]) = {
    val features = for (y1 <- labels; y2 <- labels) yield {
      weightedFeatures(Array(y1, y2), x,theta)

    }

  }

  def weightedFeatures(y: Array[Label], x: Array[Term], theta: DenseVector[Double]) = {
    val features = for (i <- 0 until k) yield {
      exp(
        theta(0) * f1(y(i), y(i-1), x(0), x(1), x(2)) + theta(1) * f2(y(i), y(i-1), x(0), x(1), x(2))
      )
    }
    features.reduceLeft(_*_)
  }

  def probability(y: Array[Label], x: Array[Term], theta: DenseVector[Double]) = {
    weightedFeatures(y, x, theta) / normalization(x, theta)


  }


  def likelihood(theta: DenseVector[Double]) {
    0.0
  }

  def likelihoodGradient(theta: DenseVector[Double]) {
    DenseVector(0.0)
  }

  val objective = new DiffFunction[DenseVector[Double]] {
    def calculate(x: DenseVector[Double]) = {
      (likelihood(x), likelihoodGradient(x))
    }
  }



  def g1(labels: Array[Label], terms: Array[Term]) = {

  }


}