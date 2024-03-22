package statistics

object Statistics {

  def average[T](data: List[T], f: T => Double): Double = {
    val sum: Double = data.map(f).sum
    val lenth: Int = data.map(f).length
    val aver: Double = sum/lenth
    aver

  }

  def topK[T](data: List[T], f: T => Double, k: Int): List[T] = {
    if (data.length <= k){
      val reverselst = data.sortBy(f).reverse
      reverselst
    }
    else{
      data.sortBy(f).reverse.slice(0, k)
    }
  }

  def bayesianAverage[T](data: List[T], f: T => Double, fakenumber: Int, fakevalue: Int): Double = {
    val sum: Double = data.map(f).sum
    val lenth: Int = data.map(f).length
    val baver: Double = (sum + (fakevalue * fakenumber)) / (fakenumber + lenth)
    baver
  }


}
