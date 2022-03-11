object PerfectNumbers extends App {

  /**
   * Returns the sum of factors of number (excluding number itself)
   * @param number
   * @return sum
   */
  def aliquot(number: Int): Int = {
    var sum = 0
    for (i <- 1 to number - 1) {
      if (number % i == 0) sum = sum + i
    }
    sum
  }


  def classify(number: Int) :Either[String, NumberType.NumberType] = {
    if (number <= 0) Left("Classification is only possible for natural numbers.")
    else (number,aliquot(number)) match {
      case (x,y) if(x==y) => Right(NumberType.Perfect)
      case (x,y) if (x>y) => Right(NumberType.Deficient)
      case (x,y) if (x<y) => Right(NumberType.Abundant)
    }
  }
}
object NumberType extends Enumeration {
  type NumberType = Value
  val Perfect, Deficient, Abundant = Value
}