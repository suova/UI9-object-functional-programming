object stringBuilder {
  def stringSum(i: Int, a: String, d: String, count: Int): String={
    if(i == count)
      ""
    else
      a + d * count + stringSum(i, a, d, count + 1)
  }
}

abstract class customArithProgression[T] {
  def iMember(i: Int, a: T, d: T): T
  def sum(n: Int, a: T, d: T): T
}

object customArithProgression{
  implicit object FloatArithProgression extends customArithProgression[Float] {
    override def iMember(i: Int, a:Float, d:Float): Float =  a + (i-1) * d
    override def sum(n: Int, a: Float, d: Float): Float = ((2 * a + (n-1) * d) / 2) * n
  }

  implicit object DoubleIArithProgression extends customArithProgression[Double] {
    override def iMember(i: Int, a: Double, d: Double): Double = a + (i-1) * d
    override def sum(n: Int, a: Double, d: Double): Double = ((2 * a + (n-1) * d) / 2) * n
  }

  implicit object IntArithProgression extends customArithProgression[Int] {
    override def iMember(i: Int, a: Int, d: Int): Int = a + (i-1) * d
    override def sum(n: Int, a: Int, d: Int): Int = ((2 * a + (n-1) * d) / 2) * n
  }

  implicit object LongArithProgression extends customArithProgression[Long] {
    override def iMember(i: Int, a: Long, d: Long): Long = a + (i-1) * d
    override def sum(n: Int, a: Long, d: Long): Long = ((2 * a + (n-1) * d) / 2) * n
  }

  implicit object StringArithProgression extends customArithProgression[String] {
    override def iMember(i: Int, a: String, d: String): String = a + d * (i-1)
    override def sum(n: Int, a: String, d: String): String = stringBuilder.stringSum(n, a, d, 0)
  }
}

class ArithProgression[T](val a: T, val d: T) {
  def sum (i: Int)(implicit arith : customArithProgression[T]): T = arith.sum(i, a, d)
  def iMember (i: Int)(implicit arith : customArithProgression[T]): T = arith.iMember(i, a, d)
}

object Main {
  def main(args: Array[String]): Unit = {

    val res1 = new ArithProgression[String]("a","b")
    println("[string] sum of 3: " + res1.sum(3))
    println("[string] 3 member: " + res1.iMember(3))
    println("\n")
    val res2 = new ArithProgression[Double](1.5,0.5)
    println("[double] sum of 4: " + res2.sum(4))
    println("[double] 4 member: " + res2.iMember(4))
    println("\n")
    val res3 = new ArithProgression[Int](5,3)
    println("[int] sum of 5: " + res3.sum(5))
    println("[int] 5 member: " + res3.iMember(5))
  }
}
