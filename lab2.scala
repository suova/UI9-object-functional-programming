import help.nominals

object help {
  val nominals = List(5000,2000,1000,500,100,50,10,5, 2,1)//возможные номиналы
  def divide(money: Int)={
    val origin: (Int, List[(Int, Int)]) = (money,Nil)
    val list = nominals.foldLeft(origin) ((p, n) => {
      val (x,list) = p
      (x%n, (n,x/n)::list)
    })
    list._2.toMap
  }
}

class Change private (ax: Map[Int,Int]) {
  val money: Map[Int, Int] = ax
  def this(amount:Int) = this(help.divide(amount))
  def count(n: Int): Map[Int,Int] = Map(n-> money(n))

  def sum()= nominals.foldLeft(0)((sum, n)=> sum+n*money(n))
  
  def + (v: Change) = new Change(sum()+v.sum())
  def - (v: Change) = new Change(sum()-v.sum())
}

object Lab2 {
  def main(args: Array[String]) {
   val vector = new Change(8669)
   val vector1 = new Change(1235)
   println(vector.money)
   println((vector-vector1).money)
  }
}
