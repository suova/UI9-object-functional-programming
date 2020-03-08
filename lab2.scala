import scala.collection.mutable

object help {
  def divide(money: Int): scala.collection.mutable.Map[Int,Int]={
    var result = scala.collection.mutable.Map(1->0, 2->0,5->0,10->0,50->0,100->2,500->0,1000->0,2000->0,5000->0)
    var count = 0
    var m = money
    if(money>=5000){
      count = money/5000
      result(5000) = count
      m = m - 5000*count
    }
    if(m >=2000){
      count = m/2000
      result(2000) = count
      m = m - 2000*count
    }
    if(m >=1000){
      count = m/1000
      result(1000) = count
      m = m - 1000*count
    }
    if(m >=500){
      count = m/500
      result(500) = count
      m = m - 500*count
    }
    if(m >=100){
      count = m/100
      result(100) = count
      m = m - 100*count
    }
    if(m >=50){
      count = m/50
      result(50) = count
      m = m - 50*count
    }
    if(m >=10){
      count = m/10
      result(10) = count
      m = m - 10*count
    }
    if(m >=5){
      count = m/5
      result(5) = count
      m = m - 5*count
    }
    if(m >=2){
      count = m/2
      result(2) = count
      m = m - 2*count
    }
    if(m >=1){
      count = m/1
      result(1) = count
      m = m - 1*count
    }
    result
  }
}

class Custom(ax: scala.collection.mutable.Map[Int,Int]) {
  val money: mutable.Map[Int, Int] = ax
  def this(amount:Int){
    this(help.divide(amount))
  }
  def count(n: Int): Map[Int,Int]={
    Map(n-> money(n))
  }
  def + (v: Custom) = new Custom(plus(money, v.money))
  def - (v: Custom) = new Custom(minus(money, v.money))
  def plus(list1:mutable.Map[Int,Int], list2:mutable.Map[Int,Int]): mutable.Map[Int,Int]={
    var result = scala.collection.mutable.Map(1->0, 2->0,5->0,10->0,50->0,100->0,500->0,1000->0,2000->0,5000->0)
    for ((k,v) <- list1) result(k) =  list1(k)+list2(k)
    result
  }
  def minus(list1:mutable.Map[Int,Int], list2:mutable.Map[Int,Int]): mutable.Map[Int,Int]={
    var result = scala.collection.mutable.Map(1->0, 2->0,5->0,10->0,50->0,100->0,500->0,1000->0,2000->0,5000->0)
    for ((k,v) <- list1) result(k) =  list1(k)-list2(k)
    result
  }
}

//возможные номиналы: [1,5,10,50,100,500,1000,5000]

object Lab2 {
  def main(args: Array[String]) {
//    val a = scala.collection.mutable.Map(1->9)
//    val b = scala.collection.mutable.Map(1->0, 2->0,5->0,10->0,50->0,100->2,500->0,1000->0,2000->0,5000->0)
    val vector = new Custom(8669)
//    val vector1 = new Custom(b)
    println(vector.money)
//    println((vector+vector1).money)
  }
}
