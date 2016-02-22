/**
  * Created by konrad on 2/22/16.
  */
import fpinscala.datastructures.{List,Cons,Nil}


object Testing {
  def main(args: Array[String]) {
    val testList:List[Double] = List(2,2,3,4,5,6,7,8,9)

    println(List.dropWhile(testList,(x:Double)=>x%2==0))

  }
}
