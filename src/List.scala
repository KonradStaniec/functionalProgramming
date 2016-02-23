package fpinscala.datastructures

import com.sun.xml.internal.bind.v2.TODO
import com.sun.xml.internal.fastinfoset.tools.FI_SAX_Or_XML_SAX_SAXEvent

/**
  * Created by konrad on 2/22/16.
  * Chapter 3
  */

sealed trait List[+A]

case object Nil extends List[Nothing]
case class Cons[+A](head:A,tail:List[A]) extends List[A]

object List {

  def sum(ints:List[Int]):Int = ints match {
    case Nil => 0;
    case Cons(x,xs) => x + sum(xs)
  }

  def product(doubles:List[Double]):Double = doubles match {
    case Nil => 1.0
    case Cons(0.0,_) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as:A*):List[A] = {
    if(as.isEmpty) Nil
    else Cons(as.head,apply(as.tail:_*))
  }

  //EXERCISE 3.2 - Removing 1 element of list
  def tail[A](ls:List[A]):List[A] = ls match {
    case Nil => sys.error("Empty List")
    case Cons(_,xs) => xs
  }
  //EXERCISE 3.3 - replacing first element of list
  def setHead[A](ls:List[A],x:A):List[A] = ls match {
    case Nil => sys.error("Empty List")
    case Cons(_,xs) => Cons(x,xs)
  }
  //EXERCISE 3.4 - generalization of tail
  def drop[A](ls:List[A],n:Int):List[A] = {
    if(n==0) ls
    else ls match {
      case Nil => Nil
      case Cons(_,_) => drop(tail(ls),n-1)
    }
  }

  //EXERCISE 3.5 - droping elements matching predicate
  def dropWhile[A](ls:List[A],f:A => Boolean):List[A] = {
      ls match {
        case Cons(x,xs) if(f(x)) => dropWhile(xs,f)
        case _ => ls
      }

  }
  //EXERCISE 3.6 - droping last element of list
  def init[A](ls:List[A]):List[A] = ls match {
    case Cons(_,Nil) => Nil
    case Cons(x,xs) => Cons(x,init(xs))
  }


  def foldRight[A,B](ls:List[A],z:B)(f:(A,B) => B) :B =
    ls match {
      case Nil => z
      case Cons(x,xs) => f(x,foldRight(xs,z)(f))
    }

  def sum2(ls:List[Int]):Int =
    foldRight(ls,0)(_+_)
  //EXERCISE 3.9 - length of list by foldright
  def length[A](ls:List[A]):Int =
    foldRight(ls,0)((_,y)=>y+1)
  //EXERCISE 3.10 - tail recursive fold left
  @annotation.tailrec
  def foldLeft[A,B](ls:List[A],z:B)(f:(B,A)=>B):B =
    ls match {
      case Nil => z
      case Cons(x,xs) => foldLeft(xs,f(z,x))(f)

    }

  def sum3(ls:List[Int]):Int =.13
    foldLeft(ls,0)(_+_)

  def product3(ls:List[Int]):Int =
    foldLeft(ls,1)(_*_)

  def reverse[A](ls:List[A]):List[A] =
    foldLeft(ls,List[A]())((acc,h)=>Cons(h,acc))


}
