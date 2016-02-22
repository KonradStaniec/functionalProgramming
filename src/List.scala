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
  def init[A](ls:List[A]):List[A] =





}
