class GParent
class Parent extends GParent
class Child extends Parent

class Box[+A] {
  // trait Function1[-T, +R] = {
  //   def apply(t: T): R = ???
  // }
  // Function1[GParent, Child] <: Function1[Parent, Parent]
  def set[B >: A](x: B): Box[B] = ???
}

class Box2[-A]

def foo(x: Box[Parent]): Box[Parent] = identity(x)
def bar(x: Box2[Parent]): Box2[Parent] = identity(x)

foo(new Box[Parent])
foo(new Box[Child])
bar(new Box2[Parent])
bar(new Box2[GParent])

//new Box[Parent] <: new Box[Child]









//import scala.annotation.tailrec

//sealed trait List[+A]
//case object Nil extends List[Nothing]
//case class Cons[+A](h: A, t: List[A]) extends List[A]
//
//object List {
//  def map[A, B](l: List[A])(f: A => B): List[B] = {
//    @tailrec
//    def loop(acc: List[B], l: List[A]): List[B] = l match {
//      case Nil => Nil
//      case Cons(h, Nil) => Cons(f(h), acc)
//      case Cons(h, t) => loop(Cons(f(h), acc), t) //Cons(f(h), loop(t)(f))
//    }
//    loop(Nil, l)
//  }
//}
//
//List.map(Cons("a", Cons("b", Cons("c", Nil)))) { a =>
//  a + "z"
//}

//abstract class TRUE
//type Preparation = String
//type Glass = String
//type Status = String
//
//class ScotchBuilder[HB, HM, HD](
//    val theBrand:Option[String],
//    val theMode:Option[Preparation],
//    val theDoubleStatus:Option[Boolean],
//    val theGlass:Option[Glass]) {
//  def withBrand(b:String): ScotchBuilder[TRUE, HM, HD] = ???
////      new ScotchBuilder[TRUE, HM, HD](Some(b), theMode, theDoubleStatus, theGlass)
//
//  def withMode(p:Preparation): ScotchBuilder[HB, TRUE, HD] = ???
////    new ScotchBuilder[HB, TRUE, HD](theBrand, Some(p), theDoubleStatus, theGlass)
//
//  def isDouble(b:Boolean): ScotchBuilder[HB, HM, TRUE] = ???
////    new ScotchBuilder[HB, HM, TRUE](theBrand, theMode, Some(b), theGlass)
//
//  def withGlass(g:Glass): ScotchBuilder[HB, HM, HD] = ???
////    new ScotchBuilder[HB, HM, HD](theBrand, theMode, theDoubleStatus, Some(g))
//}
//
//
//implicit def enableBuild(builder:ScotchBuilder[TRUE, TRUE, TRUE]) = new {
////implicit def enableBuild(builder:ScotchBuilder[Int, Int, Int]) = new {
//  def build(): Boolean = ???
////    new OrderOfScotch(builder.theBrand.get, builder.theMode.get, builder.theDoubleStatus.get, builder.theGlass);
//}
//
//
//new ScotchBuilder[String, String, String](None, None, None, None).withBrand("test").withMode("test").isDouble(true).build()
//new ScotchBuilder[Int, Int, Int](None, None, None, None)//.build()
//new ScotchBuilder[String, String, String](None, None, None, None)//.build()
