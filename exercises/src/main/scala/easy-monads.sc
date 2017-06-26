sealed trait Result[E, A]
case class Error[E, A](error: E) extends Result[E, A]
case class Success[E, A](value: A) extends Result[E, A]

val e: Error[String, Nothing] = Error("test")
val ebool: Error[Boolean, Nothing] = Error(true)

trait MyBadTeaching
class MyConfusedStudents extends MyBadTeaching
class MyDemotion extends MyBadTeaching

class Container5[-A <: MyBadTeaching](value: A)

sealed trait ConsoleIO[A] {
  def map[B](f: A => B): ConsoleIO[B] = ??? //Map(this, f)
}

def myfunc(a: String): Any
def myfunc2(a: AnyRef): Any

val listStr: List[String] = List.empty[String]
val listInt = List.empty[Integer]
val listAny: List[Any] = listStr 

val cont0: Container5[MyDemotion] = new Container5(new MyDemotion)
val cont1: Container5[MyConfusedStudents] = new Container5(new MyConfusedStudents)


trait MyTrait[A] {
  def mymethod: A
}
class Container[A <: String](value: A)
class Container2[A >: String](value: A)
class Container3[A : MyTrait](value: A)
class Container4(value: String)


//implicit def myconv[Integer](t: MyTraitImpl): MyTrait[Integer] = {
//  new MyTrait[Integer] { def mymethod = new Integer(0) }
//}
//
//class MyTraitImpl {
//}
//
//new Container3(new MyTraitImpl)

//val c0 = new Container[Any](0)
val c1 = new Container[String]("")

val list0: List[Any] = List.empty[Any]
val list3 = "String" +: list0

val list1: List[String] = List.empty[String]
val list2: List[Boolean] = List.empty[Boolean]

//class ErrorStringNothing[E, A] extends Result[E, A]
//class ErrorBooleanNothing[E, A] extends Result[E, A]

def chain[E, A, B](
  result: Result[E, A])(f: A => Result[E, B]): Result[E, B] = 
    result match {
      case Error(e) => Error(e)
      case Success(a) => f(a)
    }