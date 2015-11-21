package fpinscala.errorhandling

import utest._

import scala.util.Try

object OptionTest extends TestSuite {
  val tests = TestSuite {
    'map {
      assert(Some("test").map(_ + "test") == Some("testtest"))
      assert(None.map("test") == None)
    }

    'getOrElse {
      assert(Some("test").getOrElse("test2") == "test")
      assert(None.getOrElse("test2") == "test2")
    }

    'flatMap {
      assert(Some("test").flatMap(s => Some(s + "test2")) == Some("testtest2"))
      assert(None.flatMap(_ => Some("test2")) == None)
    }

    'orElse {
      assert(None.orElse(Some("test")) == Some("test"))
      assert(Some("testtest").orElse(Some("test")) == Some("testtest"))
    }

    'filter {
      assert(Some(1).filter(_ == 1) == Some(1))
      assert(Some(1).filter(_ == 2) == None)
      assert(None.filter(_ == 1) == None)
    }

    'variance {
      assert(Option.variance(Seq.empty[Double]) == None)
      assert(Option.variance(Seq(10d)) != None)
      assert(Option.variance(Seq(10d)) == Some(Math.pow(10d - 10d, 2)))
    }

    'map2 {
      assert(Option.map2(Some(1), Some(1))(_ + _) == Some(2))
      assert(Option.map2(None: Option[Int], Some(1))(_ + _) == None)
      assert(Option.map2(Some(1), None)(_ + _) == None)
      assert(Option.map2(None: Option[Int], None: Option[Int])(_ + _) == None)
    }

    'sequence {
      assert(Option.sequence(List(Some("a"))) == Some(List("a")))
      assert(Option.sequence(List(Some("a"), Some("b"), Some("c"))) == Some(List("a", "b", "c")))
      assert(Option.sequence(List(Some("a"), None, Some("c"))) == None)
      assert(Option.sequence(List(None)) == None)
      assert(Option.sequence(List.empty[Option[String]]) == Some(List.empty[String]))
    }

    'traverse {
      assert(Option.traverse(List("1")) { s =>
        try { Some(s.toInt) } catch { case t: Throwable => None } } == Some(List(1)))
      assert(Option.traverse(List("1", "2", "3")) { s =>
          try { Some(s.toInt) } catch { case t: Throwable => None } } == Some(List(1, 2, 3)))
      assert(Option.traverse(List.empty[String]) { s =>
          try { Some(s.toInt) } catch { case t: Throwable => None } } == Some(List.empty[Int]))
      assert(Option.traverse(List("1", "a", "3")) { s =>
          try { Some(s.toInt) } catch { case t: Throwable => None } } == None)
    }
  }

}
