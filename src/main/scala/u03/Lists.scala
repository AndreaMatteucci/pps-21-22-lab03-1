package u03

object Lists extends App :

  // A generic linkedlist
  enum List[E]:
    case Cons(head: E, tail: List[E])
    case Nil()
  // a companion object (i.e., module) for List


  object List:


    def sum(l: List[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _ => 0

    def map[A, B](l: List[A])(mapper: A => B): List[B] = l match
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      case Nil() => Nil()

    def filter[A](l1: List[A])(pred: A => Boolean): List[A] = l1 match
      case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_, t) => filter(t)(pred)
      case Nil() => Nil()

    //A
    def drop[E](l: List[E], n: Int): List[E] = l match
      case Cons(h, t) if n > 0 => drop(t, n - 1)
      case Cons(h, t) => Cons(h, t)
      case Nil() => Nil()

    //B
    def append[E](left: List[E], right: List[E]): List[E] = left match
      case Cons(h, t) if t != Nil() => Cons(h, append(t, right))
      case Cons(h, t) => Cons(h, right)

    //C
    def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = l match
      case Cons(h, t) => append(f(h), flatMap(t)(f))
      case Nil() => Nil()

    //D
    def map2[A, B](l: List[A])(mapper: A => B): List[B] =
      flatMap(l)((x) => Cons(mapper(x), Nil()))

    def filter2[A](l1: List[A])(pred: A => Boolean): List[A] =
      //flatMap(l1)(x => if pred(x) then Cons(x, Nil()) else Nil())
      val tmp = (x: A) => pred(x) match
        case true => Cons(x, Nil())
        case false => Nil()
      flatMap(l1)(tmp)

    //ES 2

    import u02.Optionals.*
    import u02.Optionals.Option.*

    def max(l: List[Int]): Option[Int] = l match
      case Cons(h, t) if (h == Option.orElse(max(t), h)) => Some(h)
      case Cons(h, t) => max(t)
      case Nil() => None()

    //ES 3

    import u02.AlgebraicDataTypes.*

    def listPersons(l: List[Person]): List[String] = l match
      case Cons(Person.Teacher(name, course), t) => Cons(course, listPersons(t))
      case Cons(Person.Student(name, course), t) => listPersons(t)
      case _ => Nil()

    def listPersonWithFlatMap(l: List[Person]): List[String] =
      val tmp = (x: Person) => x match
        case Person.Teacher(name, course) => Cons(course, Nil())
        case Person.Student(_, _) => Nil()
        case _ => Nil()
      flatMap(l)(tmp)


  val l = List.Cons(10, List.Cons(20, List.Cons(30, List.Nil())))
  println(List.sum(l)) // 60


  import List.*
  import u03.Lists.List
  import u03.Lists.List.{Cons, Nil, flatMap}


  println(sum(map(filter(l)(_ >= 20))(_ + 1)))
