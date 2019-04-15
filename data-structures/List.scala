
sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List extends App {
    def sum(ints: List[Int]): Int = ints match {
        case Nil => 0
        case Cons(x, xs) => x + sum(xs)
    }

    println("Sum - " + sum(List(1, 2, 3, 4, 5)))

    def product(ds: List[Double]): Double = ds match {
        case Nil => 1.0
        case Cons(0.0, _) => 0.0
        case Cons(x, xs) => x * product(xs)
    }

    println("Product - " + sum(List(1, 2, 3, 4, 5)))

    def apply[A](as: A*): List[A] = // Variadic function syntax
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))

    //Exercise 1
    val x = List(1, 2, 3, 4, 5) match {
        case Cons(x, Cons(2, Cons(4, _))) => x
        case Nil => 42
        case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
        case Cons(h, t) => h + sum(t)
        case _ => 101
    }

    println(x) // Output will be x=1,y=2 so x + y = 3

    //Exercise 2 - Implement function tail for removing the first element fo the list
    def tail[A](l: List[A]): List[A] =
        l match {
            case Nil => sys.error("List is empty")
            case Cons(_, t) => t // Remove the first element of the cons(head,tail)
        }

    println(tail(List(1, 2, 3, 4, 5)))

    //Exercise 3 - Implement setHead to replace the head element of the list
    def setHead[A](l: List[A], h: A): List[A] = l match {
        case Nil => sys.error("List is empty")
        case Cons(_, t) => Cons(h, t)
    }

    println(setHead(List(-1, 2, 3, 4, 5), 1))

    //Exercise 4 - Drop removes the first n elements of the list
    def drop[A](l: List[A], n: Int): List[A] =
        if (n <= 0) l
        else l match {
            case Nil => Nil
            case Cons(_, t) => drop(t, n - 1)
        }


    println(drop(List(1, 2, 3, 4, 5, 6), 2))

    //Exercise 5 - Dropwhile based on a condition
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
        l match {
            case Cons(h, t) if f(h) => dropWhile(t, f)
            case _ => l

        }

    println(dropWhile(List(1, 2, 3, 4, 5, 6), (x: Int) => x < 3))

    //Exercise 6 - Implement init that returns everything but last element of the list
    def init[A](l: List[A]): List[A] = {
        l match {
            case Nil => sys.error("List is empty")
            case Cons(_, Nil) => Nil // Terminate list just skipping the last element
            case Cons(h, t) => Cons(h, init(t))
        }
    }

    //Generic Function to perform an operation over the entire list taking 2 elements at a time and going right
    def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
        l match {
            case Nil => z
            case Cons(h, t) => f(h, foldRight(t, z)(f))

        }
    }

    println(foldRight(List(1, 2, 3, 4, 5), 0)((x, y) => x + y)) // Summation
    println(foldRight(List(1, 2, 3, 4, 5), 1)((x: Int, y: Int) => x * y)) // Product

    //What happens if we pass NIL and Cons(_,_) to foldRight
    println(foldRight(List(1, 2, 3, 4, 5), Nil: List[Int])(Cons(_, _)))

    // We will get the same list back since we will be replacing NIL with LIST[INT] and
    // Cons constructor will be replaced as f
    //Exercise 9
    def length[A](l: List[A]): Int =
        foldRight(l, 0)((_, count) => count + 1)

    println(length(List(1, 2, 3, 4, 5)))

    //Exercise 10

    @annotation.tailrec
    def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
        case Nil => z
        case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }

    //Exercise 11

    def sum3(l: List[Int]) = foldLeft(l, 0)(_ + _)

    def product3(l: List[Double]) = foldLeft(l, 1.0)(_ * _)

    def length2[A](l: List[A]): Int = foldLeft(l, 0)((acc, h) => acc + 1)

    // Exercise 12 - Reverse a list using fold

    def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc, h) => Cons(h, acc))


    def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B =
        foldLeft(reverse(l), z)((b, a) => f(a, b))

    def foldRightViaFoldLeft_1[A, B](l: List[A], z: B)(f: (A, B) => B): B =
        foldLeft(l, (b: B) => b)((g, a) => b => g(f(a, b)))(z)

    def foldLeftViaFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B =
        foldRight(l, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

    def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
        foldRight(l, r)(Cons(_, _))


    def concat[A](l: List[List[A]]): List[A] =
        foldRight(l, Nil: List[A])(appendViaFoldRight)

    def add1(l: List[Int]): List[Int] =
        foldRight(l, Nil: List[Int])((h, t) => Cons(h + 1, t))

    def doubleToString(l: List[Double]): List[String] =
        foldRight(l, Nil: List[String])((h, t) => Cons(h.toString, t))


    def map[A, B](l: List[A])(f: A => B): List[B] =
        foldRight(l, Nil: List[B])((h, t) => Cons(f(h), t))

    def map_1[A, B](l: List[A])(f: A => B): List[B] =
        foldRightViaFoldLeft(l, Nil: List[B])((h, t) => Cons(f(h), t))

    def map_2[A, B](l: List[A])(f: A => B): List[B] = {
        val buf = new collection.mutable.ListBuffer[B]

        def go(l: List[A]): Unit = l match {
            case Nil => ()
            case Cons(h, t) => buf += f(h); go(t)
        }

        go(l)
        List(buf.toList: _*) // converting from the standard Scala list to the list we've defined here
    }


    def filter[A](l: List[A])(f: A => Boolean): List[A] =
        foldRight(l, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)

    def filter_1[A](l: List[A])(f: A => Boolean): List[A] =
        foldRightViaFoldLeft(l, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)

    def filter_2[A](l: List[A])(f: A => Boolean): List[A] = {
        val buf = new collection.mutable.ListBuffer[A]

        def go(l: List[A]): Unit = l match {
            case Nil => ()
            case Cons(h, t) => if (f(h)) buf += h; go(t)
        }

        go(l)
        List(buf.toList: _*) // converting from the standard Scala list to the list we've defined here
    }


    def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
        concat(map(l)(f))

    def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
        flatMap(l)(a => if (f(a)) List(a) else Nil)


    def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
        case (Nil, _) => Nil
        case (_, Nil) => Nil
        case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairwise(t1, t2))
    }


    def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = (a, b) match {
        case (Nil, _) => Nil
        case (_, Nil) => Nil
        case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    }


    @annotation.tailrec
    def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l, prefix) match {
        case (_, Nil) => true
        case (Cons(h, t), Cons(h2, t2)) if h == h2 => startsWith(t, t2)
        case _ => false
    }

    @annotation.tailrec
    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
        case Nil => sub == Nil
        case _ if startsWith(sup, sub) => true
        case Cons(h, t) => hasSubsequence(t, sub)
    }

}

