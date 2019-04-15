
sealed trait Tree[+A]

case object Nill extends Tree[Nothing]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], value: A, right: Tree[A]) extends Tree[A]

object Tree extends App {

    val tree1 = Branch(left = Leaf(10), value = 15, right = Leaf(20))
    /*
         15
        /  \
       10  20
     */

    val tree2 = Branch(left = Nill, value = 50, right = Leaf(40))
    /*
        50
          \
          40

     */

    val tree3 = Branch(left = tree1, value = 36, right = tree2)

    /*
            36
           /  \
         15   50
        /  \    \
       10  20   40


     */

    def size[A](t: Tree[A]): Int = t match {
        case Nill => 0
        case Leaf(_) => 1                   // Traversing generic scala
        case Branch(l, _, r) => 1 + size(l) + size(r)

    }

    println(size(tree1))
    println(size(tree2))
    println(size(tree3))

    def maximum(t: Tree[Int]): Int = t match {
        case Nill => 0
        case Leaf(n) => n
        case Branch(l, v, r) => maximum(l) max maximum(r) max v
    }

    println(maximum(tree1))
    println(maximum(tree2))
    println(maximum(tree3))

    def depth[A](t: Tree[A]): Int = t match {
        case Nill => 0
        case Leaf(_) => 0
        case Branch(l, _, r) => 1 + (depth(l) max depth(r))
    }

    println(depth(tree1))
    println(depth(tree2))
    println(depth(tree3))

    def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
        case Nill => Nill
        case Leaf(a) => Leaf(f(a))
        case Branch(l, v, r) => Branch(map(l)(f), f(v), map(r)(f))
    }

    val double = (x: Int) => x * 2
    println(map(tree1)(double))
    println(map(tree2)(double))
    println(map(tree3)(double))


    //
    //    def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    //        case Leaf(a) => f(a)
    //        case Branch(l, _, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    //    }
    //
    //    def sizeViaFold[A](t: Tree[A]): Int =
    //        fold(t)(a => 1)(1 + _ + _)
    //
    //    def maximumViaFold(t: Tree[Int]): Int =
    //        fold(t)(a => a)(_ max _)
    //
    //    def depthViaFold[A](t: Tree[A]): Int =
    //        fold(t)(a => 0)((d1, d2) => 1 + (d1 max d2))
    //
    //    def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] =
    //        fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _, _))
}


