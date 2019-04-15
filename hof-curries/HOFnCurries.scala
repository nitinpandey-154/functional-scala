
import scala.annotation.tailrec

object HOFsAndCurries extends App {

    /*
        Write a function that applies a function f n times to a subject x.
        nTimes(f,n,x)
        nTimes(f,3,x) = f(f(f(x)))
     */

    @tailrec
    def nTimes(f: Int => Int, n: Int, x: Int): Int = {
        if (n == 0) x
        else {
            nTimes(f, n - 1, f(x))
        }
    }

    val square = (x: Int) => x * x
    val cube = nTimes(square, 1, 5)
    println(cube)

    val plusOne = (x: Int) => x + 1
    println(nTimes(plusOne, 10, 1))

    /* Better Implementation */

    def nTimesBetter(f: Int => Int, n: Int): Int => Int = {
        if (n == 0)
            (x: Int) => x
        else
            (x: Int) => nTimesBetter(f, n - 1)(f(x))

    }


}


