

import scala.annotation.tailrec

object TailRecursion extends App {


    //Tail recursive function to concatenate string n times
    def repeatString(str: String, n: Int): String = {
        @tailrec
        def helper(count: Int, result: String): String = {
            if (count == 1)
                result
            else {
                helper(count - 1, result + str)
            }
        }

        helper(n, str)
    }

    //Tail recursive isPrime function
    def isPrime(num: Int): Boolean = {
        @tailrec
        def helper(divisor: Int, status: Boolean): Boolean = {
            if (status) {
                if (divisor <= 1)
                    true
                else {
                    helper(divisor - 1, status && num % divisor != 0)
                }

            }
            else
                false


        }

        helper(num / 2, true)
    }

    //Tail recursive Fibonacci
    def fibonacci(num: Int): Int = {
        @tailrec
        def helper(n: Int, prev: Int, curr: Int): Int = {
            if (n <= 2) {
                curr
            }
            else {
                helper(n - 1, curr, prev + curr)
            }
        }

        helper(num, 1, 1)
    }

    println(repeatString("yellow", 3))
    println(isPrime(4))
    println(fibonacci(5))
}


