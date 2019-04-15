

object Laziness extends App {
    def callByValue(time: Long): Unit = {
        println(f"Time now -  $time")
        println(f"Time now -  $time")
    }

    def callByName(time: => Long): Unit = {
        println(f"Time now - $time")
        println(f"Time now - $time")
    }

    println("Call by value ")
    println(callByValue(System.nanoTime()))
    println("Call by Name ")
    println(callByName(System.nanoTime()))

    def stackOverflowFunc(): Int = 1 + stackOverflowFunc()

    def greet(): Unit = println("Hello, World!")

    /* Unused stackoverflow func */
    def doSomething(x: => Unit, fun: => Int): Unit = {
        x
    }

    doSomething(greet(), stackOverflowFunc())
}

