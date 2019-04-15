

object Options extends App {
    val myFirstOption: Option[Int] = Some(4)
    val noOption: Option[Int] = None

    println(myFirstOption)
    println(noOption)


    /* Don't do this*/

    //unsafe method
    def unsafeMethod(): String = null

    val result = Some(unsafeMethod()) //wrong
    // println(result) // Never have some take null

    def safeMethod(): String = "I am super safe"

    val result1 = Option(unsafeMethod())
    val result2 = Option(safeMethod())

    println(result1)
    println(result2)

    // Backup Method Calls
    val chainedResult = Option(unsafeMethod()).orElse(Option(safeMethod()))
    println(chainedResult)

    //Better Unsafe Methods
    def betterUnsafeMethod(): Option[String] = None

    def betterBackupMethod(): Option[String] = Some("Hey, I am better now")

    println(betterUnsafeMethod())
    println(betterBackupMethod())

    val betterResult = betterUnsafeMethod() orElse betterBackupMethod()
    println(betterResult)
}


