
object AnonymousFunctions extends App {

    val doubler = (x: Int) => x * 2
    val tripler: Int => Int = x => x * 3
    val adder: (Int, Int) => Int = (x, y) => x + y

    val noParamsLamda: () => Int = () => 3
    println(noParamsLamda)
    println(noParamsLamda())

    val newAdder: (Int, Int) => Int = _ + _
    println(newAdder(4, 5))


    val superAdder: Int => Int => Int = (x: Int) => (y: Int) => x + y

    val add2 = superAdder(2)
    println(add2(4))
}


