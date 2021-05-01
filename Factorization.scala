import scala.math.sqrt

object Factorization extends App {
    var target = 24
    val maxDivisor = sqrt(target).toInt

    def facotrizationRec(num: Int, divisor: Int, acc: Map[Int, Int]): Map[Int, Int] = {
        if (divisor > maxDivisor) {
            if (num == 1) acc else acc + (num -> 1)
        } else if (num % divisor == 0) {
            val nextAcc = acc + (divisor -> (acc.getOrElse(divisor, 0) + 1))
            facotrizationRec(num/divisor, divisor, nextAcc)    
        } else {
            facotrizationRec(num, divisor + 1, acc)    
        }
    }

    println(facotrizationRec(target, 2, Map()))
}