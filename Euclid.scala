object Euclid extends App {

 def greatestCommonDivisor(a: Int, b: Int): Int = {
     if (a == 0) {
         return b;
     } else if (b == 0) {
         return a;
     } else {
         return greatestCommonDivisor(b, a % b);
     }
 }   
}