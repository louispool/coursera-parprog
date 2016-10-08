//Rational numbers
def g(u: Int, v: Int): Int = (u + v) / (1 + u * v)
def errInt(lst: List[Int]): Int = lst.reduceLeft(g) - lst.reduceRight(g)

def testAssocInt: Int = {
  val r = new scala.util.Random
  val lst = List.fill(400)((r.nextDouble * 0.002).toInt)
  errInt(lst)
}

testAssocInt //Associative


//Floating point numbers
def f(u: Double, v: Double): Double =  (u + v) / (1.0 + u * v)
def errFloat(lst: List[Double]): Double = lst.reduceLeft(f) - lst.reduceRight(f)

def testAssocFloat: Double = {
  val r = new scala.util.Random
  val lst = List.fill(400)(r.nextDouble * 0.002)
  errFloat(lst)
}

testAssocFloat //Not associative

