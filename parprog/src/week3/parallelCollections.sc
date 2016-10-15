import scala.collection.concurrent.TrieMap
import scala.collection.{GenSeq, GenSet, mutable}

def initArray(xs: Array[Int])(v: Int) = {
  for (i <- (0 until xs.length).par) {
    xs(i) = v
  }
}

(1 until 1000).par.filter(n => n%3 == 0).count(n => n.toString == n.toString.reverse)

//def color(i: Int) = ???
//def coordinatesFor(idx: Int) = ???
//
////Mandelbrot set Zn+1= Z^2n+ c
//def computePixel(xc: Double, yc: Double, maxIterations: Int): Int = {
//  var i = 0
//  var x, y = 0.0
//  while (x * x + y * y < 4 && i < maxIterations) {
//    val xt = x * x - y * y + xc
//    val yt = 2 * x * y + yc
//    x = xt;
//    y = yt
//    i += 1
//  }
//  color(i)
//}
//
//def parRender(image: Array[Int], maxIterations: Int): Unit = {
//  for (idx <- (0 until image.length).par) {
//    val (xc, yc) = coordinatesFor(idx)
//    image(idx) = computePixel(xc, yc, maxIterations)
//  }
//}

//Not parallelizable: reduceLeft,-right, foldLeft,-right, scanLeft,-right
def sum(xs: Array[Int]): Int = {
  xs.par.foldLeft(0)(_ + _)
}

//we need a definition of fold on which we can create a reduction tree
//def fold[U](z: U)(op: (U, U) => U)

def max(xs: Array[Int]): Int = {
  xs.par.fold(Int.MinValue)(math.max)
}

//Parallel fold relies on the following relations to hold:
//Associativity: f(a, f(b, c)) == f(f(a, b), c)
//Neutral element: f(z, a) == f(a, z) == a

def isVowel(c: Char) = c match {
  case 'A' => true
  case 'E' => true
  case 'I' => true
  case 'O' => true
  case 'U' => true
  case _ => false
}

//Does not compile:
//Array('E', 'P', 'F', 'L').par
//    .fold(0)((count, c) => if (isVowel(c)) count + 1 else count)

//"aggregate" tries to workaround the type limitation of fold
//def aggregate[B](z: B)(f: (B, A) => B, g: (B, B) => B): B

Array('E', 'P', 'F', 'L').par
    .aggregate(0)((count, c) => if (isVowel(c)) count + 1 else count, _ + _)


//Parallelism-Agnostic code - GenSeq
def largestPalindrome(xs: GenSeq[Int]): Int = {
  xs.aggregate(Int.MinValue)((largest, n) => if (n > largest && n.toString == n.toString.reverse) n else largest, math.max)
}
val array = (0 until 1000000).toArray
largestPalindrome(array)
largestPalindrome(array.par)


//Avoid mutations of same memory locations without proper synchronization:
def intersection(a: GenSet[Int], b: GenSet[Int]) = {
  val result = mutable.Set[Int]()
  for (x <- a) if (b contains x) result += x
  result
}

intersection((0 until 1000).toSet, (0 until 1000 by 4).toSet)
intersection((0 until 1000).par.toSet, (0 until 1000 by 4).par.toSet) //Does not work always

//Avoid side effects
def intersectionFix(a: GenSet[Int], b: GenSet[Int]) = {
  if (a.size < b.size) a.filter(b(_)) //or a.filter(n => b.contains(n)) or a.intersects(b)
  else b.filter(a(_))
}

intersectionFix((0 until 1000).toSet, (0 until 1000 by 4).toSet)
intersectionFix((0 until 1000).par.toSet, (0 until 1000 by 4).par.toSet) //Now works always


//Never modify a parallel collection on which a data-parallel operation is in progress
val graph = mutable.Map[Int, Int]() ++= (0 until 100000).map(i => (i, i + 1))
graph(graph.size - 1) = 0

//Here we are reading and modifying the collection we are traversing
for ((k, v) <- graph.par) graph(k) = graph(v)
val violation = graph.find({ case (i, v) => v != (i + 2) % graph.size })
println(s"violation: $violation")

//Triemap solves this via an atomic snapshot
val trieGraph = TrieMap[Int, Int]() ++= (0 until 100000).map(i => (i, i + 1))
trieGraph(trieGraph.size - 1) = 0

//Get the snapshot
val snapshot = trieGraph.snapshot()

for ((k, v) <- trieGraph.par) trieGraph(k) = snapshot(v)
val nonViolation = trieGraph.find({ case (i, v) => v != (i + 2) % trieGraph.size })
println(s"violation: $nonViolation")