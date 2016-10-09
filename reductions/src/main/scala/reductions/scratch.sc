import java.util

import com.sun.deploy.util.ArrayUtil
import reductions.LineOfSight._

val t = upsweep(Array[Float](0f, 1f, 8f, 9f), 0, 4, 1)
t.maxPrevious

val a1 = Array[Float](0f, 1f, 3f, 4f, 3f, 8f, 5f)
val o1 = new Array[Float](7)

//lineOfSight(a1, o1)
//println(util.Arrays.toString(o1))
//
//
//val t1 = upsweep(a1, 0, 7, 1)
//t1 match {
//  case Node(l, r) => r match {
//    case Node(_, _) => println("wrong")
//    case Leaf(_, _, m) => println(m)
//  }
//  case Leaf(_, _, m) => println(m)
//}


parLineOfSight(a1, o1, 2)
println(util.Arrays.toString(o1))


val a2 = Array[Float](0f, 1f, 8f, 9f)
val o2 = new Array[Float](4)

parLineOfSight(a2, o2, 2)
println(util.Arrays.toString(o2))
