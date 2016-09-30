import org.scalameter._

val time = measure {
  (0 until 1000000).toArray
}
println(s"Array initialization time: $time ")


val time2 = measure {
  (0 until 1000000).toArray
}
println(s"Array initialization time: $time2 ")

val time3 = measure {
  (0 until 1000000).toArray
}
println(s"Array initialization time: $time3 ")

val time4 = withWarmer(new Warmer.Default).measure {
  (0 until 1000000).toArray
}
println(s"Warmed up Array initialization time: $time4 ")

val time5 = config(Key.exec.minWarmupRuns -> 20,
                   Key.exec.maxWarmupRuns -> 60,
                   Key.verbose -> true).withWarmer(new Warmer.Default).measure {
  (0 until 1000000).toArray
}
println(s"Warmed up Array initialization time: $time5 ")


val mem = withWarmer(new Warmer.Default).withMeasurer(new Measurer.MemoryFootprint).measure {
  (0 until 1000000).toArray
}
println(s"Warmed up Array mem footprint: $mem ")
