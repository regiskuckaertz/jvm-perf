package net.degoes

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import java.util.concurrent.TimeUnit
import zio.Chunk

@State(Scope.Thread)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@BenchmarkMode(Array(Mode.Throughput))
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(1)
@Threads(1)
class GroupBy {
  @Param(Array("100", "1000", "100000"))
  var size: Int = _

  var persons: Chunk[Person] = _

  val reduce = GroupMapReduce(_.teamId)(_.age)((a1, a2) => (a1 + a2) / 2)

  @Setup def setup(): Unit = {
    persons = Chunk.tabulate(size)(i => Person(i % 10, 28, 85))
  }

  @Benchmark def groupBy(b: Blackhole): Unit = {
    persons.groupBy(_.teamId).map { case (teamId, ps) => ps.foldLeft(0d)((a, p) => (a + p.age) / 2) }
  }

  @Benchmark def groupMap(b: Blackhole): Unit = {
    persons.groupMapReduce(_.teamId)(_.age)((a1, a2) => (a1 + a2) / 2)
  }

  @Benchmark def groupMapSpecial(b: Blackhole): Unit = {
    reduce(persons)
  }
  
  case class Person(teamId: Long, age: Double, weight: Double)

  object GroupMapReduce {
    import java.util.function.DoubleBinaryOperator
    import java.util.function.ToDoubleFunction
    import java.util.function.ToLongFunction

    def apply(k: ToLongFunction[Person])(v: ToDoubleFunction[Person])(p: DoubleBinaryOperator): Chunk[Person] => scala.collection.immutable.Map[Long, Double] = xs => { 
      var i = 0
      val l = xs.length
      val result = scala.collection.mutable.LongMap.empty[Double]
      while (i < l) {
        val x = xs(i)
        val key = k.applyAsLong(x)
        val value = v.applyAsDouble(x)
        if (result.contains(key))
          result.update(key, p.applyAsDouble(result(key), value))
        else
          result.update(key, value)
        i = i + 1
      }

      result.to(scala.collection.immutable.Map)
    }
      
  }
}