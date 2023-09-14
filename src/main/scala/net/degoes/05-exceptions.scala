/**
 * EXCEPTIONS
 *
 * Exceptions can be a source of overhead in any case where they cease to be "exceptional" (i.e.
 * when they occur frequently and are expected to occur as part of the business logic).
 *
 * In this section, you will explore and isolate the overhead of exceptions.
 */
package net.degoes.exceptions

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import java.util.concurrent.TimeUnit

/**
 * EXERCISE 1
 *
 * Develop a benchmark to measure the overhead of throwing and catching `MyException` with a fixed
 * message. Compare this with the overhead of constructing a new `MyException` without throwing (or
 * catching) it. What can you conclude from this benchmark?
 */
@State(Scope.Thread)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@BenchmarkMode(Array(Mode.Throughput))
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(value = 1, jvmArgsAppend = Array())
@Threads(1)
class ThrowExceptionBenchmark {
  case class MyException(message: String) extends Exception(message)

  @Benchmark
  def throwCatchException(): Unit = 
    try
      throw MyException("Hoi")
    catch {
      case _: Throwable => ()
    }

  @Benchmark
  def constructException(blackhole: Blackhole): Unit = 
    blackhole.consume(MyException("Hoi"))
}

/**
 * EXERCISE 2
 *
 * Develop a benchmark to measure the overhead of throwing and catching the same exception. Compare
 * this with the overhead of throwing and catching new exceptions. What can you conclude from this
 * comparison, together with the previous exercise?
 */
@State(Scope.Thread)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@BenchmarkMode(Array(Mode.Throughput))
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(value = 1, jvmArgsAppend = Array())
@Threads(1)
class ThrowSameExceptionBenchmark {
  case class MyException(message: String) extends Exception(message)

  val exception = MyException("Hello")

  @Benchmark
  def throwCatchNewException(): Unit = try
    throw MyException("Hello")
  catch { case _: Throwable => () }

  @Benchmark
  def throwCatchSameException(): Unit = try
    throw exception
  catch { case _: Throwable => () }
}

/**
 * EXERCISE 3
 *
 * Develop a benchmark to measure the overhead of calling Exception#fillInStackTrace. What can you
 * conclude from this benchmark?
 */
@State(Scope.Thread)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@BenchmarkMode(Array(Mode.Throughput))
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(value = 1, jvmArgsAppend = Array())
@Threads(1)
class FillInStackTraceBenchmark {
  case class MyException(message: String) extends Exception(message)

  val exception = MyException("Hello")

  @Benchmark
  def fillInStackTrace(b: Blackhole): Unit =
    b.consume(exception.fillInStackTrace())

  @Benchmark
  def throwCatchNewException(): Unit = try
    throw MyException("Hello")
  catch { case _: Throwable => () }
}


/**
 * EXERCISE 4
 *
 * Develop a benchmark to measure the overhead of try/catch distinct from the overhead of 
 */
@State(Scope.Thread)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@BenchmarkMode(Array(Mode.Throughput))
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(value = 1, jvmArgsAppend = Array())
@Threads(1)
class TryCatchBenchmark {
  sealed trait B
  case object F extends Exception("false") with B
  case object T extends Exception("true") with B

  var bool = T

  @Benchmark
  def one(b: Blackhole) =
    try throw bool
    catch {
      case t: Throwable => ()
    }
    

  @Benchmark
  def two(b: Blackhole) =
    try 
      try throw bool
      catch {
        case t: Throwable => throw t
      }
    catch {
      case _: Throwable => ()
    }
}
