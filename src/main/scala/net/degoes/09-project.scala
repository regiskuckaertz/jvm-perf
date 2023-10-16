/**
 * GRADUATION PROJECT
 *
 * In this section, you will tie together everything you have learned in order to significantly
 * optimize the performance of JVM-based code.
 */
package net.degoes.project

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import java.util.concurrent.TimeUnit
import _root_.jdk.incubator.vector.{ DoubleVector, LongVector, Vector, VectorOperators }

import zio.Chunk
import scala.util.Random
import scala.collection.mutable.Stack
import scala.collection.immutable

object dataset1 {
  sealed trait Value
  object Value {
    final case class Text(value: String)    extends Value
    final case class Integer(value: Long)   extends Value
    final case class Decimal(value: Double) extends Value
    case object NA                          extends Value
  }

  final case class Field(name: String)

  final case class Row(map: Map[String, Value]) {
    def apply(field: Field): Value = map(field.name)
  }

  final case class Dataset(rows: Chunk[Row]) { self =>
    def apply(field: Field): Dataset =
      Dataset(
        rows.map(row =>
          if (row.map.contains(field.name)) Row(Map(field.name -> row(field)))
          else Row(Map())
        )
      )

    def *(that: Dataset): Dataset =
      self.binary(that, "*") {
        case (Value.Integer(left), Value.Integer(right)) => Value.Integer(left * right)
        case (Value.Integer(left), Value.Decimal(right)) => Value.Decimal(left * right)
        case (Value.Decimal(left), Value.Decimal(right)) => Value.Decimal(left * right)
        case (Value.Decimal(left), Value.Integer(right)) => Value.Decimal(left * right)
      }

    def +(that: Dataset): Dataset =
      self.binary(that, "+") {
        case (Value.Integer(left), Value.Integer(right)) => Value.Integer(left + right)
        case (Value.Integer(left), Value.Decimal(right)) => Value.Decimal(left + right)
        case (Value.Decimal(left), Value.Decimal(right)) => Value.Decimal(left + right)
        case (Value.Decimal(left), Value.Integer(right)) => Value.Decimal(left + right)
      }

    def -(that: Dataset): Dataset =
      self.binary(that, "-") {
        case (Value.Integer(left), Value.Integer(right)) => Value.Integer(left - right)
        case (Value.Integer(left), Value.Decimal(right)) => Value.Decimal(left - right)
        case (Value.Decimal(left), Value.Decimal(right)) => Value.Decimal(left - right)
        case (Value.Decimal(left), Value.Integer(right)) => Value.Decimal(left - right)
      }

    def /(that: Dataset): Dataset =
      self.binary(that, "/") {
        case (Value.Integer(left), Value.Integer(right)) => Value.Integer(left / right)
        case (Value.Integer(left), Value.Decimal(right)) => Value.Decimal(left / right)
        case (Value.Decimal(left), Value.Decimal(right)) => Value.Decimal(left / right)
        case (Value.Decimal(left), Value.Integer(right)) => Value.Decimal(left / right)
      }

    private def binary(that: Dataset, symbol: String)(
      f: PartialFunction[(Value, Value), Value]
    ): Dataset =
      Dataset(self.rows.zip(that.rows).map { tuple =>
        val (leftRow, rightRow) = tuple

        val map =
          for {
            left                   <- leftRow.map
            (leftName, leftValue)   = left
            right                  <- rightRow.map
            (rightName, rightValue) = right
          } yield s"($leftName $symbol $rightName)" ->
            ((leftValue, rightValue) match {
              case (left, right) if f.isDefinedAt((left, right)) => f((left, right))
              case (_, _)                                        => Value.NA
            })

        Row(map)
      })
  }
}

object dataset2 {
  type Field = dataset1.Field
  val Field = dataset1.Field
  type Value = dataset1.Value
  val Value = dataset1.Value

  final case class Dataset(private val data: Map[Field, Array[Value]]) { self =>
    def apply(field: Field): Dataset =
      Dataset(Map(field -> data.getOrElse(field, Array.empty)))

    def *(that: Dataset): Dataset =
      self.binary(that, "*") {
        case (Value.Integer(left), Value.Integer(right)) => Value.Integer(left * right)
        case (Value.Integer(left), Value.Decimal(right)) => Value.Decimal(left * right)
        case (Value.Decimal(left), Value.Decimal(right)) => Value.Decimal(left * right)
        case (Value.Decimal(left), Value.Integer(right)) => Value.Decimal(left * right)
        case (_, _)                                      => Value.NA
      }

    def +(that: Dataset): Dataset =
      self.binary(that, "+") {
        case (Value.Integer(left), Value.Integer(right)) => Value.Integer(left + right)
        case (Value.Integer(left), Value.Decimal(right)) => Value.Decimal(left + right)
        case (Value.Decimal(left), Value.Decimal(right)) => Value.Decimal(left + right)
        case (Value.Decimal(left), Value.Integer(right)) => Value.Decimal(left + right)
        case (_, _)                                      => Value.NA
      }

    def -(that: Dataset): Dataset =
      self.binary(that, "-") {
        case (Value.Integer(left), Value.Integer(right)) => Value.Integer(left - right)
        case (Value.Integer(left), Value.Decimal(right)) => Value.Decimal(left - right)
        case (Value.Decimal(left), Value.Decimal(right)) => Value.Decimal(left - right)
        case (Value.Decimal(left), Value.Integer(right)) => Value.Decimal(left - right)
        case (_, _)                                      => Value.NA
      }

    def /(that: Dataset): Dataset =
      self.binary(that, "/") {
        case (Value.Integer(left), Value.Integer(right)) => Value.Integer(left / right)
        case (Value.Integer(left), Value.Decimal(right)) => Value.Decimal(left / right)
        case (Value.Decimal(left), Value.Decimal(right)) => Value.Decimal(left / right)
        case (Value.Decimal(left), Value.Integer(right)) => Value.Decimal(left / right)
        case (_, _)                                      => Value.NA
      }

    private def binary(that: Dataset, symbol: String)(
      f: Function2[Value, Value, Value]
    ): Dataset =
      Dataset(for {
        left                    <- this.data
        (leftName, leftValues)   = left
        right                   <- that.data
        (rightName, rightValues) = right
      } yield Field(s"($leftName $symbol $rightName)") -> {
        var i   = 0
        var l   = leftValues.length
        var m   = rightValues.length
        var res = Array.fill[Value](l max m)(Value.NA)
        while (i < l && i < m) {
          res(i) = f(leftValues(i), rightValues(i))
          i = i + 1
        }
        res
      })
  }
}

object dataset3 {

  val intSpecies   = LongVector.SPECIES_PREFERRED
  val floatSpecies = DoubleVector.SPECIES_PREFERRED
  type Field = dataset1.Field
  val Field = dataset1.Field
  type Value = dataset1.Value
  val Value = dataset1.Value

  sealed trait Op[a] {
    import Op._

    def *[b, c](that: Op[b])(implicit mix: Mix.Aux[a, b, c]): Op[c] =
      Mul(this, that, mix)

    def /[b, c](that: Op[b])(implicit mix: Mix.Aux[a, b, c]): Op[c] =
      Div(this, that, mix)

    def +[b, c](that: Op[b])(implicit mix: Mix.Aux[a, b, c]): Op[c] =
      Add(this, that, mix)

    def -[b, c](that: Op[b])(implicit mix: Mix.Aux[a, b, c]): Op[c] =
      Sub(this, that, mix)
  }

  object Op {
    case class GetInt(field: Field)   extends Op[Long]
    case class GetFloat(field: Field) extends Op[Double]
    case class Add[a, b, c](left: Op[a], right: Op[b], mix: Mix[a, b] { type Out = c })
        extends Op[c]
    case class Sub[a, b, c](left: Op[a], right: Op[b], mix: Mix[a, b] { type Out = c })
        extends Op[c]
    case class Mul[a, b, c](left: Op[a], right: Op[b], mix: Mix[a, b] { type Out = c })
        extends Op[c]
    case class Div[a, b, c](left: Op[a], right: Op[b], mix: Mix[a, b] { type Out = c })
        extends Op[c]
  }

  sealed trait Mix[a, b] {
    type Out
    def combine(op: VectorOperators.Binary, xs: Array[a], ys: Array[b]): Array[Out]
  }

  object Mix {
    type Aux[a, b, c] = Mix[a, b] { type Out = c }

    implicit val mixII: Aux[Long, Long, Long] = new Mix[Long, Long] {
      type Out = Long
      def combine(op: VectorOperators.Binary, xs: Array[Long], ys: Array[Long]): Array[Long] = {
        val xsVec = LongVector.fromArray(intSpecies, xs, 0)
        val ysVec = LongVector.fromArray(intSpecies, ys, 0)
        xsVec.lanewise(op, ysVec).toArray()
      }
    }

    implicit val mixFF: Aux[Double, Double, Double] = new Mix[Double, Double] {
      type Out = Double
      def combine(
        op: VectorOperators.Binary,
        xs: Array[Double],
        ys: Array[Double]
      ): Array[Double] = {
        val xsVec = DoubleVector.fromArray(floatSpecies, xs, 0)
        val ysVec = DoubleVector.fromArray(floatSpecies, ys, 0)
        xsVec.lanewise(op, ysVec).toArray()
      }
    }

    implicit val mixIF: Aux[Long, Double, Double] = new Mix[Long, Double] {
      type Out = Double
      def combine(op: VectorOperators.Binary, xs: Array[Long], ys: Array[Double]): Array[Double] = {
        val xsVec =
          LongVector.fromArray(intSpecies, xs, 0).convertShape(VectorOperators.L2D, floatSpecies, 0)
        val ysVec =
          DoubleVector.fromArray(floatSpecies, ys, 0)
        xsVec.lanewise(op, ysVec).toDoubleArray()
      }
    }

    implicit val mixFI: Aux[Double, Long, Double] = new Mix[Double, Long] {
      type Out = Double
      def combine(op: VectorOperators.Binary, xs: Array[Double], ys: Array[Long]): Array[Double] = {
        val xsVec =
          DoubleVector.fromArray(floatSpecies, xs, 0)
        val ysVec =
          LongVector.fromArray(intSpecies, ys, 0).convertShape(VectorOperators.L2D, floatSpecies, 0)
        xsVec.lanewise(op, ysVec).toDoubleArray()
      }
    }
  }

  final case class Dataset(private val data: Map[Field, Array[Long]]) {
    def execute[a](op: Op[a]): Array[a] =
      op match {
        case Op.GetInt(field)   =>
          data(field).asInstanceOf[Array[a]]
        case Op.GetFloat(field) =>
          data(field).asInstanceOf[Array[a]]
        case x: Op.Mul[a, b, c] =>
          val left  = execute(x.left)
          val right = execute(x.right)
          x.mix.combine(VectorOperators.MUL, left, right)
        case x: Op.Add[a, b, c] =>
          val left: Array[a]  = execute(x.left)
          val right: Array[b] = execute(x.right)
          x.mix.combine(VectorOperators.ADD, left, right)
        case x: Op.Sub[a, b, c] =>
          val left  = execute(x.left)
          val right = execute(x.right)
          x.mix.combine(VectorOperators.SUB, left, right)
        case x: Op.Div[a, b, c] =>
          val left  = execute(x.left)
          val right = execute(x.right)
          x.mix.combine(VectorOperators.DIV, left, right)
      }
  }
}

object dataset4 {

  val intSpecies   = LongVector.SPECIES_PREFERRED
  val floatSpecies = DoubleVector.SPECIES_PREFERRED
  type Field = dataset1.Field
  val Field = dataset1.Field
  type Value = dataset1.Value
  val Value = dataset1.Value

  sealed trait ByteCode

  object ByteCode {
    case class Get(field: Field, reg: Register)              extends ByteCode
    case class Push(value: Register)                         extends ByteCode
    case class Pop(value: Register)                          extends ByteCode
    case object Swap                                         extends ByteCode
    case class Add(mix: Mix[Long, Long] { type Out = Long }) extends ByteCode
    case class Sub(mix: Mix[Long, Long] { type Out = Long }) extends ByteCode
    case class Mul(mix: Mix[Long, Long] { type Out = Long }) extends ByteCode
    case class Div(mix: Mix[Long, Long] { type Out = Long }) extends ByteCode
  }

  sealed trait Register
  object Register {
    case object R0 extends Register
    case object R1 extends Register
  }

  sealed trait Op[a] {
    import Op._

    private var compiled: Compiled = null

    def *[b, c](that: Op[b])(implicit mix: Mix.Aux[a, b, c]): Op[c] =
      Mul(this, that, mix)

    def /[b, c](that: Op[b])(implicit mix: Mix.Aux[a, b, c]): Op[c] =
      Div(this, that, mix)

    def +[b, c](that: Op[b])(implicit mix: Mix.Aux[a, b, c]): Op[c] =
      Add(this, that, mix)

    def -[b, c](that: Op[b])(implicit mix: Mix.Aux[a, b, c]): Op[c] =
      Sub(this, that, mix)

    private def optimise(bc: Chunk[ByteCode]): Chunk[ByteCode] = {
      def go(bc: List[ByteCode]): List[ByteCode] =
        bc match {
          case ByteCode.Push(Register.R0) ::
              ByteCode.Get(f2, Register.R0) ::
              ByteCode.Swap ::
              ByteCode.Pop(Register.R0) ::
              tail =>
            ByteCode.Get(f2, Register.R1) :: go(tail)
          case otherwise :: tail =>
            otherwise :: go(tail)
          case Nil               =>
            Nil
        }

      Chunk.from(go(bc.toList))
    }

    private def calculateStackSize(bc: Chunk[ByteCode]) =
      bc.foldLeft(0 -> 0) { case ((mss, css), bc) =>
        bc match {
          case ByteCode.Push(_) => (mss max (css + 1)) -> (css + 1)
          case ByteCode.Pop(_)  => mss                 -> (css - 1)
          case _                => mss                 -> css
        }
      }._1

    def compile: Compiled = synchronized {
      def loop[a](op: Op[a]): Chunk[ByteCode] =
        op match {
          case x: GetInt             =>
            Chunk(ByteCode.Get(x.field, Register.R0))
          case x: GetFloat           =>
            Chunk(ByteCode.Get(x.field, Register.R0))
          case Add(left, right, mix) =>
            loop(left) ++ Chunk(ByteCode.Push(Register.R0)) ++ loop(right) ++ Chunk(
              ByteCode.Swap,
              ByteCode.Pop(Register.R0),
              ByteCode.Add(mix.asInstanceOf[Mix[Long, Long] { type Out = Long }])
            )
          case Sub(left, right, mix) =>
            loop(left) ++ Chunk(ByteCode.Push(Register.R0)) ++ loop(right) ++ Chunk(
              ByteCode.Swap,
              ByteCode.Pop(Register.R0),
              ByteCode.Sub(mix.asInstanceOf[Mix[Long, Long] { type Out = Long }])
            )
          case Mul(left, right, mix) =>
            loop(left) ++ Chunk(ByteCode.Push(Register.R0)) ++ loop(right) ++ Chunk(
              ByteCode.Swap,
              ByteCode.Pop(Register.R0),
              ByteCode.Mul(mix.asInstanceOf[Mix[Long, Long] { type Out = Long }])
            )
          case Div(left, right, mix) =>
            loop(left) ++ Chunk(ByteCode.Push(Register.R0)) ++ loop(right) ++ Chunk(
              ByteCode.Swap,
              ByteCode.Pop(Register.R0),
              ByteCode.Div(mix.asInstanceOf[Mix[Long, Long] { type Out = Long }])
            )
        }

      if (compiled == null) {
        compiled = {
          val code = optimise(loop(this))
          val ss   = calculateStackSize(code)
          println("******")
          println(code)
          println("******")
          Compiled(code.toArray, ss)
        }
      }

      compiled
    }
  }

  case class Compiled(code: Array[ByteCode], stackSize: Int)

  object Op {
    case class GetInt(field: Field)   extends Op[Long]
    case class GetFloat(field: Field) extends Op[Double]
    case class Add[a, b, c](left: Op[a], right: Op[b], mix: Mix[a, b] { type Out = c })
        extends Op[c]
    case class Sub[a, b, c](left: Op[a], right: Op[b], mix: Mix[a, b] { type Out = c })
        extends Op[c]
    case class Mul[a, b, c](left: Op[a], right: Op[b], mix: Mix[a, b] { type Out = c })
        extends Op[c]
    case class Div[a, b, c](left: Op[a], right: Op[b], mix: Mix[a, b] { type Out = c })
        extends Op[c]
  }

  sealed trait Mix[a, b] {
    type Out
    def combine(op: VectorOperators.Binary, xs: Array[a], ys: Array[b]): Array[Out]
  }

  object Mix {
    type Aux[a, b, c] = Mix[a, b] { type Out = c }

    implicit val mixII: Aux[Long, Long, Long] = new Mix[Long, Long] {
      type Out = Long
      def combine(op: VectorOperators.Binary, xs: Array[Long], ys: Array[Long]): Array[Long] = {
        val xsVec = LongVector.fromArray(intSpecies, xs, 0)
        val ysVec = LongVector.fromArray(intSpecies, ys, 0)
        xsVec.lanewise(op, ysVec).toArray()
      }
    }

    implicit val mixFF: Aux[Double, Double, Double] = new Mix[Double, Double] {
      type Out = Double
      def combine(
        op: VectorOperators.Binary,
        xs: Array[Double],
        ys: Array[Double]
      ): Array[Double] = {
        val xsVec = DoubleVector.fromArray(floatSpecies, xs, 0)
        val ysVec = DoubleVector.fromArray(floatSpecies, ys, 0)
        xsVec.lanewise(op, ysVec).toArray()
      }
    }

    implicit val mixIF: Aux[Long, Double, Double] = new Mix[Long, Double] {
      type Out = Double
      def combine(op: VectorOperators.Binary, xs: Array[Long], ys: Array[Double]): Array[Double] = {
        val xsVec =
          LongVector.fromArray(intSpecies, xs, 0).convertShape(VectorOperators.L2D, floatSpecies, 0)
        val ysVec =
          DoubleVector.fromArray(floatSpecies, ys, 0)
        xsVec.lanewise(op, ysVec).toDoubleArray()
      }
    }

    implicit val mixFI: Aux[Double, Long, Double] = new Mix[Double, Long] {
      type Out = Double
      def combine(op: VectorOperators.Binary, xs: Array[Double], ys: Array[Long]): Array[Double] = {
        val xsVec =
          DoubleVector.fromArray(floatSpecies, xs, 0)
        val ysVec =
          LongVector.fromArray(intSpecies, ys, 0).convertShape(VectorOperators.L2D, floatSpecies, 0)
        xsVec.lanewise(op, ysVec).toDoubleArray()
      }
    }
  }

  final case class Dataset(private val data: Map[Field, Array[Long]]) {
    def execute[a](op: Op[a]): Array[a] = {
      val Compiled(byteCode, stackSize) = op.compile
      var i                             = 0
      var l                             = byteCode.length
      var r0: Array[Long]               = null
      var r1: Array[Long]               = null
      val stack: Array[Array[Long]]     = Array.ofDim(stackSize)
      var sp                            = 0

      while (i < l) {
        byteCode(i) match {
          case _: ByteCode.Swap.type =>
            val tmp = r0
            r0 = r1
            r1 = tmp
          case x: ByteCode.Push      =>
            stack(sp) = if (x.value == Register.R0) r0 else r1
            sp = sp + 1
          case x: ByteCode.Pop       =>
            sp = sp - 1
            if (x.value == Register.R0)
              r0 = stack(sp)
            else
              r1 = stack(sp)
          case x: ByteCode.Get       =>
            val field = x.field
            val reg   = x.reg
            if (reg == Register.R0)
              r0 = data(field)
            else
              r1 = data(field)
          case x: ByteCode.Add       =>
            val mix = x.mix
            r0 = mix.combine(VectorOperators.ADD, r0, r1)
          case x: ByteCode.Mul       =>
            val mix = x.mix
            r0 = mix.combine(VectorOperators.MUL, r0, r1)
          case x: ByteCode.Div       =>
            val mix = x.mix
            r0 = mix.combine(VectorOperators.DIV, r0, r1)
          case x: ByteCode.Sub       =>
            val mix = x.mix
            r0 = mix.combine(VectorOperators.SUB, r0, r1)
        }

        i = i + 1
      }

      r0.asInstanceOf[Array[a]]
    }
  }
}

/**
 * GRADUATION PROJECT
 *
 * Develop a version of `Dataset` that has a similar API, but which is at least 10x as fast. See how
 * far you can push it (can you get to 100x?).
 *
 * You may assume data is completely homogeneous and that no values are null. However, if ambitious,
 * you may solve the same problem under the assumption of undefined values and heterogeneous data.
 */
@State(Scope.Thread)
@OutputTimeUnit(TimeUnit.SECONDS)
@BenchmarkMode(Array(Mode.Throughput))
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(value = 1, jvmArgsAppend = Array("--add-modules=jdk.incubator.vector"))
@Threads(16)
class ProjectBenchmark {
  @Param(Array("100", "1000", "10000"))
  var size: Int = _

  object benchmark1 {
    import dataset1._

    var dataset: Dataset = _

    val start: Field  = Field("start")
    val end: Field    = Field("end")
    val netPay: Field = Field("netPay")
  }

  @Setup
  def setupSlow(): Unit = {
    import benchmark1._
    import dataset1._

    val rng: Random = new Random(0L)

    dataset = Dataset(Chunk.fill(size) {
      val start  = rng.between(0, 360)
      val end    = rng.between(start, 360)
      val netPay = rng.between(20000, 60000)

      Row(
        Map(
          "start"  -> Value.Integer(start),
          "end"    -> Value.Integer(end),
          "netPay" -> Value.Integer(netPay)
        )
      )
    })
  }

  @Benchmark
  def baseline(blackhole: Blackhole): Unit = {
    import benchmark1._
    import dataset1._

    val result = (dataset(start) + dataset(end)) / dataset(netPay)

    blackhole.consume(result)
  }

  object benchmark2 {
    import dataset2._

    var dataset: Dataset = _

    val start: Field  = Field("start")
    val end: Field    = Field("end")
    val netPay: Field = Field("netPay")
  }

  @Setup
  def setupChallenger1(): Unit = {
    import dataset2._
    import benchmark2._

    val rng: Random = new Random(0L)

    val starts = Array.fill[Long](size)(rng.between(0, 360))

    benchmark2.dataset = Dataset(
      Map(
        start  -> starts.map[Value](Value.Integer.apply),
        end    -> Array.tabulate[Value](size)(i => Value.Integer(rng.between(starts(i), 360))),
        netPay -> Array.fill(size)(Value.Integer(rng.between(20000, 60000)))
      )
    )
  }

  @Benchmark
  def challenger1(blackhole: Blackhole): Unit = {
    import benchmark2._
    import dataset2._

    val result = (dataset(start) + dataset(end)) / dataset(netPay)

    blackhole.consume(result)
  }

  object benchmark3 {
    import dataset3._

    var dataset: Dataset = _

    val start: Field  = Field("start")
    val end: Field    = Field("end")
    val netPay: Field = Field("netPay")

    val op: Op[Long] = (Op.GetInt(start) + Op.GetInt(end)) / Op.GetInt(netPay)
  }

  @Setup
  def setupChallenger2(): Unit = {
    import dataset3._
    import benchmark3._

    val rng: Random = new Random(0L)

    val starts = Array.fill[Long](size)(rng.between(0, 360))

    benchmark3.dataset = Dataset(
      Map(
        start  -> starts,
        end    -> Array.tabulate(size)(i => rng.between(starts(i), 360)),
        netPay -> Array.fill(size)(rng.between(20000, 60000))
      )
    )
  }

  @Benchmark
  def challenger2(blackhole: Blackhole): Unit = {
    import benchmark3._
    import dataset3._

    val result = dataset.execute(op)

    blackhole.consume(result)
  }

  object benchmark4 {
    import dataset4._

    var dataset: Dataset = _

    val start: Field  = Field("start")
    val end: Field    = Field("end")
    val netPay: Field = Field("netPay")

    val op: Op[Long] = (Op.GetInt(start) + Op.GetInt(end)) / Op.GetInt(netPay)
  }

  @Setup
  def setupChallenger3(): Unit = {
    import dataset4._
    import benchmark4._

    val rng: Random = new Random(0L)

    val starts = Array.fill[Long](size)(rng.between(0, 360))

    benchmark4.dataset = Dataset(
      Map(
        start  -> starts,
        end    -> Array.tabulate(size)(i => rng.between(starts(i), 360)),
        netPay -> Array.fill(size)(rng.between(20000, 60000))
      )
    )
  }

  @Benchmark
  def challenger3(blackhole: Blackhole): Unit = {
    import benchmark4._
    import dataset4._

    val result = dataset.execute(op)

    blackhole.consume(result)
  }

}
