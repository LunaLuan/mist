package mist.api.args

import mist.api._
import shadedshapeless._
import shadedshapeless.ops.adjoin.Adjoin

trait ArgCombiner[A, B] extends Serializable {
  type Out
  def apply(a: ArgDef[A], b: ArgDef[B]): ArgDef[Out]
}

object ArgCombiner {

  type Aux[A, B, Out0] = ArgCombiner[A, B] { type Out = Out0 }

  implicit def combiner[A, B](implicit adj: Adjoin[A :: B :: HNil]): Aux[A, B, adj.Out] = new ArgCombiner[A, B] {
    type Out = adj.Out

    override def apply(a: ArgDef[A], b: ArgDef[B]): ArgDef[Out] = {
      new ArgDef[adj.Out] {
        def describe(): Seq[ArgInfo] = a.describe() ++ b.describe()

        def extract(ctx: FnContext): ArgExtraction[adj.Out] = {
          (b.extract(ctx), a.extract(ctx)) match {
            case (Extracted(be), Extracted(ae)) =>
              val out = adj(ae :: be :: HNil)
              Extracted(out)
            case (Missing(errB), Missing(errA)) => Missing(errA + ", " + errB)
            case (x@Missing(_), _) => x.asInstanceOf[Missing[adj.Out]]
            case (_, x@Missing(_)) => x.asInstanceOf[Missing[adj.Out]]
          }
        }

        override def validate(ctx: FnContext): Option[String] = {
          (a.validate(ctx), b.validate(ctx)) match {
            case (None, None) => None
            case (Some(errA), Some(errB)) => Some(errA + "\n" + errB)
            case (errA @ Some(_), None) => errA
            case (None, errB @ Some(_)) => errB
          }
        }
      }
    }
  }

}
