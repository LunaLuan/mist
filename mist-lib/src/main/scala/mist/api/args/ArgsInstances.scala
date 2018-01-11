package mist.api.args

import mist.api.FnContext

sealed trait ExpectedInput {
  def name: String
}

object ExpectedInput{

  case class Primitive(name: String) extends ExpectedInput
  case object Boolean extends Primitive("Boolean")
  case object Int extends Primitive("Int")
  case object String extends Primitive("String")
  case object Double extends Primitive("Double")
  case object Any extends Primitive("Any")

  final case class Arr(v: ExpectedInput) extends ExpectedInput {
    def name: String = "Arr"
  }

  final case class Obj(fields: Seq[(String, ExpectedInput)]) extends ExpectedInput {
    def name: String = "Obj"
  }

  final case class Map(k: Primitive, v: ExpectedInput) extends ExpectedInput {
    def name: String = "Map"
  }

  final case class Option(v: ExpectedInput) extends ExpectedInput {
    def name: String = "Option"
  }
}

trait ArgExtractor[A] {
  def extract(a: Any): ArgExtraction[A]
  def expect: ExpectedInput
}

object ArgExtractor {

  def nonNull[A](ei: ExpectedInput)(f: Any => ArgExtraction[A]): ArgExtractor[A] = {
    new ArgExtractor[A] {

      override def expect(): ExpectedInput = ei

      override def extract(a: Any): ArgExtraction[A] =
        if (a == null) Missing("value is missing") else f(a)
    }
  }

  implicit val int: ArgExtractor[Int] = nonNull(ExpectedInput.Int){
    case i: Int => Extracted(i)
    case _ => Missing("invalid type")
  }

  implicit def seqXXX[A](implicit u: ArgExtractor[A]): ArgExtractor[Seq[A]] =
    nonNull(ExpectedInput.Arr(u.expect)) {
      case s: Seq[_] =>
      case _ => Missing("invalid type")
    }

}

trait ArgsInstances {

  val allArgs: ArgDef[Map[String, Any]] = new ArgDef[Map[String, Any]] {
    override def describe(): Seq[ArgInfo] = Seq.empty

    override def extract(ctx: FnContext): ArgExtraction[Map[String, Any]] = Extracted(ctx.params)
  }

  def arg[A](name: String)(implicit ext: XXX[A]): ArgDef[A] = new ArgDef[A] {
    override def extract(ctx: FnContext): ArgExtraction[A] = ctx.params.get(name) match {
      case Some(any) => ext.extract(any)
      case None => Missing(s"Missing argument: $name")
    }


    override def describe(): Seq[ArgInfo] = Seq(UserInputArgument(name, ext.`type`))
  }

  def arg[A](name: String, default: => A)(implicit ext: FieldExtractor[A]): ArgDef[A] =
    arg(name).transform(v => if (v.isMissing) Extracted(default) else v)
}

object ArgsInstances extends ArgsInstances

