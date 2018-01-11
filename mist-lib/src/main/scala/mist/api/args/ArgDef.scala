package mist.api.args

import mist.api.{FullFnContext, FnContext, Handle}

trait ArgDef[A] { self =>

  def describe(): Seq[ArgInfo]

  def extract(ctx: FnContext): ArgExtraction[A]

  def combine[B](other: ArgDef[B])
      (implicit cmb: ArgCombiner[A, B]): ArgDef[cmb.Out] = cmb(self, other)

  def &[B](other: ArgDef[B])
    (implicit cmb: ArgCombiner[A, B]): ArgDef[cmb.Out] = cmb(self, other)

  def map[B](f: A => B): ArgDef[B] =
    transform(_.map(f))

  def transform[B](f: ArgExtraction[A] => ArgExtraction[B]): ArgDef[B] =
    new ArgDef[B] {
      def describe(): Seq[ArgInfo] = self.describe()
      def extract(ctx: FnContext): ArgExtraction[B] = f(self.extract(ctx))
    }

  def validated(f: A => Boolean, reason: String = ""): ArgDef[A] = {
    transform(_.flatMap(v => {
      if (f(v))
        Extracted(v)
      else {
        val msg = "Arg was rejected by validation rule" + (if (reason.isEmpty) "" else ":" + reason)
        Missing(msg)
      }
    }))
  }

  private[api] def validate(ctx: FnContext): Option[String] = self.extract(ctx) match {
    case Missing(err) => Some(err)
    case Extracted(_) => None
  }

  def apply[F, R](f: F)(implicit tjd: ToHandle.Aux[A, F, R]): Handle[R] = tjd(self, f)

}

object ArgDef {

  trait InternalArg[A] extends ArgDef[A] {
    override final def validate(ctx: FnContext): Option[String] = {
      ctx match {
        case _: FullFnContext => super.validate(ctx)
        case _ => None
      }
    }
  }

  def system[A](tags: String*)(f: FullFnContext => ArgExtraction[A]): ArgDef[A] =
    new InternalArg[A] {
      override def extract(ctx: FnContext): ArgExtraction[A] = ctx match {
          case c: FullFnContext => f(c)
          case _ =>
            val desc = s"Unknown type of job context ${ctx.getClass.getSimpleName} " +
              s"expected ${FullFnContext.getClass.getSimpleName}"
            Missing(desc)
      }

      override def describe(): Seq[ArgInfo] = Seq(InternalArgument(tags))
    }

  def system[A](f: FullFnContext => ArgExtraction[A]): ArgDef[A] = system()(f)

  def const[A](value: => A): ArgDef[A] = system(_ => Extracted(value))

  def missing[A](message: String): ArgDef[A] = system(_ => Missing(message))

}


//trait ArgInstances2 {
//
//  val allArgs: ArgDef[Map[String, Any]] = new ArgDef[Map[String, Any]] {
//    override def describe(): Seq[ArgInfo] = Seq.empty
//
//    override def extract(ctx: FnContext): ArgExtraction[Map[String, Any]] = Extracted(ctx.params)
//  }
//
//  def arg[A](name: String)(implicit ext: FieldExtractor[A]): ArgDef[A] = new ArgDef[A] {
//    override def extract(ctx: FnContext): ArgExtraction[A] = ctx.params.get(name) match {
//      case Some(any) => ext.extract(any)
//      case None => Missing(s"Missing argument: $name")
//    }
//
//
//    override def describe(): Seq[ArgInfo] = Seq(UserInputArgument(name, ext.`type`))
//  }
//
//  def arg[A](name: String, default: => A)(implicit ext: FieldExtractor[A]): ArgDef[A] =
//    arg(name).transform(v => if (v.isMissing) Extracted(default) else v)
//}

//trait SystemArg[A] extends ArgDef[A] {
//  override def validate(params: Map[String, Any]): Either[Throwable, Any] =
//    Right(())
//}
//
//object SystemArg {
//
//  def apply[A](tags: Seq[String], f: => ArgExtraction[A]): ArgDef[A] = new SystemArg[A] {
//    override def extract(ctx: FnContext): ArgExtraction[A] = f
//
//    override def describe() = Seq(InternalArgument(tags))
//  }
//
//  def apply[A](tags: Seq[String], f: FullFnContext => ArgExtraction[A]): ArgDef[A] = new SystemArg[A] {
//    override def extract(ctx: FnContext): ArgExtraction[A] = ctx match {
//      case c: FullFnContext => f(c)
//      case _ =>
//        val desc = s"Unknown type of job context ${ctx.getClass.getSimpleName} " +
//          s"expected ${FullFnContext.getClass.getSimpleName}"
//        Missing(desc)
//    }
//
//    override def describe() = Seq(InternalArgument(tags))
//  }
//}

//object ArgDef {
//
//  def system[A](f: => ArgExtraction[A], tags: Seq[String] = Seq.empty): ArgDef[A] =
//    SystemArg(tags, f)
//
//  def const[A](value: A): ArgDef[A] = system(Extracted(value))
//
//  def missing[A](message: String): ArgDef[A] = system(Missing(message))
//
//  def user[A](info: Seq[ArgInfo], f: Map[String, Any] => ArgExtraction[A]): ArgDef[A] =
//    new UserArg[A] {
//      override def extract(ctx: FnContext): ArgExtraction[A] = f(ctx.params)
//
//      override def describe(): Seq[ArgInfo] = info
//    }
//
//  def single[A](name: String, `type`: ArgType)(f: Any => ArgExtraction[A]): ArgDef[A] =
//    user(Seq(UserInputArgument(name, `type`)), map => map.get(name) match {
//      case Some(any) => f(any)
//      case None => Missing(s"Missing field:$name")
//    })
//
//  def user[A](extr: ArgExtractor[A]): ArgDef[A] = {
//    val x = extr.describe
//    user(x, extr.extract)
//  }
//
//
//}

