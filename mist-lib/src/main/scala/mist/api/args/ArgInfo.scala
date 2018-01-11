package mist.api.args

sealed trait ArgInfo
final case class InternalArgument(tags: Seq[String] = Seq.empty) extends ArgInfo
final case class UserInputArgument(name: String, t: ArgType) extends ArgInfo

object ArgInfo {
  val StreamingContextTag = "streaming"
  val SqlContextTag = "sql"
  val HiveContextTag = "hive"
}

