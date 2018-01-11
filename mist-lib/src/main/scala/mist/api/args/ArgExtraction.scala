package mist.api.args

sealed trait ArgExtraction[+A] { self =>

  def map[B](f: A => B): ArgExtraction[B] = self match {
    case Extracted(a) => Extracted(f(a))
    case m@Missing(_) => m.asInstanceOf[ArgExtraction[B]]
  }

  def flatMap[B](f: A => ArgExtraction[B]): ArgExtraction[B] = self match {
    case Extracted(a) => f(a)
    case m@Missing(_) => m.asInstanceOf[ArgExtraction[B]]
  }

  def isMissing: Boolean = self match {
    case Extracted(a) => false
    case m@Missing(_) => true
  }

}

final case class Extracted[+A](value: A) extends ArgExtraction[A]
final case class Missing[+A](description: String) extends ArgExtraction[A]

