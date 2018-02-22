package coop.rchain.rholang.intepreter

import cats._
import cats.implicits._

object Pattern {

  def matches(pattern: Channel, target: Par): Option[Unit] =
    pattern match {
      case Quote(p)   => matches(p, target)
      case ChanVar(v) => Some(())
    }

  def matches(pattern: Par, target: Par): Option[Unit] =
    (pattern, target) match {
      case (
          Par(sendpats, recvpats, evalpats, newpats, exprpats),
          Par(sendtrgs, recvtrgs, evaltrgs, newtrgs, exprtrgs)
          ) =>
        for {
          _ <- Traverse[List].traverse(sendpats.zip(sendtrgs)) {
            case (sendpat, sendtrg) => matches(sendpat, sendtrg)
          }
          _ <- Traverse[List].traverse(recvpats.zip(recvtrgs)) {
            case (recvpat, recvtrg) => matches(recvpat, recvtrg)
          }
          _ <- Traverse[List].traverse(evalpats.zip(evaltrgs)) {
            case (evalpat, evaltrg) => matches(evalpat, evaltrg)
          }
          _ <- Traverse[List].traverse(newpats.zip(newtrgs)) {
            case (newpat, newtrg) => matches(newpat, newtrg)
          }
          _ <- Traverse[List].traverse(exprpats.zip(exprtrgs)) {
            case (exprpat, exprtrg) => matches(exprpat, exprtrg)
          }
        } yield ()
    }

  def matches(pattern: New, target: New): Option[Unit] = ???

  def matches(pattern: Send, target: Send): Option[Unit]       = ???
  def matches(pattern: Receive, target: Receive): Option[Unit] = ???
  def matches(pattern: Eval, target: Eval): Option[Unit]       = ???
  def matches(pattern: Expr, target: Expr): Option[Unit]       = ???

//   def matches(
//       patterns: List[Channel],
//       procs: List[Par]
//   ): List[Either[Quote, Par]] =
//     for ((pattern, proc) <- patterns.zip(procs)) yield {
//       pattern match {
//         case Quote(_)            => Right(proc)
//         case ChanVar(FreeVar(_)) => Left(Quote(proc))
//         case _ =>
//           throw new IllegalStateException("cannot match bound variable")
//       }
//     }

//   def patternEnv(things: List[Either[Quote, Par]]): HashMap[]

}
