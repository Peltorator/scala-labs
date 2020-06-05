package lab10

import cats.CommutativeApply.ops.toAllCommutativeApplyOps
import cats.syntax.all._
import scala.concurrent.duration._
import cats.instances.list._
import cats.effect.{ExitCode, IO, IOApp, Resource}
import cats.effect.concurrent.{MVar, Ref, Semaphore}


object EchoCounter extends IOApp {
  def runPrinter(mvar: MVar[IO, String]): Resource[IO, Unit] = {
    def myPrint: IO[Unit] = for {
      cnt <- mvar.take
      _ <- IO(println(cnt))
      _ <- myPrint
    } yield ()

    Resource.make(myPrint.start)(_.cancel.flatMap(_ => IO(println("Full stop.")))).void
  }

  def runCounter(mvar: MVar[IO, String]): Resource[IO, Unit] = {
    def myCount(cnt: Int): IO[Unit] = for {
      _ <- IO.sleep(1.seconds)
      _ <- mvar.put(cnt.toString)
      _ <- myCount(cnt + 1)
    } yield ()

    Resource.make(myCount(0).start)(_.cancel.flatMap(_ => IO(println("Full stop.")))).void
  }

  val prog: Resource[IO, Unit] = for {
    mvar <- Resource.make(MVar.empty[IO, String])(_ => IO.unit)
    _ <- runPrinter(mvar)
    _ <- runCounter(mvar)
  } yield ()

  override def run(args: List[String]): IO[ExitCode] =
    prog.use(_ => IO.never)
}