package lab08

import java.nio.file.{Files, Path, Paths, StandardCopyOption}

import cats.data.{IndexedStateT, StateT}
import cats.{Applicative, Id, Monad}
import cats.syntax.all._
import cats.instances.list._

import scala.jdk.CollectionConverters._
import scala.language.higherKinds

trait MkDir[F[_], Dir] {
  def mkDir(dir: Dir, name: String): F[Dir]
}

trait MkFile[F[_], Dir, File] {
  def mkFile(dir: Dir, name: String): F[File]
}

trait getFileNames[F[_], File] {
  def getFileNames(files: List[File]): F[List[String]]
}

trait getDirFiles[F[_], Dir, File] {
  def getDirFiles(dir: Dir): F[List[File]]
}

trait MoveFile[F[_], File, Dir] {
  def moveFile(file: File, dir: Dir): F[File]
}

trait Printer[F[_], File] {
  def printName(file: File): F[Unit]
}

class Program[F[_], Dir, File](implicit
                               F: Monad[F],
                               mkDir: MkDir[F, Dir],
                               mkFile: MkFile[F, Dir, File],
                               getDirFiles: getDirFiles[F, Dir, File],
                               getFileNames: getFileNames[F, File],
                               moveFile: MoveFile[F, File, Dir],
                               printer: Printer[F, File]) {
  def run(dir: Dir): F[Unit] = for {
    testDir <- mkDir.mkDir(dir, "test_dir")
    _ <- mkFile.mkFile(testDir, "foo")
    _ <- mkFile.mkFile(testDir, "bar")
    _ <- mkFile.mkFile(testDir, "baz")
    files <- getDirFiles.getDirFiles(testDir)
    _ <- files.map(file => printer.printName(file)).sequence
    fileNames <- getFileNames.getFileNames(files)
    newDirs <- fileNames.map (name => mkDir.mkDir(testDir, name.head.toString)).sequence
    _ <- files.zip(newDirs).map(f => moveFile.moveFile(f._1, f._2)).sequence
  } yield ()
}

class RealFileSystem[F[_] : Applicative] extends MkDir[F, Path]
  with MkFile[F, Path, Path]
  with getDirFiles[F, Path, Path]
  with MoveFile[F, Path, Path]
  with getFileNames[F, Path] {
  override def mkDir(dir: Path, name: String): F[Path] = Files.createDirectory(dir.resolve(name)).pure[F]

  override def mkFile(dir: Path, name: String): F[Path] = Files.createFile(dir.resolve(name)).pure[F]

  override def getDirFiles(dir: Path): F[List[Path]] = {
    Files.list(dir).filter(Files.isRegularFile(_)).iterator().asScala.toList.pure[F]
  }

  override def moveFile(file: Path, dir: Path): F[Path] = Files.move(file, dir).pure[F]

  override def getFileNames(files: List[Path]): F[List[String]] = files.map(file => file.getFileName.toString).pure[F]
}

class ConsolePathPrinter[F[_] : Applicative] extends Printer[F, Path] {
  override def printName(file: Path): F[Unit] = println(file.getFileName).pure[F]
}

object TypeClasses {
  def main(args: Array[String]): Unit = {
    implicit val fs: RealFileSystem[Id] = new RealFileSystem[Id]
    implicit val printer: ConsolePathPrinter[Id] = new ConsolePathPrinter[Id]
    val program = new Program[Id, Path, Path]

    program.run(Paths.get("."))
  }
}