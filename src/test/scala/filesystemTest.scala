package lab08

import java.nio.file.{Files, Path, Paths, StandardCopyOption}
import java.util.Comparator

import cats.Id
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec

class filesystemTestTest extends AnyFlatSpec
  with Matchers {
  def deleteFiles(dir: Path): Unit = {
    Files.walk(dir)
      .sorted(Comparator.reverseOrder())
      .forEach(file => Files.deleteIfExists(file))
  }

  val path = Paths.get("tmp")
  val dir = Files.createDirectory(path)

  implicit val fileSystem: RealFileSystem[Id] = new RealFileSystem[Id]
  implicit val printer: ConsolePathPrinter[Id] = new ConsolePathPrinter[Id]
  
  val program = new Program[Id, Path, Path]
  program.run(path)

  Files.exists(path.resolve("test_dir")) shouldBe true

  val f = path.resolve("test_dir/f")
  val b = path.resolve("test_dir/b")

  for (cur <- List(f, b)) {
    Files.exists(cur) && Files.isDirectory(cur) shouldBe true
  }

  for (name <- List("foo", "bar", "baz")) {
    var cur = b
    if (name.head == 'f')
      cur = f
    Files.exists(cur.resolve(name)) && Files.isRegularFile(cur.resolve(name)) shouldBe true
  }

  deleteFiles(path)
}