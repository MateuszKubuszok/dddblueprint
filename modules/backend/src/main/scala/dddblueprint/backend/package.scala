package dddblueprint

import java.nio.file.Path

package object backend {

  type Backend[IO[_]] = output.Blueprint => IO[ListMap[Path, String]]
}
