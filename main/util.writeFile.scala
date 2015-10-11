package util

object writeFile extends ((java.io.File, String) => Unit) {
  def apply(file: java.io.File, content: String) {
    import java.nio.file.{Paths, Files}
    import java.nio.charset.StandardCharsets
    Files.write(Paths.get(file.toURI), content.getBytes(StandardCharsets.UTF_8))
  }
}
