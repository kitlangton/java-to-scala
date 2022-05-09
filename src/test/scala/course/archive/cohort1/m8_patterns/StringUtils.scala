package course.archive.cohort1.m8_patterns

object StringUtils {
  def red(value: Any): String =
    Console.RED + value + Console.RESET

  def blue(value: Any): String =
    Console.BLUE + value + Console.RESET

  def green(value: Any): String =
    Console.GREEN + value + Console.RESET
}
