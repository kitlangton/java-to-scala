package course.miscellany.types

/** The Light Switch
  */
sealed trait LightSwitch

object LightSwitch {
  case object On  extends LightSwitch
  case object Off extends LightSwitch
}

/** Pac-Man
  */
sealed trait PacManInput

object PacManInput {
  case object Up    extends PacManInput
  case object Down  extends PacManInput
  case object Left  extends PacManInput
  case object Right extends PacManInput
  case object Stop  extends PacManInput
}

/** Tic Tac Toe
  */
sealed trait TicTacToePosition

object TicTacToePosition {
  case object TopLeft      extends TicTacToePosition
  case object TopCenter    extends TicTacToePosition
  case object TopRight     extends TicTacToePosition
  case object MiddleLeft   extends TicTacToePosition
  case object MiddleCenter extends TicTacToePosition
  case object MiddleRight  extends TicTacToePosition
  case object BottomLeft   extends TicTacToePosition
  case object BottomCenter extends TicTacToePosition
  case object BottomRight  extends TicTacToePosition
}
