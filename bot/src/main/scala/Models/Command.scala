package Models

sealed trait Command {
  def command: String
  def description: String
}

object Command {
  case object Start extends Command {
    val command     = "/start"
    val description = "Запуск бота"
  }

  case object List extends Command {
    val command     = "/list"
    val description = "Список отслеживаемых ссылок"
  }

  case object Track extends Command {
    val command     = "/track"
    val description = "Добавить ссылку для отслеживания (с тегами и фильтрами)"
  }

  case object Untrack extends Command {
    val command     = "/untrack"
    val description = "Удалить ссылку из отслеживания"
  }

  val all: List[Command] = scala.List(Start, Command.List, Track, Untrack)
}
