package QuartzScheduler.Models

sealed trait ProcessResult
case object NoComments                            extends ProcessResult
case class Updated(newTimestamp: Long)            extends ProcessResult
case class NoUpdateNeeded(currentTimestamp: Long) extends ProcessResult
