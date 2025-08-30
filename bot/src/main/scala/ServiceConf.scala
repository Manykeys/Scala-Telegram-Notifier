package bot
import pureconfig.*
import pureconfig.generic.derivation.*
import pureconfig.generic.semiauto.*

case class ServiceConf(
    tgBotToken: String,
    tgPort: Int,
    apiPort: Int,
) derives ConfigReader
