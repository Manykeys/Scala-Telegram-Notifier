package scrapper

import pureconfig.*
import pureconfig.generic.semiauto.*

case class ServiceConf(
    githubToken: String,
    tgPort: Int,
    apiPort: Int,
) derives ConfigReader
