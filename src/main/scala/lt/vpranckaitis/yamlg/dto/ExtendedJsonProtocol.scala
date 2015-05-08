package lt.vpranckaitis.yamlg.dto

import spray.json.DefaultJsonProtocol

object ExtendedJsonProtocol extends DefaultJsonProtocol {
  implicit val BoardFormat = jsonFormat1(Board)
  implicit val GameFormat = jsonFormat4(Game)
}