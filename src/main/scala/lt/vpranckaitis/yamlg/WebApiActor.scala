package lt.vpranckaitis.yamlg

import spray.routing.HttpServiceActor

class WebApiActor extends HttpServiceActor {
  def receive = runRoute {
    path("") {
      get {
        complete("Pong")
      }
    }
  }
}