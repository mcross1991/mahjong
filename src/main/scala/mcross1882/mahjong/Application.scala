package mahjong

object Application {

    def main(args: Array[String]) {
        restartSession(new Session)
    }

    private def restartSession(session: Session) {
        try {
            session.start
        } catch {
            case e: Exception => {
                println(s"Whoops! ${e.getMessage}")
                restartSession(session)
            }
        }
    }
}


