package mahjong

object Application {

    def main(args: Array[String]) {
        val session = new Session(Seq(
            Player.create("Player A"),
            Player.createBot("Player B"),
            Player.createBot("Player C"),
            Player.createBot("Player D")
        ))

        restartSession(session)
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


