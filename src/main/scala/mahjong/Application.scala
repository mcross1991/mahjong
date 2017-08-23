package mahjong

object Application {

    def main(args: Array[String]) {
        val session = new Session(Seq(
            Player.create("Player A"),
            Player.createBot("Player B"),
            Player.createBot("Player C"),
            Player.createBot("Player D")
        ))

        startSession(session)
    }

    private def startSession(session: Session) {
        try {
            session.start
        } catch {
            case e: Exception => {
                println(s"Whoops! ${e.getMessage}")
            }
        }
    }
}


