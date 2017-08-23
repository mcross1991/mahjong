/**
  * Copyright (C) 2017-2018 the original author or authors.
  * See the LICENSE file distributed with this work for additional
  * information regarding copyright ownership.
  *
  * @author Matthew Cross <github.com/mcross1991>
  */
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
                e.printStackTrace
            }
        }
    }
}


