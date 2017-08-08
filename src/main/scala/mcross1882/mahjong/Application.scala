package mcross1882.mahjong

import scala.collection.mutable.ArrayBuffer

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

class Session {

    def start() {
        val factory = new CommandFactory

        val players = Seq(
            Player.create("Player A", factory),
            Player.createBot("Player B", factory),
            Player.createBot("Player C", factory),
            Player.createBot("Player D", factory)
        )

        val game = new Game(players)

        DealStartingTiles().execute(game)

        while (!game.isFinished) {
            AskOtherPlayers().execute(game)

            val player = game.getCurrentPlayer

            println(s"${player.name} tiles")

            ShowTiles(player, player.tiles, false).execute(game)
            DealTile(player).execute(game)
            AskPlayer(player).execute(game)
        }
    }
}

