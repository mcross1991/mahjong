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
        val game = new Game

        val factory = new CommandFactory

        val players: Seq[Player] = Seq(
            Player.createRemote("Player A", factory),
            Player.createBot("Player B", factory),
            Player.createBot("Player C", factory),
            Player.createBot("Player D", factory)
        )

        val commands = new ArrayBuffer[Command]

        for (player <- players) {
            commands ++= dealStartingTiles(player)
        }

        for (command <- commands) {
            command.execute(game)
        }

        while (!game.isFinished) {
            for (player <- players) {
                println(s"${player.name} tiles")
                ShowTiles(player, player.groupedTiles.flatMap(x => x), true).execute(game)

                game.setCurrentPlayer(player)

                DealTile(player).execute(game)

                executePlayerCommands(player, game)

                if (game.isFinished) {
                    return
                }

                game.lastDiscardedTile match {
                    case Some(lastTile) => checkOtherPlayers(players, player, lastTile, game)
                    case None => // noop
                }
            }
        }
    }

    private def executePlayerCommands(player: Player, game: Game) {
        while (game.isWaitingForPlayer()) {
            val command = player.nextCommand()
            command.execute(game)

            if (player.canCallMahjong) {
                println(s"${player.name} calls mahjong!")
                game.finish
            }
        }
    }

    private def checkOtherPlayers(players: Seq[Player], currentPlayer: Player, lastTile: Tile, game: Game) {
        var isTileTaken: Boolean = false
        for (otherPlayer <- players if otherPlayer != currentPlayer && !isTileTaken) {
            otherPlayer.checkLastTile(lastTile) match {
                case command: SkipCommand => command.execute(game)
                case command: Command => {
                    command.execute(game)
                    isTileTaken = true
                }
            }

            if (otherPlayer.canCallMahjong) {
                println(s"${otherPlayer.name} calls mahjong!")
                game.finish
            }
        }
    }

    private def dealStartingTiles(player: Player): Seq[DealTile] = List.fill(13)(new DealTile(player))
}

