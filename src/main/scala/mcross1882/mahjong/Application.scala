package mcross1882.mahjong

import scala.collection.mutable.ArrayBuffer

object Application {

    def main(args: Array[String]) {

        val game = new Game

        val factory = new CommandFactory

        val players: Seq[Player] = Seq(
            Player.create("Player A", factory),
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
                game.setCurrentPlayer(player)

                (new DealTile(player)).execute(game)
                while (game.isWaitingForPlayer()) {
                    val command = player.nextCommand
                    command.execute(game)
                }
            }
        }
    }

    private def dealStartingTiles(player: Player): Seq[DealTile] = List.fill(13)(new DealTile(player))
}

