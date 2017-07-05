package mcross1882.mahjong

import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn

object Application {

    def main(args: Array[String]) {

        val game = new Game

        val factory = new CommandFactory(game)

        val players = Seq(
            new Player("Player A"),
            new Bot("Player B"),
            new Bot("Player C"),
            new Bot("Player D")
        )

        val commands = new ArrayBuffer[Command]

        for (player <- players) {
            commands ++= dealStartingTiles(player)
        }

        for (command <- commands) {
            command.execute(game)
        }


        var line: String = ""
        while (!game.isFinished) {
            for (player <- players) {
                line = player match {
                    case p: NPC => p.getNextMove
                    case p: Player => StdIn.readLine(s"${player.name}> ")
                } 

                if ("exit" == line) {
                    return
                }

                val command = factory.create(player, line)
                command.execute(game)
            }
        }
    }

    private def dealStartingTiles(player: Player): Seq[DealTile] = List.fill(13)(new DealTile(player))
}
