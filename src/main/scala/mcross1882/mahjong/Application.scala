package mcross1882.mahjong

import scala.collection.mutable.ArrayBuffer

object Application {

    def main(args: Array[String]) {

        val game = new Game

        val screen = new ConsoleScreen

        val player = new Player("demo")

        val commands = new ArrayBuffer[Command]

        commands ++= dealStartingTiles(player)

        for (command <- commands) {
            command.execute(game)
        }

        screen.renderPlayerTiles(player)
    }

    private def dealStartingTiles(player: Player): Seq[DealTile] = List.fill(13)(new DealTile(player))
}
