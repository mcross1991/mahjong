package mcross1882.mahjong

import scala.collection.mutable.ArrayBuffer

object Application {

    def main(args: Array[String]) {

        val game = new Game

        val players = Seq(
            new Player("demo"),
            new SimplePlayer,
            new SimplePlayer,
            new SimplePlayer
        )

        val commands = new ArrayBuffer[Command]

        for (player <- players) {
            commands ++= dealStartingTiles(player)
        }

        for (command <- commands) {
            command.execute(game)
        }

        val tiles = player.tiles
        println(tiles)

        val selectedTiles = Seq(4, 2, 5).map(x => tiles(x))
        println(selectedTiles)
    }

    private def dealStartingTiles(player: Player): Seq[DealTile] = List.fill(13)(new DealTile(player))
}
