package mahjong

import scala.collection.mutable.ArrayBuffer

class Session {

    def start() {
        val players = Seq(
            Player.create("Player A"),
            Player.createBot("Player B"),
            Player.createBot("Player C"),
            Player.createBot("Player D")
        )

        val game = new Game(players)

        DealStartingTiles().execute(game)

        while (!game.isFinished) {
            AskOtherPlayers().execute(game)

            val player = game.getCurrentPlayer

            game.renderLine(s"${player.name} tiles")

            ShowTiles(player, player.tiles, false).execute(game)
            DealTile(player).execute(game)
            AskPlayer(player).execute(game)
        }
    }
}
