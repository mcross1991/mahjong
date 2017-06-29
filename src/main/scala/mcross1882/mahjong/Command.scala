package mcross1882.mahjong

sealed abstract class Command {

    def execute(game: Game)
}

class DealTile(player: Player) extends Command {

    def execute(game: Game) {
        player.takeTile(game.dealTile)
    }
}

class DiscardTile(player: Player, tileIndex: Int) extends Command {

    def execute(game: Game) {
        game.discardTile(player.giveTile(tileIndex))
    }
}

class CallPung(player: Player, indicies: Seq[Int]) extends Command {

    def execute(game: Game) {
        val tiles = indicies.map(x => player.tiles(x))
        if (game.isPung(tiles)) {
            println("Pung")
            player.addScore(10)
        }
    }
}

class CallKong(player: Player, indicies: Seq[Int]) extends Command {

    def execute(game: Game) {
        val tiles = indicies.map(x => player.tiles(x))
        if (game.isKong(tiles)) {
            println("Kong")
            player.addScore(10)
        }
    }
}
