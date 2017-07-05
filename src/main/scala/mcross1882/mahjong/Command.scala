package mcross1882.mahjong

sealed abstract class Command {

    def execute(game: Game)
}

class ShowTiles(player: Player) extends Command {

    def execute(game: Game) {
        player.printTiles
    }
}

class DealTile(player: Player) extends Command {

    def execute(game: Game) {
        val tile = game.dealTile
        player.takeTile(tile)
        println(s"${player.name} takes $tile")
    }
}

class DiscardTile(player: Player, tileIndex: Int) extends Command {

    def execute(game: Game) {
        val tile = player.giveTile(tileIndex)
        game.discardTile(tile)
        println(s"${player.name} discards $tile")
        
    }
}

class LastDiscardedTile(player: Player) extends Command {

    def execute(game: Game) {
        val tile = game.lastDiscardedTile
        println(s"${player.name} discarded $tile")
    }
}

sealed trait MatchingTileFinder {

    protected def isMatchingSet(tiles: Seq[Tile], requiredCount: Int): Boolean = {
        if (tiles.length != requiredCount) {
            return false
        }

        val firstTile = tiles.head
        for (index <- 1 until tiles.length) {
            if (firstTile != tiles(index)) {
                return false
            }
        }
        true
    }
}

class CallPung(player: Player, selectedTiles: Seq[Tile]) extends Command with MatchingTileFinder  {

    def execute(game: Game) {
        if (isPung(selectedTiles)) {
            println("Pung")
            player.addScore(10)
        } else {
            println("Combination is not a pung")
        }
    }

    private def isPung(tiles: Seq[Tile]): Boolean = isMatchingSet(tiles, 3)
}

class CallKong(player: Player, selectedTiles: Seq[Tile]) extends Command with MatchingTileFinder {

    def execute(game: Game) {
        if (isKong(selectedTiles)) {
            println("Kong")
            player.addScore(10)
        } else {
            println("Combination is not a kong")
        }
    }

    private def isKong(tiles: Seq[Tile]): Boolean = isMatchingSet(tiles, 4)
}
