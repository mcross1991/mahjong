package mcross1882.mahjong

trait Command {

    def execute(game: Game)
}

case class ExitGame() extends Command {

    def execute(game: Game) {
        game.finish
    }
}

case class ShowTiles(player: Player, tiles: Seq[Tile], hideIndicies: Boolean) extends Command {

    def execute(game: Game) {
        printTiles()
    }

    private def printTiles() {
        val numberOfTiles = tiles.length

        for (index <- 0 until numberOfTiles) {
            print(" +--+ ")
        }
        println

        for (index <- 0 until numberOfTiles) {
            print(s" |${tiles(index).value}| ")
        }
        println

        for (index <- 0 until numberOfTiles) {
            print(" |--| ")
        }
        println

        for (index <- 0 until numberOfTiles) {
            print(s" |${tiles(index).category}| ")
        }
        println

        for (index <- 0 until numberOfTiles) {
            print(" +--+ ")
        }
        println

        if (!hideIndicies) {
            for (index <- 0 until numberOfTiles) {
                print(f"  ${index}%2d  ")
            }
            println
        }
    }
}

case class ShowScore(player: Player) extends Command {

    def execute(game: Game) {
        val header = s"${player.name} Score"
        println(header)
        println("-" * header.length)
        println
        listScoreTiles(game, "kong", player.score.listKongs)
        println
        listScoreTiles(game, "pung", player.score.listPungs)
        println
        listScoreTiles(game, "chow", player.score.listChows)
        println
    }

    private def listScoreTiles(game: Game, setName: String, list: Seq[Seq[Tile]]) {
        if (list.length > 0) {
            println(s"${setName.capitalize} (${list.length})")
            for (item <- list) {
                (new ShowTiles(player, item, true)).execute(game)
            }
        } else {
            println(s"No ${setName}s")
        } 
    }
}

case class DealTile(player: Player) extends Command {

    def execute(game: Game) {
        val tile = game.dealTile
        player.takeTile(tile)
        println(s"${player.name} takes $tile")
    }
}

case class DiscardTile(player: Player, tileIndex: Int) extends Command {

    def execute(game: Game) {
        val tile = player.giveTile(tileIndex)
        game.discardTile(tile)
        game.stopWaiting
        println(s"${player.name} discards $tile")
        
    }
}

case class LastDiscardedTile(player: Player) extends Command {

    def execute(game: Game) {
        val tile = game.lastDiscardedTile
        println(s"${player.name} discarded $tile")
    }
}

trait MatchingTileFinder {

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

    protected def isLinearSet(tiles: Seq[Tile], requiredCount: Int): Boolean = {
        if (tiles.length != requiredCount) {
            return false
        }

        var currentValue = tiles.head.intValue
        for (index <- 1 until tiles.length) {
            if (currentValue != (tiles(index).intValue - 1)) {
                 return false
            }
            currentValue = tiles(index).intValue
        }
        true
    }
}

case class CallPung(player: Player, selectedTiles: Seq[Tile]) extends Command with MatchingTileFinder  {

    def execute(game: Game) {
        if (isPung(selectedTiles)) {
            println("Pung")
            player.score.addPung(selectedTiles)
            player.removeTiles(selectedTiles)
            game.setCurrentPlayer(player)
            (new DealTile(player)).execute(game)
        } else {
            println("Combination is not a pung")
        }
    }

    private def isPung(tiles: Seq[Tile]): Boolean = isMatchingSet(tiles, 3)
}

case class CallKong(player: Player, selectedTiles: Seq[Tile]) extends Command with MatchingTileFinder {

    def execute(game: Game) {
        if (isKong(selectedTiles)) {
            println("Kong")
            player.score.addKong(selectedTiles)
            player.removeTiles(selectedTiles)
            game.setCurrentPlayer(player)
            (new DealTile(player)).execute(game)
        } else {
            println("Combination is not a kong")
        }
    }

    private def isKong(tiles: Seq[Tile]): Boolean = isMatchingSet(tiles, 4)
}

case class CallChow(player: Player, selectedTiles: Seq[Tile]) extends Command with MatchingTileFinder {

    def execute(game: Game) {
        if (isChow(selectedTiles)) {
            println("Chow")
            player.score.addChow(selectedTiles)
            player.removeTiles(selectedTiles)
            game.setCurrentPlayer(player)
            (new DealTile(player)).execute(game)
        } else {
            println("Combination is not a chow")
        }
    }

    private def isChow(tiles: Seq[Tile]): Boolean = {
        if (!Tile.isSuitedTile(tiles.head)) {
            return false
        }
        isLinearSet(tiles, 3)
    }
}

