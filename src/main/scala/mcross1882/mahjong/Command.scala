package mcross1882.mahjong

trait Command {

    def execute(game: Game)
}

case class SkipCommand() extends Command {

    def execute(game: Game) {
        game.stopWaiting
    }
}

case object ExitGame extends Command {

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
            player.renderOutput(" +--+ ")
        }
        player.renderOutput("\n")

        for (index <- 0 until numberOfTiles) {
            player.renderOutput(s" |${tiles(index).value}| ")
        }
        player.renderOutput("\n")

        for (index <- 0 until numberOfTiles) {
            player.renderOutput(" |--| ")
        }
        player.renderOutput("\n")

        for (index <- 0 until numberOfTiles) {
            player.renderOutput(s" |${tiles(index).category}| ")
        }
        player.renderOutput("\n")

        for (index <- 0 until numberOfTiles) {
            player.renderOutput(" +--+ ")
        }
        player.renderOutput("\n")

        if (!hideIndicies) {
            for (index <- 0 until numberOfTiles) {
                player.renderOutput(f"  ${index}%2d  ")
            }
            player.renderOutput("\n")
        }
    }
}

case class ShowScore(player: Player) extends Command {

    def execute(game: Game) {
        val header = s"${player.name} Score"
        player.renderOutput(header + "\n")
        player.renderOutput("-" * header.length + "\n")
        player.renderOutput("\n")
        listScoreTiles(game, "kong", player.score.kongs)
        player.renderOutput("\n")
        listScoreTiles(game, "pung", player.score.pungs)
        player.renderOutput("\n")
        listScoreTiles(game, "chow", player.score.chows)
        player.renderOutput("\n")
    }

    private def listScoreTiles(game: Game, setName: String, list: Seq[Seq[Tile]]) {
        if (list.length > 0) {
            player.renderOutput(s"${setName.capitalize}s (${list.length})\n")
            for (item <- list) {
                ShowTiles(player, item, true).execute(game)
            }
        } else {
            player.renderOutput(s"No ${setName}s\n")
        } 
    }
}

case class DealTile(player: Player) extends Command {

    def execute(game: Game) {
        val tile = game.dealTile
        player.takeTile(tile)
        player.renderOutput(s"${player.name} takes $tile\n")
    }
}

case class DiscardTile(player: Player, tileIndex: Int) extends Command {

    def execute(game: Game) {
        val tile = player.giveTile(tileIndex)
        game.discardTile(tile)
        game.stopWaiting
        player.renderOutput(s"${player.name} discards $tile\n")
        
    }
}

case class LastDiscardedTile(player: Player) extends Command {

    def execute(game: Game) {
        game.lastDiscardedTile match {
            case Some(tile) => {
                player.renderOutput(s"Last discarded tile was\n")
                (new ShowTiles(player, Seq(tile), true)).execute(game)
            }
            case None => player.renderOutput("No tile has been discard yet\n")
        }
    }
}

case class CallPung(player: Player, selectedTiles: Seq[Tile]) extends Command with MatchingTileFinder  {

    def execute(game: Game) {
        if (isPung(selectedTiles)) {
            player.renderOutput(s"${player.name} calls pung! (${selectedTiles.mkString(",")})\n")
            player.score.pungs += selectedTiles
            player.removeTiles(selectedTiles)
            game.setCurrentPlayer(player)
            DealTile(player).execute(game)
        } else {
            player.renderOutput("Combination is not a pung\n")
        }
    }

    private def isPung(tiles: Seq[Tile]): Boolean = isMatchingSet(tiles, 3)
}

case class CallKong(player: Player, selectedTiles: Seq[Tile]) extends Command with MatchingTileFinder {

    def execute(game: Game) {
        if (isKong(selectedTiles)) {
            player.renderOutput(s"${player.name} calls kong! (${selectedTiles.mkString(",")})\n")
            player.score.kongs += selectedTiles
            player.removeTiles(selectedTiles)
            game.setCurrentPlayer(player)
            DealTile(player).execute(game)
        } else {
            player.renderOutput("Combination is not a kong\n")
        }
    }

    private def isKong(tiles: Seq[Tile]): Boolean = isMatchingSet(tiles, 4)
}

case class CallChow(player: Player, selectedTiles: Seq[Tile]) extends Command with MatchingTileFinder {

    def execute(game: Game) {
        if (isChow(selectedTiles)) {
            player.renderOutput(s"${player.name} calls chow! (${selectedTiles.mkString(",")})\n")
            player.score.chows += selectedTiles
            player.removeTiles(selectedTiles)
            game.setCurrentPlayer(player)
            DealTile(player).execute(game)
        } else {
            player.renderOutput("Combination is not a chow\n")
        }
    }

    private def isChow(tiles: Seq[Tile]): Boolean = {
        if (!Tile.isSuitedTile(tiles.head)) {
            return false
        }
        isLinearSet(tiles, 3)
    }
}

