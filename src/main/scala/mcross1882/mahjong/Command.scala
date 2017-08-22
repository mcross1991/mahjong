package mcross1882.mahjong

trait Command {

    def execute(game: Game)
}

case class SkipCommand() extends Command {

    def execute(game: Game) {
        game.stopWaiting
    }
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
        listScoreTiles(game, "kong", player.score.kongs)
        println
        listScoreTiles(game, "pung", player.score.pungs)
        println
        listScoreTiles(game, "chow", player.score.chows)
        println
    }

    private def listScoreTiles(game: Game, setName: String, list: Seq[Seq[Tile]]) {
        if (list.length > 0) {
            println(s"${setName.capitalize}s (${list.length})")
            for (item <- list) {
                ShowTiles(player, item, true).execute(game)
            }
        } else {
            println(s"No ${setName}s")
        } 
    }
}

case class SetNextPlayer(player: Player) extends Command {

    def execute(game: Game) {
        game.setCurrentPlayer(player)
    }
}

case class AskPlayer(player: Player) extends Command {

    def execute(game: Game) { 
        while (game.isWaitingForPlayer) {
            player.nextCommand.execute(game)
        }

        if (player.canCallMahjong) {
            println(s"${player.name} calls mahjong!")
            game.finish
        }
    }
}

case class AskOtherPlayers() extends Command {

    def execute(game: Game) {
        val currentPlayer = game.getCurrentPlayer
        game.allPlayers
            .filter(_ != currentPlayer)
            .map(p => CheckDiscardedTile(p))
            .foreach(_.execute(game))
    }
}

case class CheckDiscardedTile(player: Player) extends Command {

    def execute(game: Game) {
        game.lastDiscardedTile match {
            case Some(lastTile) => checkIfPlayerCanCall(lastTile, game)
            case None => println("No tiles have been discarded yet, skipping")
        }
    }

    private def checkIfPlayerCanCall(lastTile: Tile, game: Game) {
        player.checkLastTile(lastTile) match {
            case c: SkipCommand => c.execute(game)
            case c: CallKong => c.execute(game)
            case c: CallPung => c.execute(game)
            case c: CallChow => c.execute(game)
            case _ => CheckDiscardedTile(player).execute(game)
        }

        if (player.canCallMahjong) {
            println(s"${player.name} calls mahjong!")
            game.finish
        }
    }
}

case class DealStartingTiles() extends Command {

    def execute(game: Game) {
        val players = game.allPlayers
        for (index <- 0 until 13) {
            for (player <- players) {
                DealTile(player).execute(game)
            }
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
        game.lastDiscardedTile match {
            case Some(tile) => {
                println(s"Last discarded tile was")
                (new ShowTiles(player, Seq(tile), true)).execute(game)
            }
            case None => println("No tile has been discard yet")
        }
    }
}

case class CallPung(player: Player, selectedTiles: Seq[Tile]) extends Command with MatchingTileFinder  {

    def execute(game: Game) {
        if (isPung(selectedTiles)) {
            println(s"${player.name} calls pung! (${selectedTiles.mkString(",")})")
            player.score.pungs += selectedTiles
            player.removeTiles(selectedTiles)
            game.setCurrentPlayer(player)
            DealTile(player).execute(game)
        } else {
            println("Combination is not a pung")
        }
    }

    private def isPung(tiles: Seq[Tile]): Boolean = isMatchingSet(tiles, 3)
}

case class CallKong(player: Player, selectedTiles: Seq[Tile]) extends Command with MatchingTileFinder {

    def execute(game: Game) {
        if (isKong(selectedTiles)) {
            println(s"${player.name} calls kong! (${selectedTiles.mkString(",")})")
            player.score.kongs += selectedTiles
            player.removeTiles(selectedTiles)
            game.setCurrentPlayer(player)
            DealTile(player).execute(game)
        } else {
            println("Combination is not a kong")
        }
    }

    private def isKong(tiles: Seq[Tile]): Boolean = isMatchingSet(tiles, 4)
}

case class CallChow(player: Player, selectedTiles: Seq[Tile]) extends Command with MatchingTileFinder {

    def execute(game: Game) {
        if (isChow(selectedTiles)) {
            println(s"${player.name} calls chow! (${selectedTiles.mkString(",")})")
            player.score.chows += selectedTiles
            player.removeTiles(selectedTiles)
            game.setCurrentPlayer(player)
            DealTile(player).execute(game)
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

