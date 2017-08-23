/**
  * Copyright (C) 2017-2018 the original author or authors.
  * See the LICENSE file distributed with this work for additional
  * information regarding copyright ownership.
  *
  * @author Matthew Cross <github.com/mcross1991>
  */
package mahjong

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
        printTiles(game)
    }

    private def printTiles(game: Game) {
        val numberOfTiles = tiles.length

        for (index <- 0 until numberOfTiles) {
            game.render(" +--+ ")
        }
        game.renderLine("")

        for (index <- 0 until numberOfTiles) {
            game.render(s" |${tiles(index).value}| ")
        }
        game.renderLine("")

        for (index <- 0 until numberOfTiles) {
            game.render(" |--| ")
        }
        game.renderLine("")

        for (index <- 0 until numberOfTiles) {
            game.render(s" |${tiles(index).category}| ")
        }
        game.renderLine("")

        for (index <- 0 until numberOfTiles) {
            game.render(" +--+ ")
        }
        game.renderLine("")

        if (!hideIndicies) {
            for (index <- 0 until numberOfTiles) {
                game.render(f"  ${index}%2d  ")
            }
            game.renderLine("")
        }
    }
}

case class ShowScore(player: Player) extends Command {

    def execute(game: Game) {
        val header = s"${player.name} Score"
        game.renderLine(header)
        game.renderLine("-" * header.length)
        game.renderLine("")
        listScoreTiles(game, "kong", player.score.kongs)
        game.renderLine("")
        listScoreTiles(game, "pung", player.score.pungs)
        game.renderLine("")
        listScoreTiles(game, "chow", player.score.chows)
        game.renderLine("")
    }

    private def listScoreTiles(game: Game, setName: String, list: Seq[Seq[Tile]]) {
        if (list.length > 0) {
            game.renderLine(s"${setName.capitalize}s (${list.length})")
            for (item <- list) {
                ShowTiles(player, item, true).execute(game)
            }
        } else {
            game.renderLine(s"No ${setName}s")
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
            game.renderLine(s"${player.name} calls mahjong!")
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
            case None => game.renderLine("No tiles have been discarded yet, skipping")
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
            game.renderLine(s"${player.name} calls mahjong!")
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
        game.renderLine(s"${player.name} takes $tile")
    }
}

case class DiscardTile(player: Player, tileIndex: Int) extends Command {

    def execute(game: Game) {
        val tile = player.giveTile(tileIndex)
        game.discardTile(tile)
        game.stopWaiting
        game.renderLine(s"${player.name} discards $tile")
    }
}

case class LastDiscardedTile(player: Player) extends Command {

    def execute(game: Game) {
        game.lastDiscardedTile match {
            case Some(tile) => {
                game.renderLine(s"Last discarded tile was")
                (new ShowTiles(player, Seq(tile), true)).execute(game)
            }
            case None => game.renderLine("No tile has been discard yet")
        }
    }
}

case class CallPung(player: Player, selectedTiles: Seq[Tile]) extends Command with MatchingTileFinder  {

    def execute(game: Game) {
        if (isPung(selectedTiles)) {
            game.renderLine(s"${player.name} calls pung! (${selectedTiles.mkString(",")})")
            player.score.pungs += selectedTiles
            player.removeTiles(selectedTiles)
            game.setCurrentPlayer(player)
            DealTile(player).execute(game)
        } else {
            game.renderLine("Combination is not a pung")
        }
    }

    private def isPung(tiles: Seq[Tile]): Boolean = isMatchingSet(tiles, 3)
}

case class CallKong(player: Player, selectedTiles: Seq[Tile]) extends Command with MatchingTileFinder {

    def execute(game: Game) {
        if (isKong(selectedTiles)) {
            game.renderLine(s"${player.name} calls kong! (${selectedTiles.mkString(",")})")
            player.score.kongs += selectedTiles
            player.removeTiles(selectedTiles)
            game.setCurrentPlayer(player)
            DealTile(player).execute(game)
        } else {
            game.renderLine("Combination is not a kong")
        }
    }

    private def isKong(tiles: Seq[Tile]): Boolean = isMatchingSet(tiles, 4)
}

case class CallChow(player: Player, selectedTiles: Seq[Tile]) extends Command with MatchingTileFinder {

    def execute(game: Game) {
        if (isChow(selectedTiles)) {
            game.renderLine(s"${player.name} calls chow! (${selectedTiles.mkString(",")})")
            player.score.chows += selectedTiles
            player.removeTiles(selectedTiles)
            game.setCurrentPlayer(player)
            DealTile(player).execute(game)
        } else {
            game.renderLine("Combination is not a chow")
        }
    }

    private def isChow(tiles: Seq[Tile]): Boolean = {
        if (!Tile.isSuitedTile(tiles.head)) {
            return false
        }
        isLinearSet(tiles, 3)
    }
}

