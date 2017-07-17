package mcross1882.mahjong

import scala.io.StdIn

trait InputController {

    def requestNextCommand(player: Player, game: Game): Command

    def requestCallCommand(player: Player, groupedTiles: Seq[Seq[Tile]], game: Game): Command
}


class ConsoleController(factory: CommandFactory) extends InputController {

    def requestNextCommand(player: Player, game: Game): Command = {
        factory.create(player, game, StdIn.readLine(s"${player.name}> ")) match {
            case Some(p) => p
            case None => requestNextCommand(player, game)
        }
    }

    def requestCallCommand(player: Player, groupedTiles: Seq[Seq[Tile]], game: Game): Command = {
        factory.create(player, game, StdIn.readLine(s"${player.name}> ")) match {
            case Some(p: DiscardTile) => requestCallCommand(player, groupedTiles, game)
            case Some(p) => p
            case None => requestCallCommand(player, groupedTiles, game)
        }
    }
}

class BotController(factory: CommandFactory) extends InputController with MatchingTileFinder {

    type TileGroups = Seq[Seq[Tile]]

    def requestNextCommand(player: Player, game: Game): Command = {
        val groups = player.groupedTiles
        findKongs(groups) match {
            case Some(list) => return CallKong(player, list)
            case None => // noop
        }

        findPungs(groups) match {
            case Some(list) => return CallPung(player, list)
            case None => // noop
        }

        findChows(groups) match {
            case Some(list) => return CallChow(player, list)
            case None => // noop
        }

        DiscardTile(player, findSmallestTile(groups))
    }

    def requestCallCommand(player: Player, groups: Seq[Seq[Tile]], game: Game): Command = {
        findKongs(groups) match {
            case Some(list) => return CallKong(player, list)
            case None => // noop
        }

        findPungs(groups) match {
            case Some(list) => return CallPung(player, list)
            case None => // noop
        }

        findChows(groups) match {
            case Some(list) => return CallChow(player, list)
            case None => // noop
        }

        SkipCommand()
    }

    private def findPungs(groups: TileGroups): Option[Seq[Tile]] = {
        groups.filter(x => x.length == 3 && isMatchingSet(x, 3)).headOption
    }

    private def findKongs(groups: TileGroups): Option[Seq[Tile]] = {
        groups.filter(x => x.length == 4 && isMatchingSet(x, 4)).headOption
    }

    private def findChows(groups: TileGroups): Option[Seq[Tile]] = {
        groups.filter(x => x.length == 3 && isLinearSet(x, 3)).headOption
    }

    private def findSmallestTile(groups: TileGroups): Int = {
        var smallestTileIndex: Int = 0
        var smallestValue: Int = 4        
        for (index <- 0 until groups.length) {
            if (groups(index).length < smallestValue) {
                smallestTileIndex = index
                smallestValue = groups(index).length
            }
        }
        smallestTileIndex
    }
}

