package mcross1882.mahjong

import scala.io.StdIn

trait InputController {

    def listenForCommand(player: Player): Command
}


class ConsoleController(factory: CommandFactory) extends InputController {

    def listenForCommand(player: Player): Command = {
        factory.create(player, StdIn.readLine(s"${player.name}> "))
    }
}

class BotController(factory: CommandFactory) extends InputController with MatchingTileFinder {

    type TileGroups = Seq[Seq[Tile]]

    def listenForCommand(player: Player): Command = {
        val groups = player.groupedTiles(None)
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

        val tileIndex = findSmallestTile(groups)
        new DiscardTile(player, tileIndex)
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

