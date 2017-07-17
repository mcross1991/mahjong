package mcross1882.mahjong

import scala.io.StdIn

trait InputController {

    def requestNextCommand(player: Player): Command

    def requestCallCommand(player: Player, groupedTiles: Seq[Seq[Tile]]): Command
}


class ConsoleController(factory: CommandFactory) extends InputController {

    def requestNextCommand(player: Player): Command = {
        factory.create(player, StdIn.readLine(s"${player.name}> ")) match {
            case Some(p) => p
            case None => requestNextCommand(player)
        }
    }

    def requestCallCommand(player: Player, groupedTiles: Seq[Seq[Tile]]): Command = {
        factory.create(player, StdIn.readLine(s"${player.name}> ")) match {
            case Some(p: DiscardTile) => requestCallCommand(player, groupedTiles)
            case Some(p) => p
            case None => requestCallCommand(player, groupedTiles)
        }
    }
}

class BotController(factory: CommandFactory) extends InputController with MatchingTileFinder {

    type TileGroups = Seq[Seq[Tile]]

    def requestNextCommand(player: Player): Command = {
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

        DiscardTile(player, findSmallestTile(player, groups))
    }

    def requestCallCommand(player: Player, groups: TileGroups): Command = {
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

    private def findKongs(groups: TileGroups): Option[Seq[Tile]] = {
        groups.map(x => extractMatchingSet(x, 4)).filter(!_.isEmpty).headOption
    }

    private def findPungs(groups: TileGroups): Option[Seq[Tile]] = {
        groups.map(x => extractMatchingSet(x, 3)).filter(!_.isEmpty).headOption
    }

    private def findChows(groups: TileGroups): Option[Seq[Tile]] = {
        groups.map(x => extractLinearSet(x, 3)).filter(!_.isEmpty).headOption
    }

    private def findSmallestTile(player: Player, groups: TileGroups): Int = {
        var smallestTile: Tile = groups.head.head
        var smallestValue: Int = 4        
        for (index <- 0 until groups.length) {
            if (groups(index).length < smallestValue) {
                smallestTile = groups(index).head
                smallestValue = groups(index).length
            }
        }

        player.tileIndex(smallestTile)
    }
}

