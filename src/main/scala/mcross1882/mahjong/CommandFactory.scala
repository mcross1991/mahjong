package mcross1882.mahjong

class InvalidCommandException extends Exception("Invalid command given")

class CommandFactory {

    def create(player: Player, game: Game, line: String): Option[Command] = {
        val command = cleanInput(line) match {
            case Array("pung", tileA, tileB, tileC) => CallPung(player, findTiles(player, game, tileA, tileB, tileC))
            case Array("kong", tileA, tileB, tileC, tileD) => CallKong(player, findTiles(player, game, tileA, tileB, tileC, tileD))
            case Array("chow", tileA, tileB, tileC) => CallChow(player, findTiles(player, game, tileA, tileB, tileC))
            case Array("show") => ShowTiles(player, player.tiles, false)
            case Array("show", "groups") => ShowTiles(player, player.groupedTiles().flatMap(x => x), true)
            case Array("show", "score") => ShowScore(player)
            case Array("last") => LastDiscardedTile(player)
            case Array("discard", index) => DiscardTile(player, index.toInt)
            case Array("skip") => SkipCommand()
            case Array("exit") => ExitGame
            case _ => null
        }

        if (null == command) {
            return None
        }
        Some(command)
    }

    private def cleanInput(line: String): Array[String] = line.toLowerCase.trim.split(" ")

    private def findTiles(player: Player, game: Game, indices: String*): Seq[Tile] = {
        val lastTile = game.lastDiscardedTile

        indices.map{ x =>
            if ("last" == x) {
                game.lastDiscardedTile match {
                    case Some(tile) => tile
                    case None => throw new Exception("No last tile to select")
                }
            } else {
                player.tiles(x.toInt)
            }
        }
    }
}
