package mcross1882.mahjong

class InvalidCommandException extends Exception("Invalid command given")

class CommandFactory {

    def create(player: Player, line: String): Option[Command] = {
        val command: Command = cleanInput(line) match {
            case Array("pung", tileA, tileB, tileC) => CallPung(player, findTiles(player, tileA, tileB, tileC))
            case Array("kong", tileA, tileB, tileC, tileD) => CallKong(player, findTiles(player, tileA, tileB, tileC, tileD))
            case Array("chow", tileA, tileB, tileC) => CallChow(player, findTiles(player, tileA, tileB, tileC))
            case Array("show") => ShowTiles(player, player.tiles, false)
            case Array("show", "groups") => ShowTiles(player, player.groupedTiles().flatMap(x => x), true)
            case Array("show", "score") => ShowScore(player)
            case Array("last") => LastDiscardedTile(player)
            case Array("discard", index) => DiscardTile(player, index.toInt)
            case Array("skip") => SkipCommand()
            case Array("exit") => ExitGame()
            case _ => null
        }

        if (null == command) {
            return None
        }
        Some(command)
    }

    private def cleanInput(line: String): Array[String] = line.toLowerCase.trim.split(" ")

    private def findTiles(player: Player, indices: String*): Seq[Tile] = {
        indices.map(x => player.tiles(x.toInt))
    }
}
