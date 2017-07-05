package mcross1882.mahjong

class InvalidCommandException extends Exception("Invalid command given")

class CommandFactory(game: Game) {

    def create(player: Player, line: String): Command = {
        line.toLowerCase.trim.split(" ") match {
            case Array("pung", tileA, tileB, tileC) => new CallPung(player, findTiles(player, tileA, tileB, tileC))
            case Array("kong", tileA, tileB, tileC, tileD) => new CallKong(player, findTiles(player, tileA, tileB, tileC, tileD))
            case Array("show") => new ShowTiles(player)
            case Array("last") => new LastDiscardedTile(player)
            case Array("discard", index) => new DiscardTile(player, index.toInt)
            case Array("deal") => new DealTile(player)
            case _ => throw new InvalidCommandException()
        }
    }

    private def findTiles(player: Player, indices: String*): Seq[Tile] = {
        indices.map{ x =>
            if ("last" == x) {
                game.lastDiscardedTile
            } else {
                player.tiles(x.toInt)
            }
        }
    }
}
