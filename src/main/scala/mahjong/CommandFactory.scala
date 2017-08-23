/**
  * Copyright (C) 2017-2018 the original author or authors.
  * See the LICENSE file distributed with this work for additional
  * information regarding copyright ownership.
  *
  * @author Matthew Cross <github.com/mcross1991>
  */
package mahjong

class InvalidCommandException extends Exception("Invalid command given")

class CommandFactory {

    def create(player: Player, line: String): Option[Command] = {
        cleanInput(line) match {
            case Array("pung", tileA, tileB, tileC) => {
                Some(CallPung(player, findTiles(player, tileA, tileB, tileC)))
            }
            case Array("kong", tileA, tileB, tileC, tileD) => {
                Some(CallKong(player, findTiles(player, tileA, tileB, tileC, tileD)))
            }
            case Array("chow", tileA, tileB, tileC) => {
                Some(CallChow(player, findTiles(player, tileA, tileB, tileC)))
            }
            case Array("show") => {
                Some(ShowTiles(player, player.tiles, false))
            }
            case Array("show", "groups") => {
                Some(ShowTiles(player, player.groupedTiles().flatMap(x => x), true))
            }
            case Array("show", "score") => {
                Some(ShowScore(player))
            }
            case Array("last") => {
                Some(LastDiscardedTile(player))
            }
            case Array("discard", index) => {
                Some(DiscardTile(player, index.toInt))
            }
            case Array("skip") => Some(SkipCommand())
            case Array("exit") => Some(ExitGame())
            case _ => None
        }
    }

    private def cleanInput(line: String): Array[String] = line.toLowerCase.trim.split(" ")

    private def findTiles(player: Player, indices: String*): Seq[Tile] = {
        indices.map(x => player.tiles(x.toInt))
    }
}
