package mcross1882.mahjong

import scala.collection.mutable.{ArrayBuffer, HashMap}

class Game {

    type TileBuffer = ArrayBuffer[Tile]

    private val availableTiles: TileBuffer = Tile.startingTiles

    private val discardedTiles = new TileBuffer

    def dealTile(): Tile = {
        val tile = availableTiles.head
        availableTiles -= tile
        tile
    }

    def discardTile(tile: Tile) {
        discardedTiles += tile
    }

    def isPung(tiles: Seq[Tile]): Boolean = isMatchingSet(tiles, 3)

    def isKong(tiles: Seq[Tile]): Boolean = isMatchingSet(tiles, 4)

    private def isMatchingSet(tiles: Seq[Tile], requiredCount: Int): Boolean = {
        if (tiles.length != requiredCount) {
            return false
        }

        val firstTile = tiles.head
        for (index <- 1 until tiles.length) {
            if (firstTile != tiles(index)) {
                return false
            }
        }
        true
    }
}
