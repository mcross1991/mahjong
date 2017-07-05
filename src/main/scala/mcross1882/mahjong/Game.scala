package mcross1882.mahjong

import scala.collection.mutable.{ArrayBuffer, HashMap}

class Game {

    type TileBuffer = ArrayBuffer[Tile]

    private val availableTiles: TileBuffer = Tile.startingTiles

    private val discardedTiles = new TileBuffer

    private var currentPlayer: String = ""

    private var isGameFinished: Boolean = false

    def dealTile(): Tile = {
        val tile = availableTiles.head
        availableTiles -= tile
        tile
    }

    def discardTile(tile: Tile) {
        discardedTiles += tile
    }

    def setCurrentPlayer(player: Player) {
        currentPlayer = player.name
    }

    def isCurrentPlayer(player: Player): Boolean = player.name == currentPlayer

    def lastDiscardedTile(): Tile = discardedTiles.last

    def isFinished(): Boolean = isGameFinished

    def finish() {
        isGameFinished = true
    }

    def reset() {
        isGameFinished = false
    }
}
