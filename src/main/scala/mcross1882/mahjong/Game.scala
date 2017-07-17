package mcross1882.mahjong

import scala.collection.mutable.{ArrayBuffer, HashMap}

class Game {

    type TileBuffer = ArrayBuffer[Tile]

    private val availableTiles: TileBuffer = Tile.startingTiles

    private val discardedTiles = new TileBuffer

    private var currentPlayer: Option[Player] = None

    private var isGameFinished: Boolean = false

    private var isWaiting: Boolean = false

    def dealTile(): Tile = {
        if (availableTiles.isEmpty) {
            throw new Exception("No more tiles!")
        }

        val tile = availableTiles.head
        availableTiles -= tile
        tile
    }

    def discardTile(tile: Tile) {
        discardedTiles += tile
    }

    def setCurrentPlayer(player: Player) {
        currentPlayer = Some(player)
        isWaiting = true
    }

    def isWaitingForPlayer(): Boolean = isWaiting

    def stopWaiting() {
        isWaiting = false
    }

    def lastDiscardedTile(): Option[Tile] = discardedTiles.lastOption

    def isFinished(): Boolean = isGameFinished

    def finish() {
        isGameFinished = true
        isWaiting = false
    }

    def reset() {
        isGameFinished = false
        isWaiting = false
    }
}
