package mcross1882.mahjong

import scala.collection.mutable.ArrayBuffer

class Player(name: String) {

    private val tileBuffer = new ArrayBuffer[Tile]

    private var score: Int = 0

    def addScore(amount: Int) {
        score += amount
    }

    def takeTile(tile: Tile) {
        tileBuffer += tile
    }

    def giveTile(tileIndex: Int): Tile = {
        val tile = tileBuffer(tileIndex)
        tileBuffer.remove(tileIndex)
        tile
    }

    def tiles(): Seq[Tile] = tileBuffer.toSeq

    def tiles(index: Int): Tile = tileBuffer(index)

    def groupedTiles(): Seq[Tile] = tiles.groupBy(_.category).values.flatMap(x => x).toSeq
}

