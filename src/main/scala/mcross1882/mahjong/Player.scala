package mcross1882.mahjong

import scala.collection.mutable.ArrayBuffer

class Player(n: String) {

    private val tileBuffer = new ArrayBuffer[Tile]

    private var score: Int = 0

    def name(): String = n

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

    def groupedTiles(lastDiscardedTile: Tile): Seq[Tile] = {
        tiles.groupBy(_.category).values.flatMap(x => x).toSeq ++ Seq(lastDiscardedTile)
    }

    def nextCommand(game: Game): Command = CommandFactory.create(StdIn.readLine(s"${name}> "))

    def printTiles() {
        val numberOfTiles = tileBuffer.length

        for (index <- 0 until numberOfTiles) {
            print(" +--+ ")
        }
        println

        for (index <- 0 until numberOfTiles) {
            print(s" |${tiles(index).value}| ")
        }
        println

        for (index <- 0 until numberOfTiles) {
            print(" |--| ")
        }
        println

        for (index <- 0 until numberOfTiles) {
            print(s" |${tiles(index).category}| ")
        }
        println

        for (index <- 0 until numberOfTiles) {
            print(" +--+ ")
        }
        println

        for (index <- 0 until numberOfTiles) {
            print(f"  ${index}%2d  ")
        }
        println
    }
}

