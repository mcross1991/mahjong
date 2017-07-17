package mcross1882.mahjong

import scala.collection.mutable.ArrayBuffer

object Player {

    def create(name: String, factory: CommandFactory): Player = Player(name, createScore, new ConsoleController(factory))

    def createBot(name: String, factory: CommandFactory): Player = Player(name, createScore, new BotController(factory))

    def createScore(): Score = new Score(new ArrayBuffer[Seq[Tile]], new ArrayBuffer[Seq[Tile]], new ArrayBuffer[Seq[Tile]])
}

case class Score(kongs: ArrayBuffer[Seq[Tile]], pungs: ArrayBuffer[Seq[Tile]], chows: ArrayBuffer[Seq[Tile]])

case class Player(name: String, score: Score, controller: InputController) {

    private val tileBuffer = new ArrayBuffer[Tile]

    def takeTile(tile: Tile) {
        tileBuffer += tile
    }

    def giveTile(tileIndex: Int): Tile = {
        val tile = tileBuffer(tileIndex)
        tileBuffer.remove(tileIndex)
        tile
    }

    def removeTiles(tiles: Seq[Tile]) {
        tileBuffer --= tiles
    }

    def tiles(): Seq[Tile] = tileBuffer.toSeq

    def tiles(index: Int): Tile = tileBuffer(index)

    def groupedTiles(): Seq[Seq[Tile]] = {
        tileBuffer.groupBy(_.category).map(_._2.sortBy(_.intValue)).toSeq
    }

    def nextCommand(game: Game): Command = {
        controller.requestNextCommand(this, game)
    }

    def checkLastTile(game: Game, lastTile: Tile): Command = {
        val grouped = (tiles ++ Seq(lastTile)).groupBy(_.category).map(_._2.sortBy(_.intValue)).toSeq
        controller.requestCallCommand(this, grouped, game)
    }
}

