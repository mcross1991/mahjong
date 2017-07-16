package mcross1882.mahjong

import scala.collection.mutable.ArrayBuffer

object Player {

    def create(name: String, factory: CommandFactory): Player = Player(name, new Score, new ConsoleController(factory))

    def createBot(name: String, factory: CommandFactory): Player = Player(name, new Score, new BotController(factory))
}

class Score {

    private val kongs = new ArrayBuffer[Seq[Tile]]

    private val pungs = new ArrayBuffer[Seq[Tile]]

    private val chows = new ArrayBuffer[Seq[Tile]]

    def addKong(kong: Seq[Tile]) {
        kongs += kong
    }

    def addPung(pung: Seq[Tile]) {
        pungs += pung
    }

    def addChow(chow: Seq[Tile]) {
        chows += chow
    }

    def listKongs(): Seq[Seq[Tile]] = kongs.toSeq

    def listPungs(): Seq[Seq[Tile]] = pungs.toSeq

    def listChows(): Seq[Seq[Tile]] = chows.toSeq
}

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

    def groupedTiles(lastDiscardedTile: Option[Tile]): Seq[Seq[Tile]] = {
        tiles.groupBy(_.category).values.toSeq
    }

    def nextCommand(): Command = {
        controller.listenForCommand(this)
    }
}

