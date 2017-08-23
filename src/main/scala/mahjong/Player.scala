/**
  * Copyright (C) 2017-2018 the original author or authors.
  * See the LICENSE file distributed with this work for additional
  * information regarding copyright ownership.
  *
  * @author Matthew Cross <github.com/mcross1991>
  */
package mahjong

import scala.collection.mutable.ArrayBuffer

class MissingTileException extends Exception("The selected tile does not exist")

object Player {

    private val factory = new CommandFactory

    def create(name: String): Player = Player(name, createScore, new ConsoleController(factory))

    def createRemote(name: String, port: Int): Player = Player(name, createScore, new SocketController(factory, port))

    def createBot(name: String): Player = Player(name, createScore, new BotController(factory))

    def createScore(): Score = new Score(new ArrayBuffer[Seq[Tile]], new ArrayBuffer[Seq[Tile]], new ArrayBuffer[Seq[Tile]])
}

case class Score(kongs: ArrayBuffer[Seq[Tile]], pungs: ArrayBuffer[Seq[Tile]], chows: ArrayBuffer[Seq[Tile]])

case class Player(name: String, score: Score, controller: Controller) {

    private val tileBuffer = new ArrayBuffer[Tile]

    def canCallMahjong(): Boolean = {
        tileBuffer.isEmpty || (tileBuffer.length == 2 && tileBuffer(0) == tileBuffer(1))
    }

    def takeTile(tile: Tile) {
        tileBuffer += tile
    }

    def giveTile(tileIndex: Int): Tile = {
        if (!tileBuffer.isDefinedAt(tileIndex)) {
            throw new MissingTileException()
        }

        val tile = tileBuffer(tileIndex)
        tileBuffer.remove(tileIndex)
        tile
    }

    def removeTiles(tiles: Seq[Tile]) {
        tileBuffer --= tiles
    }

    def tiles(): Seq[Tile] = tileBuffer.toSeq

    def tiles(index: Int): Tile = tileBuffer(index)

    def tileIndex(tile: Tile): Int = tileBuffer.indexOf(tile)

    def groupedTiles(): Seq[Seq[Tile]] = {
        tileBuffer.groupBy(_.category).map(_._2.sortBy(_.intValue)).toSeq
    }

    def nextCommand(): Command = {
        controller.requestNextCommand(this)
    }

    def checkLastTile(lastTile: Tile): Command = {
        val grouped = (tiles ++ Seq(lastTile)).groupBy(_.category).map(_._2.sortBy(_.intValue)).toSeq
        controller.requestCallCommand(this, grouped)
    }

    def render(message: String) {
        controller.render(this, message)
    }

    def renderLine(message: String) {
        controller.renderLine(this, message)
    }
}

