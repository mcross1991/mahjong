/**
  * Copyright (C) 2017-2018 the original author or authors.
  * See the LICENSE file distributed with this work for additional
  * information regarding copyright ownership.
  *
  * @author Matthew Cross <github.com/mcross1991>
  */
package mahjong

import scala.collection.mutable.{ArrayBuffer, HashMap}

class NoMoreTilesException extends Exception("No more tiles!")

class Game(players: Seq[Player]) {

    type TileBuffer = ArrayBuffer[Tile]

    private val availableTiles: TileBuffer = Tile.startingTiles

    private val discardedTiles = new TileBuffer

    private var currentPlayer: Int = 0

    private var isGameFinished: Boolean = false

    private var isWaiting: Boolean = true

    def dealTile(): Tile = {
        if (availableTiles.isEmpty) {
            throw new NoMoreTilesException()
        }

        val tile = availableTiles.head
        availableTiles -= tile
        tile
    }

    def discardTile(tile: Tile) {
        discardedTiles += tile
    }

    def setCurrentPlayer(player: Player) {
        currentPlayer = players.indexOf(player)
        isWaiting = true
    }

    def gotoNextPlayer() {
        if (currentPlayer < players.length) {
            currentPlayer += 1
        } else {
            currentPlayer = 0
        }
    }

    def allPlayers(): Seq[Player] = players

    def getCurrentPlayer(): Player = players(currentPlayer)

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

    def render(message: String) {
        players.foreach(_.render(message))
    }

    def renderLine(message: String) {
        players.foreach(_.renderLine(message))
    }
}
