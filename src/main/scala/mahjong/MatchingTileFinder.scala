/**
  * Copyright (C) 2017-2018 the original author or authors.
  * See the LICENSE file distributed with this work for additional
  * information regarding copyright ownership.
  *
  * @author Matthew Cross <github.com/mcross1991>
  */
package mahjong

import scala.collection.mutable.ArrayBuffer

trait MatchingTileFinder {

    protected def extractMatchingSet(tiles: Seq[Tile], requiredCount: Int): Seq[Tile] = {
        val empty = Seq.empty[Tile]
        if (tiles.length < requiredCount) {
            return empty
        }

        for (tile <- tiles) {
            val count = tiles.count(_ == tile)
            if (count == requiredCount) {
                return tiles.filter(_ == tile).toSeq
            }
        }
        empty
    }

    protected def extractLinearSet(tiles: Seq[Tile], requiredCount: Int): Seq[Tile] = {
        val empty = Seq.empty[Tile]
        if (tiles.length < requiredCount) {
            return empty
        }

        val buffer = new ArrayBuffer[Tile]

        for (tile <- tiles) {
            var firstValue = tile.intValue

            buffer.clear
            for (index <- firstValue until firstValue + requiredCount) {
                tiles.find(_.intValue == index) match {
                    case Some(t) => buffer.append(t)
                    case None => // noop
                }
            }

            if (buffer.length == requiredCount) {
                return buffer.toSeq
            }
        }

        empty
    }

    protected def isMatchingSet(tiles: Seq[Tile], requiredCount: Int): Boolean = {
        if (tiles.length != requiredCount) {
            return false
        }

        val firstTile = tiles.head
        for (index <- 1 until requiredCount) {
            if (firstTile != tiles(index)) {
                return false
            }
        }
        true
    }

    protected def isLinearSet(tiles: Seq[Tile], requiredCount: Int): Boolean = {
        if (tiles.length != requiredCount) {
            return false
        }

        var currentValue = tiles.head.intValue
        for (index <- 1 until requiredCount) {
            if (currentValue != (tiles(index).intValue - 1)) {
                 return false
            }
            currentValue = tiles(index).intValue
        }
        true
    }
}
