/**
  * Copyright (C) 2017-2018 the original author or authors.
  * See the LICENSE file distributed with this work for additional
  * information regarding copyright ownership.
  *
  * @author Matthew Cross <github.com/mcross1991>
  */
package mahjong

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

case class Tile(category: String, value: String) {

    def intValue(): Int = Tile.NUMERALS.indexOf(value) + 1

    override def toString(): String = s"$value$category"
}

object Tile {

    val NUMBER_OF_TILES_PER_GROUP = 4

    val NUMERALS = Seq("一", "二", "三", "四", "五", "六", "七", "八", "九")
    val DIRECTIONS = Seq("北", "东", "南", "西")
    val CATEGORY_TIAO = "条"
    val CATEGORY_WAN  = "万"
    val CATEGORY_TONG = "筒"
    val CATEGORY_FENG = "风"

    def isSuitedTile(tile: Tile): Boolean = {
        tile.category == CATEGORY_WAN || tile.category == CATEGORY_TIAO || tile.category == CATEGORY_TONG
    }

    def startingTiles(): ArrayBuffer[Tile] = {
        val buffer = new ArrayBuffer[Tile]
        buffer ++= createTileGroup(Tile("中", "红"))
        buffer ++= createTileGroup(Tile("发", "青"))
        buffer ++= createTileGroup(Tile("板", "白"))
        createTileRange(CATEGORY_TIAO, NUMERALS, buffer)
        createTileRange(CATEGORY_WAN, NUMERALS, buffer)
        createTileRange(CATEGORY_TONG, NUMERALS, buffer)
        createTileRange(CATEGORY_FENG, DIRECTIONS, buffer)
        Random.shuffle(buffer)
    }

    private def createTileRange(category: String, values: Seq[String], buffer: ArrayBuffer[Tile]) {
        for (value <- values) {
            for (repeat <- 0 until NUMBER_OF_TILES_PER_GROUP) {
                buffer.append(Tile(category, value))
            }
        }
    }

    private def createTileGroup(tile: Tile): Seq[Tile] = Seq.fill(NUMBER_OF_TILES_PER_GROUP)(tile)
}


