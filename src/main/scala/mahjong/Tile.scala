package mahjong

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

case class Tile(category: String, value: String) {

    def intValue(): Int = Tile.NUMERALS.indexOf(value) + 1

    override def toString(): String = s"$value$category"
}

object Tile {

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
        buffer ++= Seq.fill(4)(Tile("中", "红"))
        buffer ++= Seq.fill(4)(Tile("发", "青"))
        buffer ++= Seq.fill(4)(Tile("板", "白"))
        tileRange(CATEGORY_TIAO, NUMERALS, buffer)
        tileRange(CATEGORY_WAN, NUMERALS, buffer)
        tileRange(CATEGORY_TONG, NUMERALS, buffer)
        tileRange(CATEGORY_FENG, DIRECTIONS, buffer)
        Random.shuffle(buffer)
    }

    private def tileRange(category: String, values: Seq[String], buffer: ArrayBuffer[Tile]) {
        for (value <- values) {
            for (repeat <- 0 until 4) {
                buffer.append(Tile(category, value))
            }
        }
    }
}


