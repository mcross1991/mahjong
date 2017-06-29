package mcross1882.mahjong

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

case class Tile(category: String, value: String) {

    override def toString(): String = s"$value$category"
}

object Tile {

    private val numerals = Seq("一", "二", "三", "四", "五", "六", "七", "八", "九")

    def startingTiles(): ArrayBuffer[Tile] = {
        val buffer = new ArrayBuffer[Tile]
        buffer ++= Seq.fill(4)(Tile("中", "红"))
        buffer ++= Seq.fill(4)(Tile("发", "青"))
        buffer ++= Seq.fill(4)(Tile("板", "白"))
        tileRange("条", numerals, buffer)
        tileRange("万", numerals, buffer)
        tileRange("筒", numerals, buffer)
        tileRange("风", Seq("北", "东", "南", "西"), buffer)
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


