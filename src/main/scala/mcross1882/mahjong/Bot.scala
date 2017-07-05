package mcross1882.mahjong

import scala.collection.mutable.ArrayBuffer

class Bot(n: String) extends Player(n) {

    def nextMove(game: Game): String = {
        val groups = groupedTiles(game.lastDiscardedTile)
        val kongs = findKongs(groups)
        if (!kongs.isEmpty) {
            return s"kong ${kongs.mkString(" ")}"
        }

        val pungs = findPungs(groups)
        if (!pungs.isEmpty) {
            return s"pung ${pungs.mkString(" ")}"
        }

        ""
    }

    private def findPungs(groups: TileBuffer): Seq[Int] = {
        groups.findWhere(x => x.length == 3)
    }

    private def findKongs(groups: TileBuffer): Seq[Int] = {
        groups.findWhere(x => x.length == 4)
    }
}

