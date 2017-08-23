package mcross1882.mahjong
package test

import org.scalatest.{FlatSpec, Matchers}

class TileSpec extends BaseSpec {

    "A Tile" should "determine if another tile is a suited tile" in {
        Tile.isSuitedTile(Tile(Tile.CATEGORY_TIAO, "条")) should be(true)
        Tile.isSuitedTile(Tile(Tile.CATEGORY_WAN, "万")) should be(true)
        Tile.isSuitedTile(Tile(Tile.CATEGORY_TONG, "筒")) should be(true)
        Tile.isSuitedTile(Tile(Tile.CATEGORY_FENG, "风")) should be(false)
        Tile.isSuitedTile(Tile("中", "红")) should be(false)
        Tile.isSuitedTile(Tile("发", "青")) should be(false)
        Tile.isSuitedTile(Tile("板", "白")) should be(false)
    }

    "A starting tile deck" should "have the correct number of cards" in {
        val tiles = Tile.startingTiles
        val groups = tiles.groupBy(_.category)

        tiles.length should be(136)

        val suites = Seq(Tile.CATEGORY_TIAO, Tile.CATEGORY_WAN, Tile.CATEGORY_TONG)
        for (suite <- suites) {
            groups(suite).length should be(36)
        }

        groups(Tile.CATEGORY_FENG).length should be(16)

        val categories = Seq("中", "发", "板")
        for (category <- categories) {
            groups(category).length should be(4)
        }
    }
}
