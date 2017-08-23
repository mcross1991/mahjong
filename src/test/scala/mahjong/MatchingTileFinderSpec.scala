package mahjong
package test

import org.scalatest.{FlatSpec, Matchers}

class MatchingTileFinderSpec extends BaseSpec with MatchingTileFinder {

    "A MatchingTileFinder" should "extract matching tiles with a minimum length" in {
        val tiles = Seq(
            Tile(Tile.CATEGORY_TIAO, "一"),
            Tile(Tile.CATEGORY_TIAO, "一"),
            Tile(Tile.CATEGORY_TIAO, "一")
        )

        val result = extractMatchingSet(tiles, 3)
        result should be(tiles)
    }

    it should "return an empty sequence if the number of matching tiles is less than the specified length" in {
        val tiles = Seq(
            Tile(Tile.CATEGORY_TIAO, "一"),
            Tile(Tile.CATEGORY_TIAO, "一")
        )

        val result = extractMatchingSet(tiles, 3)
        result.isEmpty should be(true)
    }

    it should "return an empty sequence if the tiles are not a matching set" in {
        val tiles = Seq(
            Tile(Tile.CATEGORY_TIAO, "一"),
            Tile(Tile.CATEGORY_TIAO, "一"),
            Tile(Tile.CATEGORY_TIAO, "二")
        )

        val result = extractMatchingSet(tiles, 3)
        result.isEmpty should be(true)
    }

    it should "extract linear tiles with a minmum length" in {
        val tiles = Seq(
            Tile(Tile.CATEGORY_TIAO, "一"),
            Tile(Tile.CATEGORY_TIAO, "二"),
            Tile(Tile.CATEGORY_TIAO, "三")
        )

        val result = extractLinearSet(tiles, 3)
        result should be(tiles)

    }

    it should "should return an empty sequence if the number of linear tiles is less than the specified length" in {
        val tiles = Seq(
            Tile(Tile.CATEGORY_TIAO, "一"),
            Tile(Tile.CATEGORY_TIAO, "二"),
            Tile(Tile.CATEGORY_TIAO, "三")
        )

        val result = extractLinearSet(tiles, 4)
        result.isEmpty should be(true)
    }

    it should "return an empty sequence if the tiles are not a linear set" in {
        val tiles = Seq(
            Tile(Tile.CATEGORY_TIAO, "一"),
            Tile(Tile.CATEGORY_TIAO, "一"),
            Tile(Tile.CATEGORY_TIAO, "二")
        )

        val result = extractLinearSet(tiles, 3)
        result.isEmpty should be(true)
    }

    it should "only extract linear sets from suited tiles" in {
        val tiles = Seq(
            Tile(Tile.CATEGORY_FENG, "北"),
            Tile(Tile.CATEGORY_FENG, "东"),
            Tile(Tile.CATEGORY_FENG, "南"),
            Tile(Tile.CATEGORY_FENG, "西")
        )

        val result = extractLinearSet(tiles, 4)
        result.isEmpty should be(true)
    }

    it should "determine if a set of tiles is a matching set of a specified length" in {
        val tiles = Seq(
            Tile(Tile.CATEGORY_TIAO, "一"),
            Tile(Tile.CATEGORY_TIAO, "一"),
            Tile(Tile.CATEGORY_TIAO, "一")
        )

        isMatchingSet(tiles, 2) should be(false)
        isMatchingSet(tiles, 3) should be(true)
        isMatchingSet(tiles, 4) should be(false)

        val invalidTiles = Seq(
            Tile(Tile.CATEGORY_TIAO, "一"),
            Tile(Tile.CATEGORY_TIAO, "二"),
            Tile(Tile.CATEGORY_TIAO, "三")
        )

        isMatchingSet(invalidTiles, 3) should be(false)
    }


    it should "determine if a set of tiles is a linear set of a specified length" in {
        val tiles = Seq(
            Tile(Tile.CATEGORY_TIAO, "一"),
            Tile(Tile.CATEGORY_TIAO, "二"),
            Tile(Tile.CATEGORY_TIAO, "三")
        )

        isLinearSet(tiles, 2) should be(false)
        isLinearSet(tiles, 3) should be(true)
        isLinearSet(tiles, 4) should be(false)

        val invalidTiles = Seq(
            Tile(Tile.CATEGORY_TIAO, "一"),
            Tile(Tile.CATEGORY_TIAO, "一"),
            Tile(Tile.CATEGORY_TIAO, "一")
        )

        isLinearSet(invalidTiles, 3) should be(false)
    }
}
