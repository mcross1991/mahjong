package mahjong
package test

import org.scalatest.{FlatSpec, Matchers}

class CommandSpec extends BaseSpec {

    "SkipCommand" should "stop the game from waiting for the current player" in {
        val game = fakeGame
        game.isWaitingForPlayer should be(true)
        SkipCommand().execute(game)
        game.isWaitingForPlayer should be(false)
    }

    "ExitGame" should "finish the game" in {
        val game = fakeGame
        game.isFinished should be (false)
        ExitGame().execute(game)
        game.isFinished should be(true)
    }

    "SetNextPlayer" should "set the current player of the game" in {
        val players = Seq(fakePlayer("demo_a"), fakePlayer("demo_b"))
        val game = new Game(players)
        game.getCurrentPlayer should not be(players(1))
        SetNextPlayer(players(1)).execute(game)
        game.getCurrentPlayer should be(players(1))
    }

    "AskPlayer" should "ask the current player for the next command" in {
        val player = fakePlayer("demo_a")
        val game = new Game(Seq(player))
        player.takeTile(Tile(Tile.CATEGORY_TIAO, "二"))
        AskPlayer(player).execute(game)
        game.isFinished should be(false)
    }

    it should "let the player call mahjong after the last command" in {
        val player = fakePlayer("demo_a")
        val game = new Game(Seq(player))
        player.takeTile(Tile(Tile.CATEGORY_TIAO, "二"))
        player.takeTile(Tile(Tile.CATEGORY_TIAO, "二"))
        AskPlayer(player).execute(game)
        game.isFinished should be(true)
    }

    "CheckDiscardedTile" should "ask the player if they want to use the last discarded tile to form a hand" in {
        val player = fakePlayer("demo_a")
        val game = new Game(Seq(player))
        player.takeTile(Tile(Tile.CATEGORY_TIAO, "二"))
        game.discardTile(Tile(Tile.CATEGORY_TIAO, "二"))
        CheckDiscardedTile(player).execute(game)
        game.isFinished should be(false)
    }

    it should "allow a player to call mahjong after they check the tiles" in {
        val player = fakePlayer("demo_a")
        val game = new Game(Seq(player))
        player.takeTile(Tile(Tile.CATEGORY_TIAO, "二"))
        player.takeTile(Tile(Tile.CATEGORY_TIAO, "二"))
        game.discardTile(Tile(Tile.CATEGORY_TIAO, "二"))
        CheckDiscardedTile(player).execute(game)
        game.isFinished should be(true)
    }

    it should "skip asking players if the game does not have a discarded tile" in {
        val player = fakePlayer("demo_a")
        val game = new Game(Seq(player))
        CheckDiscardedTile(player).execute(game)
        game.isFinished should be(false)
    }

    "DealStartingTiles" should "deal the first 13 tiles to each player in the game" in {
        val players = Seq(fakePlayer("demo_a"), fakePlayer("demo_b"))
        val game = new Game(players)
        DealStartingTiles().execute(game)
        players(0).tiles.length should be(13)
        players(1).tiles.length should be(13)
    }

    "DealTile" should "deal a tile to a player" in {
        val player = fakePlayer("demo_a")
        val game = new Game(Seq(player))
        DealTile(player).execute(game)
        player.tiles.length should be(1)
    }

    it should "stop the game if there are no more tiles left" in {
        val player = fakePlayer("demo_a")
        val game = new Game(Seq(player))

        assertThrows[NoMoreTilesException] {
            for (index <- 0 until 137) DealTile(player).execute(game)
        }
    }

    "DiscardTile" should "discard a players tile into the games discared pile" in {
        val player = fakePlayer("demo_a")
        val game = new Game(Seq(player))
        val tile =Tile(Tile.CATEGORY_TIAO, "二")
        player.takeTile(tile)
        DiscardTile(player, 0).execute(game)
        player.tiles.length should be(0)
        game.lastDiscardedTile should be(Some(tile))
    }

    it should "throw an exception if the tile does not exist in the players set" in {
        val player = fakePlayer("demo_a")
        val game = new Game(Seq(player))

        assertThrows[MissingTileException] {
            DiscardTile(player, 20).execute(game)
        }

        player.tiles.length should be(0)
        game.lastDiscardedTile should be(None)
    }

    "CallPung" should "let the player call pung if they have a matching set of 3" in {
        val player = fakePlayer("demo_a")
        val game = new Game(Seq(player))
        val tiles = Seq(
           Tile(Tile.CATEGORY_TIAO, "二"),
           Tile(Tile.CATEGORY_TIAO, "二"),
           Tile(Tile.CATEGORY_TIAO, "二")
        )

        tiles.foreach(t => player.takeTile(t))

        CallPung(player, tiles).execute(game)

        player.tiles.length should be(1)
        player.score.pungs.length should be(1)
        game.lastDiscardedTile should be(None)
    }

    it should "not add any score if the tile set was not a pung" in {
        val player = fakePlayer("demo_a")
        val game = new Game(Seq(player))
        val tiles = Seq(
           Tile(Tile.CATEGORY_TIAO, "二"),
           Tile(Tile.CATEGORY_TIAO, "二"),
           Tile(Tile.CATEGORY_TIAO, "三")
        )

        tiles.foreach(t => player.takeTile(t))

        CallPung(player, tiles).execute(game)

        player.tiles.length should be(3)
        player.score.pungs.length should be(0)
        game.lastDiscardedTile should be(None)
    }

    "CallKong" should "let the player call kong if they have a matching set of 4" in {
        val player = fakePlayer("demo_a")
        val game = new Game(Seq(player))
        val tiles = Seq(
           Tile(Tile.CATEGORY_TIAO, "二"),
           Tile(Tile.CATEGORY_TIAO, "二"),
           Tile(Tile.CATEGORY_TIAO, "二"),
           Tile(Tile.CATEGORY_TIAO, "二")
        )

        tiles.foreach(t => player.takeTile(t))

        CallKong(player, tiles).execute(game)

        player.tiles.length should be(1)
        player.score.kongs.length should be(1)
        game.lastDiscardedTile should be(None)
    }

    it should "not add any score if the tile set was not a kong" in {
        val player = fakePlayer("demo_a")
        val game = new Game(Seq(player))
        val tiles = Seq(
           Tile(Tile.CATEGORY_TIAO, "二"),
           Tile(Tile.CATEGORY_TIAO, "二"),
           Tile(Tile.CATEGORY_TIAO, "二"),
           Tile(Tile.CATEGORY_TIAO, "三")
        )

        tiles.foreach(t => player.takeTile(t))

        CallKong(player, tiles).execute(game)

        player.tiles.length should be(4)
        player.score.kongs.length should be(0)
        game.lastDiscardedTile should be(None)
    }

    "CallChow" should "let the player call chow if they have a linear set of 3" in {
        val player = fakePlayer("demo_a")
        val game = new Game(Seq(player))
        val tiles = Seq(
           Tile(Tile.CATEGORY_TIAO, "一"),
           Tile(Tile.CATEGORY_TIAO, "二"),
           Tile(Tile.CATEGORY_TIAO, "三")
        )

        tiles.foreach(t => player.takeTile(t))

        CallChow(player, tiles).execute(game)

        player.tiles.length should be(1)
        player.score.chows.length should be(1)
        game.lastDiscardedTile should be(None)
    }

    it should "not add any score if the tile set was not a chow" in {
        val player = fakePlayer("demo_a")
        val game = new Game(Seq(player))
        val tiles = Seq(
           Tile(Tile.CATEGORY_TIAO, "一"),
           Tile(Tile.CATEGORY_TIAO, "二"),
           Tile(Tile.CATEGORY_TIAO, "二")
        )

        tiles.foreach(t => player.takeTile(t))

        CallChow(player, tiles).execute(game)

        player.tiles.length should be(3)
        player.score.chows.length should be(0)
        game.lastDiscardedTile should be(None)
    }
    
    private def fakeGame(): Game = new Game(Seq(fakePlayer("demo_a"), fakePlayer("demo_b")))

    private def fakePlayer(name: String): Player = new Player(name, Player.createScore, new TestController)
}
