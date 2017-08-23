package mahjong

class TooManyPlayersException extends Exception("Cannot register anymore players, 4 is the max")

class SessionAlreadyStartedException extends Exception("Session is already started")

class Session(players: Seq[Player]) {

    private val MAX_PLAYERS = 4

    private var isStarted: Boolean = false

    if (players.length > MAX_PLAYERS) {
        throw new TooManyPlayersException()
    }

    def start() {
        if (isStarted) {
            throw new SessionAlreadyStartedException()
        }

        isStarted = true

        val game = new Game(players.toSeq)

        DealStartingTiles().execute(game)

        while (!game.isFinished) {
            AskOtherPlayers().execute(game)

            val player = game.getCurrentPlayer

            game.renderLine(s"${player.name} tiles")

            ShowTiles(player, player.tiles, false).execute(game)
            DealTile(player).execute(game)
            AskPlayer(player).execute(game)
        }
    }
}
