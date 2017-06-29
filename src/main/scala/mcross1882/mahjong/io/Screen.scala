package mcross1882.mahjong

trait Screen {

    def renderPlayerTiles(player: Player)
}

class ConsoleScreen {

    def renderPlayerTiles(player: Player) {
        val tiles = player.tiles
        val numberOfTiles = tiles.length

        for (index <- 0 until numberOfTiles) {
            print(" +--+ ")
        }
        println

        for (index <- 0 until numberOfTiles) {
            print(s" |${tiles(index).value}| ")
        }
        println

        for (index <- 0 until numberOfTiles) {
            print(" |--| ")
        }
        println

        for (index <- 0 until numberOfTiles) {
            print(s" |${tiles(index).category}| ")
        }
        println

        for (index <- 0 until numberOfTiles) {
            print(" +--+ ")
        }
        println

        for (index <- 0 until numberOfTiles) {
            print(f"  ${index+1}%2d  ")
        }
        println
    }
}

