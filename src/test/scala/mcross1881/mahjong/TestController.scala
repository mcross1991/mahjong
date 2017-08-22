package mcross1882.mahjong
package test

class TestController extends InputController {

    def requestNextCommand(player: Player): Command = SkipCommand()

    def requestCallCommand(player: Player, groupedTiles: Seq[Seq[Tile]]): Command = SkipCommand()

    def renderOutput(player: Player, message: String) {}
}
