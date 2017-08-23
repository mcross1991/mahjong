package mahjong
package test

class TestController extends Controller {

    def requestNextCommand(player: Player): Command = SkipCommand()

    def requestCallCommand(player: Player, groupedTiles: Seq[Seq[Tile]]): Command = SkipCommand()

    def render(player: Player, message: String) {}

    def renderLine(player: Player, message: String) {}
}
