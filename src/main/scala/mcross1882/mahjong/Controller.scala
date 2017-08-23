package mcross1882.mahjong

import scala.io.StdIn
import java.io.{InputStreamReader, BufferedReader, PrintWriter}
import java.net.{ServerSocket, Socket}

trait Controller {

    def requestNextCommand(player: Player): Command

    def requestCallCommand(player: Player, groupedTiles: Seq[Seq[Tile]]): Command

    def render(player: Player, message: String)

    def renderLine(player: Player, message: String)
}


class ConsoleController(factory: CommandFactory) extends Controller {

    def requestNextCommand(player: Player): Command = {
        factory.create(player, StdIn.readLine(s"${player.name}> ")) match {
            case Some(p) => p
            case None => requestNextCommand(player)
        }
    }

    def requestCallCommand(player: Player, groupedTiles: Seq[Seq[Tile]]): Command = {
        factory.create(player, StdIn.readLine(s"${player.name}> ")) match {
            case Some(p: DiscardTile) => requestCallCommand(player, groupedTiles)
            case Some(p) => p
            case None => requestCallCommand(player, groupedTiles)
        }
    }

    def render(player: Player, message: String) {
        print(message)
    }

    def renderLine(player: Player, message: String) {
        println(message)
    }
}

class SocketController(factory: CommandFactory, port: Int) extends Controller {

    private val server = new ServerSocket(port)

    private val socket: Socket = server.accept

    private val reader: BufferedReader = new BufferedReader(new InputStreamReader(socket.getInputStream))

    private val writer: PrintWriter = new PrintWriter(socket.getOutputStream)

    def requestNextCommand(player: Player): Command = {
        println(s"Waiting for ${player.name}")
        writePrompt(player)
        factory.create(player, reader.readLine) match {
            case Some(command) => command
            case None => requestNextCommand(player)
        }
    }

    def requestCallCommand(player: Player, groupedTiles: Seq[Seq[Tile]]): Command = {
        println(s"Waiting for ${player.name}")
        writePrompt(player)
        factory.create(player, reader.readLine) match {
            case Some(command: DiscardTile) => requestCallCommand(player, groupedTiles)
            case Some(command) => command
            case None => requestCallCommand(player, groupedTiles)
        }
    }

    def render(player: Player, message: String) {
        writer.print(message)
        writer.flush
    }

    def renderLine(player: Player, message: String) {
        writer.println(message)
        writer.flush
    }

    private def writePrompt(player: Player) {
        writer.print(s"${player.name}> ")
        writer.flush
    }
}

class BotController(factory: CommandFactory) extends Controller with MatchingTileFinder {

    type TileGroups = Seq[Seq[Tile]]

    def requestNextCommand(player: Player): Command = {
        val groups = player.groupedTiles
        findKongs(groups) match {
            case Some(list) => return CallKong(player, list)
            case None => // noop
        }

        findPungs(groups) match {
            case Some(list) => return CallPung(player, list)
            case None => // noop
        }

        findChows(groups) match {
            case Some(list) => return CallChow(player, list)
            case None => // noop
        }

        DiscardTile(player, findSmallestTile(player, groups))
    }

    def requestCallCommand(player: Player, groups: TileGroups): Command = {
        findKongs(groups) match {
            case Some(list) => return CallKong(player, list)
            case None => // noop
        }

        findPungs(groups) match {
            case Some(list) => return CallPung(player, list)
            case None => // noop
        }

        findChows(groups) match {
            case Some(list) => return CallChow(player, list)
            case None => // noop
        }

        SkipCommand()
    }

    def render(player: Player, message: String) {
        // noop
    }

    def renderLine(player: Player, message: String) {
        // noop
    }
    
    private def findKongs(groups: TileGroups): Option[Seq[Tile]] = {
        groups.map(x => extractMatchingSet(x, 4)).filter(!_.isEmpty).headOption
    }

    private def findPungs(groups: TileGroups): Option[Seq[Tile]] = {
        groups.map(x => extractMatchingSet(x, 3)).filter(!_.isEmpty).headOption
    }

    private def findChows(groups: TileGroups): Option[Seq[Tile]] = {
        groups.map(x => extractLinearSet(x, 3)).filter(!_.isEmpty).headOption
    }

    private def findSmallestTile(player: Player, groups: TileGroups): Int = {
        var smallestTile: Tile = groups.head.head
        var smallestValue: Int = 4        
        for (index <- 0 until groups.length) {
            if (groups(index).length < smallestValue) {
                smallestTile = groups(index).head
                smallestValue = groups(index).length
            }
        }

        player.tileIndex(smallestTile)
    }
}

