/**
  * Copyright (C) 2017-2018 the original author or authors.
  * See the LICENSE file distributed with this work for additional
  * information regarding copyright ownership.
  *
  * @author Matthew Cross <github.com/mcross1991>
  */
package mahjong
package test

class TestController extends Controller {

    def requestNextCommand(player: Player): Command = SkipCommand()

    def requestCallCommand(player: Player, groupedTiles: Seq[Seq[Tile]]): Command = SkipCommand()

    def render(player: Player, message: String) {}

    def renderLine(player: Player, message: String) {}
}
