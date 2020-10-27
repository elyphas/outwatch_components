package outwatch_components

import monix.execution.Scheduler.Implicits.global

import colibri.Observer
import outwatch.reactive.handler._
import outwatch.dsl._
import outwatch._

class GridCatalogs {

  val hdlPositionCursor = Handler.create[(Int, Int)]((0, 0)).unsafeRunSync()
  
  def render(items: List[(String, String)], pos: (Int, Int), onClickItem: Observer[(String, String)] ) = {
        table ( key := "gridCatalog", cls := "gridCatalog", cls := "scroll",
                top := pos._1.toString + "px", left := pos._2.toString + "px",
        thead(tr(td("Id", textAlign := "Center"), td("Descripción", textAlign := "Center"))),
        tbody (
          items.map { case ( k, v ) =>
                tr (
                  onClick.use( ( k, v ) ) --> onClickItem,
                  td ( k, textAlign := "Justify", width :="40px" ),
                  td ( v, textAlign := "Justify", width :="450px" )
                )
          }
        )
      )
    }
}