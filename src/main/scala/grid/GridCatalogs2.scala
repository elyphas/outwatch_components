package outwatch_components

import monix.execution.Scheduler.Implicits.global

import colibri.Observer
import outwatch.reactive.handler._
import outwatch.dsl._
import outwatch._
import scala.collection.immutable.ListMap

class GridCatalogs2 {

  val hdlPositionCursor = Handler.create[(Int, Int)]((0, 0)).unsafeRunSync()
  
  def render[A: TransformData]( items: List[A],
                                pos: (Int, Int),
                                onClickItem: Observer[Map[String,String]] ) = {

        val ls = items.zipWithIndex.map{ case (item, i) => i -> TransformData(item) }.toMap

        val titles =  ls.get(1).getOrElse(Map.empty[String, String]).map { case (k, v) =>
                            td ( k, textAlign := "Center" )
                      }

        table ( key := "gridCatalog", cls := "gridCatalog", cls := "scroll",
                top := pos._1.toString + "px", left := pos._2.toString + "px",
        thead(
              tr(
                  titles.toList
                /*td("Id", textAlign := "Center"),
                td("Descripción", textAlign := "Center")*/
              )
            ),
        tbody (
          ls.map { case ( indx, item ) =>
            val tds = item map { case (k, v) =>
                                                td ( v, textAlign := "Justify", width :="40px" )
                                              }
                                              tr( onClick.use( item ) --> onClickItem, tds.toList )
              }.toList
        )
      )
    }
}