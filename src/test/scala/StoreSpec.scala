package client_testear

import cats.effect.{IO, SyncIO}
import client_testear.Deprecated.IgnoreWarnings.initEvent
import org.scalajs.dom.{CustomEvent, Element, Event, document, html}
import org.scalatest.Tag

import scala.concurrent.Await
import scala.concurrent.duration._

import monix.execution.Cancelable
import monix.reactive.{Observable}

import outwatch.dsl.{ span => sp, _}
import outwatch._

import colibri.ext.monix._
import colibri.ext.monix.ops._
import colibri._

import outwatch.reactive.handlers.monix._ // or if you want to use our own reactive library instead of monix: import outwatch.reactive.handler._

object StoreTest1 extends Tag("StoreTest1")

class StoreLstItemsSpec extends JSDomSpec  {

  def sendEvent(elem: Element, eventType: String) = {
      val event = document.createEvent("Events")
      initEvent(event)(eventType, canBubbleArg = true, cancelableArg = false)
      elem.dispatchEvent(event)
  }

  val click = IO {
    val event = document.createEvent("Events")
    initEvent(event)("click", canBubbleArg = false, cancelableArg = true)
    event
  }

  "testing store" should "Testing Store" taggedAs StoreTest1 in {
    import outwatch_components.StoreLstItems

    val impStore = new StoreLstItems {}
    import impStore._
    
    println("Vamos a testear el Store en el outwatch_components")

    store.foreach { case (action, state) =>
      println ( "Store Rreacting ********************" )

      state.items.foreach { case (k, v) =>
        println(s"Row: $k  values: $v")
      }

    }

    val itemsInit = Map(
                      1 -> Map( "id" -> "changos1", "description" -> "gorilas1"),
                      2 -> Map( "id" -> "changos2", "description" -> "gorilass2")
                    )

    store.onNext(UpdateLstItems(itemsInit))

    store.onNext(UpdateItem(1, "id", "changos3"))
 
    /*for {
        _ <- OutWatch.renderInto[IO]( "#root", div( frm.render ))
        _ <- IO{ 
            store.onNext( UpdateIdx ( idx ))
        }
    } yield {
        println("Termino")
        succeed
    }*/
  }
}