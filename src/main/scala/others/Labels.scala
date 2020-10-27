package outwatch_components

import outwatch._
import outwatch.dsl.{ span => sp, _}
//import outwatch.helpers.VDomModifier

import monix.execution.Scheduler.Implicits.global
import colibri.ext.monix._
import outwatch.reactive.handlers.monix._ // or if you want to use our own reactive library instead of monix: import outwatch.reactive.handler._

object Labels {

  /*val hdlMessageProcessing = Handler.create[Option[String]](None)

  def messageWorking = for {
    h <- hdlMessageProcessing
  } yield {
      h.map( msg =>
        sp(msg, cls:="processingSideEffects")
      )
  }*/

  val hdlMessageProcessing = Handler.create[Option[String]](None).unsafeRunSync()

  def messageWorking = hdlMessageProcessing.map( msg =>
      msg.map ( m =>
          sp(cls:="processingSideEffects")
            //sp ( m, cls := "processingSideEffects")
      )
  )

}
