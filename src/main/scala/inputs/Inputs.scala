package outwatch_components

import cats.implicits._
import org.scalajs.dom.html

import monix.execution.Cancelable
import monix.execution.Scheduler.Implicits.global
import monix.reactive.{Observable, Observer}

import colibri.ext.monix._
import colibri.ext.monix.ops._

import outwatch.reactive.handlers.monix._ // or if you want to use our own reactive library instead of monix: import outwatch.reactive.handler._

import outwatch._
import outwatch.dsl.{ span => sp, _}
import scala.util.{Failure, Success, Try}
import org.scalajs.dom.window.alert

object inputs {

  def cmpCombo[S, F](lbl: String, hdl: Handler[String], lst: Map[String, String], events: VDomModifier, itemSelected: String, w: Int) = {

    val optDefault = option(disabled, selected := true, display.none)

    val lstOpt = lst.map { case ( k, v ) =>
        val selectedd = k == itemSelected
        option( k, value := v, selected := selectedd, cls := "option" )
    }.toSeq

    div ( id := "div" + lbl, cls := "div-component", marginLeft := "10px", marginTop := "10px", display.block, 
        float.left,
        label ( lbl, marginLeft := "20px", textAlign:="Left", display.block ),
        select (  id:= "cbo" + lbl, key := "cbo" + lbl, 
            display.block, 
            value <-- hdl.map( r => r ),
                optGroup( cls := "optgroup", 
                    optDefault +: lstOpt
                ),
            onChange.value --> hdl,
            events
        )
    )

  }

  /*def cmpInput2(lbl: String, hdl: Handler[String], w: Int, events: VDomModifier) =
    div(id := "txt" + lbl,
      float.left, width := ( w + 35 ).toString  + "px", label( lbl, width := "60px" ),
      input ( width := w.toString + "px", cls:="selectall",
        value <-- hdl,
        onChange.target.value.map(_.toUpperCase()) --> hdl,
        events,
      )
    )*/

  def cmpCheckBoxOption(lbl: String, hdl: Handler[Option[Boolean]], props: VDomModifier, events: VDomModifier) = {

    cmpCheckBox(lbl, hdl.mapSubject[Boolean]{ 
                                    case false => None
                                    case x => Some(x)
                                }{
                                    case Some(x) => x
                                    case None => false
                                },
                                    props, events)
  }

  def cmpCheckBox(lbl: String, hdl: Handler[Boolean], props: VDomModifier, events: VDomModifier) =
    div( id := "txt" + lbl,
      float.left, props,
      label( lbl, props ),
      input ( key := "chk" + lbl, `type`:="checkbox",
        lbl,
        checked <-- hdl,
        onClick.checked --> hdl,
        props, events
      )
    )

/*def cmpInputDate(lbl: String, hdl: Handler[java.util.Date],
                     props: VDomModifier = VDomModifier.empty,
                     events: Option[VDomModifier] = None,
                     inputType: String = "input") = {

      cmpInput( lbl, 
          hdl.mapSubject[String]{ d =>
              new java.util.Date ( scala.scalajs.js.Date.parse( d ).toLong )
              
          }{ d: java.util.Date =>
              val jsDate = new scala.scalajs.js.Date(d.getTime().toDouble)
              jsDate.getDay + "/" + (jsDate.getMonth+1) + "/" + jsDate.getFullYear
          },
          props, events, inputType
      )
  }*/


  def cmpInputNumberDbl(lbl: String, hdl: Handler[Double],
                     props: VDomModifier = VDomModifier.empty,
                     events: Option[VDomModifier] = None,     //events: VDomModifier = VDomModifier.empty,
                     inputType: String = "input") = {
      cmpInput(lbl,
          hdl.mapSubject[String](_.toDouble){ f: Double =>
            if (f <= 0.0) ""
            else f.toString
          },
          props, events, inputType
      )
  }

  def cmpInputOptionString(lbl: String, hdl: Handler[Option[String]],
                        props: VDomModifier = VDomModifier.empty,
                        events: Option[VDomModifier] = None,      //events: VDomModifier = VDomModifier.empty,
                        inputType: String = "input") = {
        cmpInput(lbl,
          hdl.mapSubject[String]{
            case "" => None
            case x => Some (x)
          }{
            case Some(x) => x
            case None => ""
          },
          props, events, inputType
        )
  }

  def cmpInputOptionDbl(lbl: String, hdl: Handler[Option[Double]],
                        props: VDomModifier = VDomModifier.empty,
                        events: Option[VDomModifier] = None,      //events: VDomModifier = VDomModifier.empty,
                        inputType: String = "input") = {
    cmpInput(lbl,
      hdl.mapSubject[String]{
        case "" => None
        case x => Some (x.toDouble)
      }{
          case Some(x) => x.toString
          case None => ""
      },
      props, events, inputType
    )
  }

  def cmpInputOptionInt(lbl: String, hdl: Handler[Option[Int]],
                     props: VDomModifier = VDomModifier.empty,
                     events: Option[VDomModifier] = None,     //events: VDomModifier = VDomModifier.empty,
                     inputType: String = "input") = {
    cmpInput(lbl,
      hdl.mapSubject[String]{
          case "" => None
          case x => Some (x.toInt)
      }{ 
          case Some(x) => x.toString
          case None => ""
       },
      props, events, inputType
    )
  }

  def cmpInputNumber(lbl: String, hdl: Handler[Int],
               props: VDomModifier = VDomModifier.empty,
               events: Option[VDomModifier] = None,
               inputType: String = "input") = {
        
        cmpInput(lbl,
                hdl.mapSubject[String]{ x =>
                    Try {
                      x.toInt
                    } match {
                      case Success(value) => value
                      case Failure(exception) =>
                        println("Hubo un error line:172 from inputs in outwatch_components " + exception.getMessage())
                        0
                    }
                }{ f: Int => if (f <= 0) "" else f.toString},
            props, events, inputType
        )
  }

  def cmpInput(lbl: String, hdl: Handler[String],
                  props: VDomModifier = VDomModifier.empty,
                  events: Option[VDomModifier] = None,
                  inputType: String = "input",
                  ) = {

      val vDom = VDomModifier ( id:= "txt" + lbl, key := "txt" + lbl, display.block,
                  value <-- hdl,
                  onKeyUp.map { k =>
                      val txt = k.currentTarget.asInstanceOf[html.Input]
                      if ( txt.value == "" ) "" else txt.value
                  } --> hdl,
                  props, events.getOrElse(VDomModifier.empty)
              )

      val txtComponent = if (inputType == "input") input(vDom) else textArea(vDom)

      div( id := "div" + lbl, cls := "div-component", display.block, marginRight := "30px", 
          label(lbl, textAlign:="Left", /*marginLeft := "10px",*/ display.block), 
          props, txtComponent
      )
  }

  def cmpPasswrod(lbl: String, hdl: Handler[String],
               props: VDomModifier = VDomModifier.empty,
               events: Option[VDomModifier] = None
              ) = {

    val vDom = VDomModifier ( id:= "txt" + lbl, key := "txt" + lbl, tpe := "password", display.block,
      value <-- hdl,
      onKeyUp.map { k =>
        val txt = k.currentTarget.asInstanceOf[html.Input]
        if ( txt.value == "" ) "" else txt.value
      } --> hdl,
      props, events.getOrElse(VDomModifier.empty)
    )

    val txtComponent = input(vDom)

    div( id := "div" + lbl, cls := "div-component", display.block, marginRight := "30px",
      label(lbl, textAlign:="Left", /*marginLeft := "10px",*/ display.block),
      props, txtComponent
    )
  }

  

  /*def cmpInputTxt(lbl: String, hdl: Handler[String],
               props: VDomModifier = VDomModifier.empty,
               events: VDomModifier = VDomModifier.empty,
               inputType: String = "input") = {

    val vDom = VDomModifier ( id:= "txt" + lbl, key := "txt" + lbl,
        clear.both,
        value <-- hdl,
        onKeyUp.map { k =>
              val txt = k.currentTarget.asInstanceOf[html.Input]
              if ( txt.value == "" ) "" else txt.value
        } --> hdl,
        props, events
    )

    val txtComponent = if (inputType == "input") input(vDom) else textArea(vDom)

    div( id := "div" + lbl, cls := "div-component",
        label(lbl, float.left, display.block, textAlign := "left"),
        txtComponent,
        props
    )
  }*/

  /*def cmpInputFechas( lbl: String, hdl: Handler[Fechas], w: Int ) =
    div( float.left, width := (w + 35).toString  + "px",
      label( lbl, width := "60px" ),
      input (
        value <-- hdl.map( r => r.fecha ),
        onChange.target.value.map(r=> Fechas(r)) --> hdl,
        width := w.toString + "px"
      )
    )*/

}
