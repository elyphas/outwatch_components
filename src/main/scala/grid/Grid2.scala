package outwatch_components

import cats.effect.{SyncIO, IO, Sync}
import cats.implicits._

import org.scalajs
import org.scalajs.dom
import org.scalajs.dom.html
import org.scalajs.dom.window.alert

import monix.execution.Scheduler.Implicits.global
import monix.reactive.subjects.PublishSubject
import monix.reactive.Observable                    //or if you want to use our own reactive library instead of monix: import outwatch.reactive.handler._
import outwatch.reactive.handlers.monix._           //or if you want to use our own reactive library instead of monix: import outwatch.reactive.handler._

import colibri.Observer
import colibri.ext.monix._
import colibri.ext.monix.ops._

import outwatch.dsl._
import outwatch._
import outwatch.util.Store

import scala.util.Try
import scala.concurrent.duration._
import org.scalajs.dom.raw.KeyboardEvent
import org.scalajs.dom.ext.KeyCode

import scala.language.postfixOps
import scala.util.Failure
import scala.util.Success

import shapeless._

class Grid2(  colFmt: Seq[ColG],
                  eventsGrid: VDomModifier,
                  eventsCell: VDomModifier,
                  initValue: Map[String, String],
                  //defaultValues: Option[Map[String, String]] = None,
              ) extends FormatNumber with StoreLstItems {

    val initState = State( items = Map.empty[Int, Map[String, Any]],
                            defaultValues = None,
                            rowActive = 0, 
                            itemNew = ItemNew(row = 0) )
  
  private val store = Store.create[IO](Clean, initState, reduce).unsafeRunSync()  //Store.create[IO](Clean, State(), reduce).unsafeRunSync()

  val hdlFieldActive = Handler.create[String]("").unsafeRunSync()
  val hdlChangingValue = PublishSubject[(String, String)]()
  val hdlPositionCursor = Handler.create[(Int, Int)]((0, 0)).unsafeRunSync()

  val hdlActionsGrid = PublishSubject[Int]()

  val mountCellFocus = Observer.create[(dom.Element, String, String)] { case (elem, inputType, idTxt) =>
      if (idTxt == elem.id) {
          val txt = if ( inputType == "input" ) elem.asInstanceOf[html.Input] else elem.asInstanceOf[html.TextArea]
          val range = dom.document.createRange()
          val sel = scalajs.dom.window.getSelection()
          sel.removeAllRanges()
          range.selectNodeContents(txt)
          sel.addRange(range)
          txt.focus
      }
  }

  def contentTd(idTd: String, valor: String, col: Int, row: Int) = {
        val field = colFmt(col)
        val idTxt = idTd + "txtEditable"
        val tabIdx = ((row + 1) * 10) + col

        val eventOnChanging = if (field.inputType == "checkbox") onChangingValueChk else onChangingValueTxt

        val valorVDom = Try {
                            if (field.inputType != "checkbox") VDomModifier ( value := valor )
                            else VDomModifier(checked := (valor.nonEmpty && valor.toBoolean) )
                        } match {
                            case Success(value) => 
                                  VDomModifier(value)
                            case Failure(exception) =>  
                                alert( "Hubo un error al convertir el valor del componente: " + exception.getMessage() )
                                VDomModifier.empty
                        }
        
        val domMount = SyncIO( onDomMount.transformLifted( (e: Observable[(dom.Element)]) =>
                                      //e.combineLatest ( hdlTransition ) )
                                      e.combineLatest(store.liftSource[monix.reactive.Observable]))
                                          //.map { case (elem, (rowActiv, colum) ) =>
                                            .map { case (elem, (action@_, state) ) =>
                                              val rowActiv = state.rowActive
                                              val colum = state.transition._2
                                              ( elem, field.inputType, "r" + rowActiv + "c" + colum + "txtEditable" )
                                        }  --> mountCellFocus )

        val vdomM = VDomModifier( key := idTxt, 
                                  id := idTxt, 
                                  cls := "editCell", 
                                  tabIndex := tabIdx, 
                                  field.styleCell.getOrElse(VDomModifier(display.none)) ,
                                  valorVDom,
                                  eventsCell, 
                                  eventOnChanging, 
                                  domMount, focusUpatePositionCursor, focusCelUpdateField, focusCelUpdateRow 
                              )
        
        if (field.edit) {
            if (field.inputType == "TextArea") textArea(vdomM)
            else if (field.inputType == "input") input(vdomM)
            else input(`type` := "checkbox", vdomM)
        } else {
            val texto = if (field.typeNumber == "money") fmtMiles(valor.toDouble)
            else if (field.typeNumber == "amount") fmtMiles(valor.toDouble, 0)
            else valor
            label(texto, height := "100px", field.styleCell.getOrElse(VDomModifier(display.none)))
        }
    }

  def newCell(txt: String, col: Int, row: Int) = {
      val idTd = "r" + row + "c" + col
      val eventsTd = VDomModifier.empty //val eventsTd = VDomModifier( hiddeGridCatalogsLostFocus) //Tamos a 16 de Abril del 2020, quitar porque se pasa el control a la applicacion.    
      
      td( id := idTd, key := idTd, cls := "tdCell",
          contentTd(idTd, txt, col, row), eventsTd, colFmt(col).styleCell.getOrElse(VDomModifier(display.none) )
      )
  }

  val onChangingValueTxt = SyncIO(onInput.value.transformLifted( (e: Observable[(String)]) =>
              e.distinctUntilChanged.withLatestFrom(hdlFieldActive){ case (txt: String, field: String) => 
                      (field, txt.toUpperCase)}) --> hdlChangingValue )

  val onChangingValueChk = SyncIO(onInput.checked.transformLifted( (e: Observable[Boolean]) =>
              e.distinctUntilChanged.withLatestFrom(hdlFieldActive){ case (chk: Boolean, field: String) => (field, chk.toString)}) --> hdlChangingValue)

  private val focusCel = onFocus.map { f =>
      f.stopPropagation()
      f.preventDefault()
      f.currentTarget.asInstanceOf[html.Input]
  }

  private val focusCelUpdateField = SyncIO( focusCel.map { case txt =>
                                            val cel = txt.parentElement.asInstanceOf[html.TableCell]
                                            colFmt.zipWithIndex.filter{ case (_, idx) => idx == cel.cellIndex }.map { case (col, _) => col.field }.head
                                        } --> hdlFieldActive.contramap[String]{ field => field})

  private val focusUpatePositionCursor = SyncIO(  focusCel.map { case txt =>
                                                  val rect = txt.getBoundingClientRect()
                                                  (rect.bottom.toInt, rect.left.toInt) 
                                              } --> hdlPositionCursor.contramap[(Int,Int)]{ case (bottom, left) => (bottom, left)})

  private val focusCelUpdateRow = SyncIO (  focusCel.map { case txt =>
                                            val row = txt.parentElement.parentElement.asInstanceOf[html.TableRow]
                                            row.rowIndex - 1
                                      }  --> store.contramap[Int](r => UpdateRowActive(r)))

  //hdlTransition.map(x => println(s"Se va ha actualizar la transition:  $x "))
  //hdlTransition.foreach(x => println(s"Se va ha actualizar la transition:  $x "))

    /*def makeTransition(row: Int, col: Int) = {
        println("Deberia actualizar la transition")
        hdlTransition.onNext ( ( row, col ) )
    }*/


  def connectEventsGrid = {
    //val updateTransition = hdlTransition.contramap[(Int,Int)]{ case (r, c) => (r,c)}
    val updateTransition = store.contramap[(Int, Int)]{ case (r, c) => UpdateTransitionColumn(c) }

    //def changeV(mlSeconds: FiniteDuration) = hdlChangingValue.debounce(mlSeconds)
    div (
            /*emitter(changeV(800 millis).withLatestFrom(store.liftSource[monix.reactive.Observable]){ case ((key, value), (action@_, state)) =>
                      (state.rowActive, key, value)
            } ) --> store.contramap[(Int, String, String)]{case (r, k, v) => UpdateItem(Some(r), k, v)},*/

        emitter(hdlChangingValue.withLatestFrom(store.liftSource[monix.reactive.Observable]){ case ((key, value), (action@_, state)) =>
              (state.rowActive, key, value)
        } ) --> store.contramap[(Int, String, String)]{case (r, k, v) => UpdateItem( Some(r), k, v, None) },

        emitter(hdlActionsGrid.filter(_ === KeyCode.Insert).withLatestFrom(store.liftSource[monix.reactive.Observable]) { case (event, (action@_, state)) => 
                      (state.rowActive + 1, 3)  //val rowActiv = state.rowActive + 1    //hdlTransition.onNext(rowActiv, 3)
            }) --> Observer.combineVaried(store.contramap[(Int,Int)]{ case (r, c) => InsertItem(r)}, updateTransition),

        emitter(hdlActionsGrid.filter( _ === KeyCode.Delete)
              .withLatestFrom(store.liftSource[monix.reactive.Observable]){ case (event, (action@_, state)) => 
                      (state.rowActive -1, 3)   //val rowActiv = state.rowActive      //hdlTransition.onNext(rowActiv - 1, 3)
            }) --> Observer.combineVaried( store.contramap[(Int, Int)]{ case (r, c) => DeleteItem(r)}, updateTransition),
    )
  }

  //It is going to update the active row, with (field and value) Map[String, String]
  def updateRowActive(values: Map[String, String], row: Option[Int] = None, colTransition: Option[Int]) =
                values.foreach { case (k, v) =>
                      store.onNext( UpdateItem(row, k, v, colTransition) )
                }

  val actionOnEnter = hdlActionsGrid
                            .filter ( _ == KeyCode.Enter )
                            .withLatestFrom(getActiveRow.liftSource[monix.reactive.Observable]){ case (action@_, record) => record }

  def getActiveRow = store.map { case (action@_, state) => state.items.get(state.rowActive) }

  def getRow(row: Int) = store.map{ case (action@_, state) => state.items.get(row)}

  def getActiveRowNumber = store.map { case (action@_, state) => state.rowActive}

  def loadValues[A: TransformData](items: List[A]) = {
        val ls = items.zipWithIndex.map{ case (item, i) => i -> TransformData(item) }.toMap  //This transform List[case class] to Map[String, String]
        store.onNext(UpdateLstItems(ls))
  }

  def loadValues(items: Map[Int, Map[String, String]]) = store.onNext(UpdateLstItems(items))

  def setDefaultValues(defaultValues: Map[String, String]) = store.onNext(SetDefaultValues(defaultValues))

  val actionsGrid = SyncIO ( onKeyUp.filter {  _.keyCode match {
                                                        case KeyCode.Delete => true
                                                        case KeyCode.Insert => true
                                                        case KeyCode.Escape => true
                                                        case KeyCode.Enter => true
                                                        case _ => false
                                                  } }.map{_.keyCode} --> hdlActionsGrid)

  def setFocus = {
      val gridElement = dom.document.getElementById("tblGrid").asInstanceOf[html.TableCell]
      gridElement.focus
  }

  def render = div(
        table( id := "tblGrid", cls := "tblGrid",
          thead(tr( colFmt.map(t => td(t.title, t.styleTitle.getOrElse(VDomModifier(display.none)))))),
          tbody(
            eventsGrid,
            actionsGrid,
            store.map { case (action@_, state) =>
                state.items.toSeq.sortBy(_._1).map { case (idRow: Int, fields: Map[String, Any]) =>
                    tr( id := "row" + idRow.toString,
                        colFmt.zipWithIndex.map { case (col: ColG, ii: Int) => 
                            newCell(fields.getOrElse(col.field, "").toString , col = ii, row = idRow)
                        }
                    )
                }.toList

            },
            connectEventsGrid,
          ),
        ),
      )
}

/*val clickableView: EmitterBuilder[Boolean, VDomModifier] = EmitterBuilder.ofModifier { sink: Observer[Boolean] =>
      VDomModifier( display.flex,  minWidth := "0px",
        Monoid[EmitterBuilder[Boolean, VDomModifier]].combine(onMouseDown(true), onMouseUp(false)) --> sink
      )
    }
    for {
      handler <- Handler.create[String]
      node = div(
        id := "strings",
        clickableView.map {
          case true => "yes"
          case false => "no"
        } --> handler,
        handler
      )
    } yield succeed
}*/