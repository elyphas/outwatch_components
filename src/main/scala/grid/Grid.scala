package outwatch_components

import cats.effect.SyncIO
import cats.implicits._

import org.scalajs
import org.scalajs.dom
import org.scalajs.dom.ext.KeyCode
import org.scalajs.dom.html

import monix.execution.Scheduler.Implicits.global
import monix.reactive.subjects.PublishSubject
import monix.reactive.Observable //or if you want to use our own reactive library instead of monix: import outwatch.reactive.handler._
import outwatch.reactive.handlers.monix._ //or if you want to use our own reactive library instead of monix: import outwatch.reactive.handler._

import colibri.Observer
import colibri.ext.monix._
import colibri.ext.monix.ops._

import outwatch.dsl._
import outwatch._

import scala.util.Try

/*case class ColG (   field: String, 
                    title: Option[String] = None, 
                    styleTitle: Option[VDomModifier] = None, 
                    styleCell: Option[VDomModifier] = None,
                    typeNumber: String = "",
                    inputType: String = "input",
                    edit: Boolean = false )*/

class Grid0( colFmt: Seq[ColG],
            eventsGrid: VDomModifier,
            eventsCell: VDomModifier
          ) extends FormatNumber {

  /*helpers.OutwatchTracing.patch.zipWithIndex.foreach { case (proxy, index) =>
      org.scalajs.dom.console.log(s"Snabbdom patch ($index)!", JSON.parse(JSON.stringify(proxy)), proxy)
  }*/

  val hdlFieldActive = Handler.create[String]("").unsafeRunSync()
  val hdlRowActive = PublishSubject[Int]()
  val hdlChangingValue = PublishSubject[(String, String)]()

  val hdlPositionCursor = Handler.create[(Int, Int)]((0, 0)).unsafeRunSync()
  val hdlTransition = Handler.create[(Int, Int)]((0,7)).unsafeRunSync()       //row, col
  val hdlActionsGrid = PublishSubject[String]() //Handler.create[String]("").unsafeRunSync()

  /*private def transitionOnEnter = SinkObserver.create[(Int, Int)]{ case (row, col) =>
      transitions.get(col).foreach( nextCol => hdlTransition.onNext((row, col)))
  }*/

  protected val keyEnter = onKeyDown.filter(_.keyCode == KeyCode.Enter).map { case k => k.stopPropagation(); k }

  val keyInsert = onKeyUp.filter(_.keyCode == KeyCode.Insert)

  private def getCellFromKeyBoard(k: dom.KeyboardEvent) = k.target.asInstanceOf[html.TableCell]

  private def getRowFromCell(c: html.TableCell) = c.parentNode.asInstanceOf[html.TableRow]

  def getPositionCell ( k: dom.KeyboardEvent ) = {
    val cell = getCellFromKeyBoard(k)
    val row = getRowFromCell(cell)
    (cell.cellIndex, row.rowIndex)
  }

  def getPositionCellEnter = keyEnter.map(getPositionCell)

  //private def insertRow = SinkObserver.create[(Int, Int)]{ case (row, col) => hdlTransition.onNext(Some(row, focusColOnInsert))}
  /*def getPositionCellInsert = keyInsert.map(getPositionCell) --> hdlTransition.contramap[(Int, Int)]{case (r, c) => Some(r, focusColOnInsert)}*/

  //def onInsertRow = keyInsert.map(getPositionCell) --> hdlTransition.contramap[(Int, Int)]{case (r, c) => (r, focusColOnInsert)}

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
        val valorVDom =  if (field.inputType == "checkbox") VDomModifier(checked := valor.toBoolean) else VDomModifier(value:=valor)


        val domMount = SyncIO(onDomMount
            .transformLifted( (e: Observable[(dom.Element)]) =>
              e.combineLatest(hdlTransition.distinctUntilChanged))
                .map { case (elem, (rowAct, colum) ) =>
                  (elem, field.inputType, "r" + (rowAct-1) + "c" + colum + "txtEditable")
                } --> mountCellFocus
        )

        /*val domMount = SyncIO(
                    onDomMount.transform( _.lift[Observable].combineLatest(hdlTransition.distinctUntilChanged))
                        .map { case (elem, (rowAct, colum) ) =>
                          (elem, field.inputType, "r" + (rowAct-1) + "c" + colum + "txtEditable")
                        } --> mountCellFocus
                      )*/

        val vdomM = VDomModifier( key := idTxt, id := idTxt, cls:="editCell", tabIndex := tabIdx, 
                                  field.styleCell.getOrElse(VDomModifier(display.none)) ,
                                  //value := txt,
                                  valorVDom,
                                  eventsCell,
                                  focusUpatePositionCursor,
                                  focusCelUpdateField, focusCelUpdateRow, eventOnChanging,
                                  domMount,
                  )

        //Ya tenemos components para input y textArea.
        if (field.edit) {
          if (field.inputType == "TextArea") textArea(vdomM)
          else if (field.inputType == "input") input(vdomM)
          else input(`type` := "checkbox", vdomM)
        } else {
            val texto = if (field.typeNumber == "money") fmtMiles(valor.toDouble)
            else if (field.typeNumber == "amount") fmtMiles(valor.toDouble, 0)
            else valor
            label(texto, field.styleCell.getOrElse(VDomModifier(display.none)))
          }

    }

  def newCell(txt: String, col: Int, row: Int) = {
      val idTd = "r" + row + "c" + col
      val eventsTd = VDomModifier( hiddeGridCatalogsLostFocus)
      td( id := idTd, key := idTd, cls := "tdCell", contentTd(idTd, txt, col, row), eventsTd, colFmt(col).styleCell.getOrElse(VDomModifier(display.none)) )
  }


  def showDetails( l: Map[Int, Map[String, String]]) = 
      l
        .toSeq
        .sortBy(_._1) map{ case (idRow: Int, fields: Map[String, String]) => 
            tr(id := "row" + idRow.toString,
                colFmt
                  .zipWithIndex
                  .map { case (col: ColG, ii: Int) => newCell(fields.getOrElse(col.field, ""), col = ii, row = idRow ) }
            )
          }
  
  val onChangingValueTxt = SyncIO ( onInput.value.transformLifted( (e: Observable[(String)]) =>
      e.distinctUntilChanged
      .withLatestFrom(hdlFieldActive){ case (txt: String, field: String) =>
        (field, txt.toUpperCase)}) --> hdlChangingValue)
  
  val onChangingValueChk = SyncIO (
      onInput.checked.transformLifted( (e: Observable[Boolean]) =>
          e.distinctUntilChanged
            .withLatestFrom(hdlFieldActive){ case (chk: Boolean, field: String) =>
              (field, chk.toString)
            }
      ) --> hdlChangingValue )

  private val focusCel = onFocus.map{ f =>
      f.stopPropagation()
      f.preventDefault()
      f.currentTarget.asInstanceOf[html.Input]
  }

  private val focusUpatePositionCursor = SyncIO(focusCel.map { case txt =>
      val rect = txt.getBoundingClientRect()
      (rect.bottom.toInt, rect.left.toInt) } --> hdlPositionCursor.contramap[(Int,Int)]{ case (bottom, left) => (bottom, left)}
  )

  private val focusCelUpdateField = SyncIO( focusCel.map { case txt =>
      val cel = txt.parentElement.asInstanceOf[html.TableCell]
      colFmt.zipWithIndex.filter{ case (_, idx) => idx == cel.cellIndex }.map { case (col, _) => col.field }.head
  } --> hdlFieldActive.contramap[String]{ field => field})

  private val focusCelUpdateRow = SyncIO(focusCel.map { case txt =>
                                                val row = txt.parentElement.parentElement.asInstanceOf[html.TableRow]
                                                row.rowIndex
                                          }  --> hdlRowActive.contramap[Int]{row => row})


  val actionsGrid = SyncIO(
      onKeyUp.map { k =>
        k.keyCode match {
          case KeyCode.Insert => "Insert"
          case KeyCode.Delete => "Delete"
          case _ => "Nothing"
        }
      } --> hdlActionsGrid
  )

  protected val hiddeGridCatalogsLostFocus = SyncIO(onBlur.use((0,0)) --> hdlPositionCursor.contramap[(Int,Int)](pos => pos))

  protected val onEscape = onKeyDown.filter(_.keyCode == KeyCode.Escape)
  protected val restarPosition = onEscape.use((0,0)) --> hdlPositionCursor.contramap[(Int,Int)](pos => pos)
  protected val restarCatalog = onEscape.use(("","")) --> hdlChangingValue.contramap[(String, String)](catalog => catalog)

  //def render(items: Map[Int, Map[String, String]]) = {
  def render[A: TransformData](items: List[A] ) = {
      val ls = items.zipWithIndex.map{ case (item, i) => i -> TransformData(item) }.toMap
      table(
        id := "tblGrid", cls := "tblGrid",
        thead( 
          tr( 
            colFmt.map { t => 
                td(t.title, t.styleTitle.getOrElse(VDomModifier(display.none)))
            }
          ) 
        ),
        tbody(
          eventsGrid,
          actionsGrid,
          showDetails(ls),
          //hiddeGridCatalogs,
          restarCatalog, restarPosition
        )
      )
    }
}

/********************
val clickableView: EmitterBuilder[Boolean, VDomModifier] = EmitterBuilder.ofModifier { sink: Observer[Boolean] =>
      VDomModifier(
        display.flex,
        minWidth := "0px",

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
      _ <- OutWatch.renderInto("#app", node)

      element = document.getElementById("strings")
      _ = element.innerHTML shouldBe ""

      _ = sendEvent(element, "mousedown")
      _ <- monix.eval.Task.unit.delayResult(0.1.seconds).to[IO]
      _ = element.innerHTML shouldBe "yes"

      _ = sendEvent(element, "mouseup")
      _ <- monix.eval.Task.unit.delayResult(0.1.seconds).to[IO]
      _ = element.innerHTML shouldBe "no"
    } yield succeed
}
*/