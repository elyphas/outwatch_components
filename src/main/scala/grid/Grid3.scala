package outwatch_components

import cats.effect.{IO, Sync, SyncIO}
import cats.implicits._
import org.scalajs
import org.scalajs.dom
import org.scalajs.dom.html
import org.scalajs.dom.window.alert
import monix.execution.Scheduler.Implicits.global
import monix.reactive.subjects.PublishSubject
import monix.reactive.Observable
import outwatch.reactive.handlers.monix._
import colibri.Observer
import colibri.ext.monix._
import colibri.ext.monix.ops._
import encoder_json.HelpersLabelledGeneric.JsonEncoder

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

import unwrapjson._

import shapeless.{HList, LabelledGeneric, Typeable, Witness}
import shapeless.record._
import shapeless.ops.hlist.ToTraversable
import shapeless.ops.record.Fields

import encoder_json.types._

class Grid3[A]( colFmt: Seq[ColG_Json],
      eventsGrid: VDomModifier,
      eventsCell: VDomModifier,
      initValue: Map[String, String],
      transitionOnInsert: Int
      ) extends FormatNumber with StoreLstItems3 {

  val initState = State(  items = Map.empty[Int, Map[String, JsonValue]],
                          defaultValues = Map.empty[String, JsonValue],
                          rowActive = 0, colActive = 0,
                          fieldActive = ("", JsonNull),
                          itemNew = ItemNew(row = 0))

  private val store = Store.create[IO](Clean, initState, reduce).unsafeRunSync

  //Se debe quitar por que lo vamos a poner en el store. ********************************
  val hdlFieldActive = Handler.create[(String, JsonValue)](("",JsonNull)).unsafeRunSync

                                          //Key, Value
  val hdlChangingValue = PublishSubject[(String, JsonValue)]()

  val hdlPositionCursor = Handler.create[(Int, Int)]((0, 0)).unsafeRunSync

  val hdlActionsGrid = PublishSubject[Int]()

  val mountCellFocus = Observer.create[(dom.Element, String, String)] { case (elem, inputType, idTxt) =>
      if (idTxt == elem.id) {
        val txt = if ( inputType == "input" ) elem.asInstanceOf[html.Input] else elem.asInstanceOf[html.TextArea]
        Try {
              val range = dom.document.createRange()
              val sel = scalajs.dom.window.getSelection()
              sel.removeAllRanges()
              range.selectNodeContents(txt)
              sel.addRange(range)
        } match {
              case Success(value) => txt.focus
              case Failure(exception) => alert ( "Hubo un error al convertir el valor del componente: line:102 from Grid3 " + exception.getMessage( ) )
        }
      }
    }

  def contentTd ( idTd: String, valor: String, col: Int, row: Int ) = {
      val field = colFmt ( col )
      val idTxt = idTd + "txtEditable"
      val tabIdx = ( ( row + 1 ) * 10 ) + col

      val eventOnChanging = if ( field.inputType == "checkbox" ) onChangingValueChk else onChangingValueTxt

      val valorVDom = Try {
        if (field.inputType != "checkbox") VDomModifier ( value := valor )
        else VDomModifier(checked := (valor.nonEmpty && valor.toBoolean) )
        } match {
          case Success(value) => VDomModifier(value)
          case Failure(exception) =>
            alert ( "Hubo un error al convertir el valor del componente: line:102 from Grid3 " + exception.getMessage( ) )
            VDomModifier.empty
        }

        val domMount = SyncIO(onDomMount.transformLifted( (e: Observable[(dom.Element)] ) =>
            e.withLatestFrom(store.liftSource[monix.reactive.Observable])
            { case (elem, (action@_, state) ) =>
                  val rowActiv = state.transition._1.r
                  val colum = state.transition._2.c
                  (elem, field.inputType, "r" + rowActiv + "c" + colum + "txtEditable")
            })  --> mountCellFocus
          )

        val vdomM = VDomModifier( key := idTxt, id := idTxt, cls := "editCell", tabIndex := tabIdx, 
                                  field.styleCell.getOrElse(VDomModifier(display.none)) ,
                                  valorVDom,
                                  eventsCell, eventOnChanging, domMount, focusUpatePositionCursor, focusCelUpdateField, focusCelUpdateRow)
        Try {
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
          } match {
            case Success(value) => value
            case Failure(error) => 
              alert(error.getMessage)
              VDomModifier.empty
          }
  }

  def newCell(txt: String, col: Int, row: Int) = {
        Try {
           val idTd = "r" + row + "c" + col
           val eventsTd = VDomModifier.empty
           td( id := idTd, key := idTd, cls := "tdCell",
              contentTd(idTd, txt, col, row), eventsTd, colFmt(col).styleCell.getOrElse(VDomModifier(display.none) )
           )
        } match {
           case Success(value) => value
           case Failure(exception) =>
              alert("Hubo un error al crear una nueva celda: " + exception.getMessage)
              td()
        }
  }

  def wrapValue(newValue: String, fieldActive: JsonValue) =
         fieldActive match {
            case JsonString(value) => JsonString(newValue.toUpperCase)
            case JsonDouble(value) =>
            /**Here is going to be a validated from cats.*/
            val newValue2 = if(newValue == "") 0.0 else newValue.toDouble
            JsonDouble(newValue2)
         case JsonInt(value) =>
            /**Here is going to be a validated from cats.*/
            val newValue2 = Try {
                                if(newValue == "") 0 else newValue.toInt
                            } match {
                              case Success ( value ) => value
                              case Failure(error) => 
                                  alert(error.getMessage)
                                 0 
                            }
            JsonInt(newValue2)
         case _ => JsonString("Something lacks check it out in Grid3, line 146")
  }

  val onChangingValueTxt =
        SyncIO(
          onInput
             .value
             .transformLifted( (e: Observable[String]) =>
               e
               .distinctUntilChanged
               .withLatestFrom(hdlFieldActive){ case (txt: String, (field: String, currentValue: JsonValue)) =>
               //.withLatestFrom(store.liftSource[monix.reactive.Observable]){ case (txt: String, (action@_, state)) =>
                  val currentValue2: JsonValue = Try {
                      //val (field, currentValue) = state.fieldActive
                      wrapValue(txt, currentValue)
                  } match {
                    case Success(value) => value
                    case Failure(exception) => JsonNull
                  }
                  (field, currentValue2)
          }) --> /*store.contramap[(String, JsonValue)]( f => UpdateFieldActive(f))*/ hdlChangingValue
   )

   val onChangingValueChk = SyncIO(onInput.checked.transformLifted( (e: Observable[Boolean]) =>
           e
           .distinctUntilChanged
           .withLatestFrom(hdlFieldActive){ case (chk: Boolean, (field: String, currentValue: JsonValue) ) =>
           //.withLatestFrom(store.liftSource[monix.reactive.Observable]){ case (chk: Boolean, (action@_, state)) =>
                //(field, chk.toString)
                println("Ta pasando por el evento onChangingValueChk")
                //val (field, currentValue) = state.fieldActive
                (field, wrapValue(chk.toString, currentValue))
           }) --> /*store.contramap[(String, JsonValue)]( f => UpdateFieldActive(f))*/    hdlChangingValue
   )

   def srcChangingValue(  field: String, 
                          condit: String => Boolean
                          //lng: Int
                     ) = hdlChangingValue.debounce(800 milliseconds)
             .filter { case (fld, value@_) => fld == fld }
             .filter { case (fld@_, JsonString(txt)) => condit(txt) }
             .map { case (fld@_, JsonString(txt)) => txt }

   private val focusCel = onFocus.map { f =>
       f.stopPropagation()
       f.preventDefault()
       f.currentTarget.asInstanceOf[html.Input]
   }

   val observCombined = Observer.combineVaried (
         store.contramap[(String, JsonValue)]( f => UpdateFieldActive(f)),
         hdlFieldActive.contramap[(String, JsonValue)]( f => f)
   )

   private val focusCelUpdateField = SyncIO( focusCel.map { case txt =>
        val cel = txt.parentElement.asInstanceOf[html.TableCell]
        colFmt
            .zipWithIndex
            .filter { case (_, idx) => idx == cel.cellIndex }
            .map { case (col, _) => col.field }.head
         } --> /*store.contramap[(String, JsonValue)]( f => UpdateFieldActive(f))*/ observCombined
   )

   private val focusUpatePositionCursor = SyncIO(  focusCel.map { case txt =>
        val rect = txt.getBoundingClientRect()
           (rect.bottom.toInt, rect.left.toInt) 
        } --> hdlPositionCursor.contramap[(Int,Int)]{ case (bottom, left) => (bottom, left)})

   /*private val focusCelUpdateRow = SyncIO (  focusCel.map { case txt =>
       val row = txt.parentElement.parentElement.asInstanceOf[html.TableRow]
       row.rowIndex - 1
   }  --> store.contramap[Int](r => UpdateRowActive(r) ) )*/

   private val focusCelUpdateRow = SyncIO (  focusCel.map { case txt =>
        val row = txt.parentElement.parentElement.asInstanceOf[html.TableRow]
        val r = row.rowIndex -1
        val cel = txt.parentElement.asInstanceOf[html.TableCell]
        val c = colFmt.zipWithIndex.filter { case (_, idx) => idx == cel.cellIndex }
        .map { case (col, idx) =>
            idx
        }.head
        (Row(r),Column(c))
    } --> store.contramap[(Row,Column)](t=> UpdateTransition(t)))

    def connectEventsGrid = {
        val updateTransitionCol = store.contramap[(Row, Column)]{ case (r, c) => UpdateTransitionColumn(c.c) }
        val updateTransition = store.contramap[(Row, Column)]{ case t => UpdateTransition(t) }

        div (
           emitter(hdlChangingValue
              .withLatestFrom( store.liftSource[monix.reactive.Observable]) { case ((key, value), (action@_, state)) =>
                 (state.rowActive, key, value)
           }) --> store.contramap[(Int, String, JsonValue)]{case (r, k, v) => UpdateItem( Some(r), k, v, None) },

          emitter(hdlActionsGrid.filter(_ === KeyCode.Insert)
            .withLatestFrom(store.liftSource[monix.reactive.Observable]) { case (event, (action@_, state)) => 
            (Row(state.rowActive + 1), Column(transitionOnInsert))
          }) --> Observer.combineVaried(store.contramap[(Row, Column)]{ case (r,c) =>  InsertItem(r.r)}, updateTransition),

          emitter(hdlActionsGrid.filter( _ === KeyCode.Delete)
             .withLatestFrom(store.liftSource[monix.reactive.Observable]){ case (event, (action@_, state)) => 
                //(Row(state.rowActive - 1), Column(transitionOnInsert))
                (Row(state.rowActive), Column(transitionOnInsert))
             }) --> Observer.combineVaried( store.contramap[(Row, Column)]{ case (r, c) => DeleteItem(r.r)}, updateTransition),
          )
    }

    //It is going to update the active row, with (field and value) Map[String, String]
    def updateRowActive(values: Map[String, JsonValue], row: Option[Int] = None, colTransition: Option[Int]) =
           values.foreach { case (k, v) =>
             store.onNext( UpdateItem ( row, k, v, colTransition ) )
           }

    def actionOnEnter = hdlActionsGrid.filter ( _ == KeyCode.Enter )
           .withLatestFrom( store.liftSource[monix.reactive.Observable]
           ){ case (action@_, (act@_, state )) => state.fieldActive }

    //def getPositionCursor = store.map { case (action@_, state) => state.items.get(state.rowActive) }

    def insertRow(nextToRow: Option[Int]) = store.onNext(InsertItem2(nextToRow))

    /*emitter(hdlActionsGrid.filter(_ === KeyCode.Insert).withLatestFrom(store.liftSource[monix.reactive.Observable]) { case (event, (action@_, state)) =>
         (state.rowActive + 1, 3)      //val rowActiv = state.rowActive + 1    //hdlTransition.onNext(rowActiv, 3)
    }) --> Observer.combineVaried(store.contramap[(Int,Int)]{ case (r, c) => InsertItem(r)}, updateTransition)*/

    def getActiveDataRow = store.map { case (action@_, state) => state.items.get(state.rowActive) }

    def getRow(row: Int) = store.map{ case (action@_, state) => state.items.get(row)}

    def getActiveRowNumber = store.map { case (action@_, state) => state.rowActive}

  /*def loadValues[A: TransformData](items: List[A]) = {
           val ls = items
              .zipWithIndex
              .map{ case (item, i) => i -> TransformData(item) }
              .toMap
              store.onNext(UpdateLstItems(ls))
  }*/

  def loadValues[A: JsonEncoder](items: List[A]) = {
         val ls = items
               .zipWithIndex
               .map{ case (item, i) =>
                 val JsonObject(fields) = JsonEncoder[A].encode(item)
                 (i -> fields.toMap)
               }
               .toMap
               store.onNext(UpdateLstItems(ls))
  }

  //def loadValues(items: Map[Int, Map[String, String]]) = store.onNext(UpdateLstItems(items))

  //def setDefaultValues(defaultValues: Map[String, String]) = store.onNext(SetDefaultValues(defaultValues))
  def setDefaultValues(defaultValues: Map[String, JsonValue]) = store.onNext(SetDefaultValues(defaultValues))


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

  def unWrap(json: JsonValue) =
    Try {
        json match {
           case JsonString(value) => value
           case JsonDouble(value) => value.toString
           case JsonInt(value) => value.toString
           case JsonBoolean(value) => value.toString
           case JsonNull => ""
           case _ => "Hubo un problema en el Grid3, line 328"
        }
    } match {
        case Success(value) => value
        case Failure(error) => 
            alert(error.getMessage)
            ""
    }
  
  //def unWrap[ T <: JsonValue ](json: T)(implicit unwrapjson: UnWrapJson[T]): unwrapjson.Out = unwrapjson(json)

  def render = div(
        table( id := "tblGrid", cls := "tblGrid",
          thead(tr( colFmt.map(t => td(t.title, t.styleTitle.getOrElse(VDomModifier(display.none)))))),
          tbody(
            eventsGrid,
            actionsGrid,
            connectEventsGrid,
            store.map { case (action@_, state) =>
                //Se necesita manejar un Try-match
                state.items.toSeq.sortBy(_._1)
                  //.map { case (idRow: Int, fields: Map[String, Any]) =>
                  .map { case (idRow: Int, fields: Map[String, JsonValue]) =>
                    Try {
                      tr( id := "row" + idRow.toString,
                        colFmt.zipWithIndex.map { case ( col: ColG_Json, ii: Int ) =>

                          val currentValue = Try {
                                    fields.getOrElse ( col.field._1, JsonNull )
                          } match {
                              case Success(value) => value
                              case Failure(error) => 
                                alert(error.getMessage)
                                JsonNull
                          }

                          val currentValue2: String = Try { 
                                                        unWrap ( currentValue )
                            } match {
                              case Success(value) => value
                              case Failure(error) => alert(error.getMessage)
                                  ""
                            }
                          //newCell ( fields.getOrElse(col.field, "" ).toString , col = ii, row = idRow )
                          //val currentValue2: String = unWrapJson ( currentValue )
                          newCell ( currentValue2, col = ii, row = idRow )
                        }
                      )
                    } match {
                      case Success(value) => value
                      case Failure(exception) =>
                        alert( "Hubo un error al crear una fila: " + exception.getMessage() )
                        td()
                    }

                }.toList
            },
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
