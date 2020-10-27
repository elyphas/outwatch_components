package outwatch_components

import cats._
import cats.implicits._

import monix.execution.Ack.Continue
import monix.execution.{Ack, Cancelable}
import monix.execution.Scheduler.Implicits.global
import monix.execution.exceptions.DummyException

import monix.reactive.{Observable, Observer}
import outwatch.reactive.handlers.monix._
import monix.eval.Task

import outwatch._
import outwatch.dsl._

import outwatch.util.Reducer

import shapeless._
import encoder_json.types._
import encoder_json.HelpersLabelledGeneric._

trait StoreLstItems3 {

    sealed trait ActionsStore
    case object Clean extends ActionsStore
    case class InsertItem(row: Int) extends ActionsStore
    case class InsertItem2(row: Option[Int]) extends ActionsStore
    case class DeleteItem(row: Int) extends ActionsStore
    case class UpdateRowActive(row: Int) extends ActionsStore
    case class ItemNew( row: Int )

    case class UpdateFieldActive(field: (String, JsonValue)) extends ActionsStore

    case class UpdateTransitionColumn( col: Int ) extends ActionsStore
    case class UpdateTransition(transition: (Row,Column)) extends ActionsStore

    case class UpdateItem(row: Option[Int], key: String, value: JsonValue, colTransition: Option[Int]) extends ActionsStore

    case class UpdateLstItems( l: Map[Int, Map[String, JsonValue]]) extends ActionsStore
    case class SetDefaultValues(items: Map[String, JsonValue]) extends ActionsStore

    case class State( items: Map[Int, Map[String, JsonValue]],
                      defaultValues: Map[String, JsonValue],
                      fieldActive: (String, JsonValue) = ("", JsonNull),      //fieldActive: (String, JsonValue),
                      rowActive: Int,
                      colActive: Int,
                      itemNew: ItemNew,
                      transition: (Row, Column) = (Row(0),Column(0))    //row, column
                    )

  val reduce: Reducer[ActionsStore, State] = Reducer {
      case (s, Clean) => s.copy(items = Map.empty[Int, Map[String, JsonValue]])
      case (s, SetDefaultValues(defaultValues)) => s.copy(defaultValues = defaultValues)
      case (s, UpdateLstItems(lst)) => //lst.size start on 1 but, others types start on 0.
         /*val r = lst.size
         val lstPlusNew = s.defaultValues match {
            case Some(value) =>
              val newVal = (r -> (Map("renglon" -> (r + 1).toString()) ++ value))
              lst + newVal
            case None => lst + ( r -> Map("renglon" -> (r + 1).toString()))
         }
         s.copy(items = lstPlusNew, rowActive = r)*/
           s.copy(items = lst, rowActive = lst.size)
      case (s, UpdateRowActive(r)) => s.copy(rowActive = r)
      case (s, UpdateTransition(t)) => 
        val rowAct = t._1.r
        s.copy(transition = t, rowActive = rowAct)
      case (s, UpdateFieldActive(f)) => s.copy(fieldActive = f)
      case (s, InsertItem(r)) => // Insert when the Keyboard is press.
          val newItem = (r -> s.defaultValues)
          val itemsUpdated = s.items + newItem
          println("al insertar una fila")
          println(newItem)
          val newRowActive = s.items.size
          s.copy( items = itemsUpdated, /*rowActive = r,*/ itemNew = s.itemNew.copy(row = r))
      case (s, InsertItem2(r)) => //Insert by code.
          //val postInsert = r.getOrElse(s.rowActive + 1)
          val postInsert = r.getOrElse(s.items.size + 1)
          val newItem = (postInsert -> s.defaultValues)
          val itemsUpdated = s.items + newItem
          s.copy( items = itemsUpdated, rowActive = postInsert, itemNew = s.itemNew.copy(row = postInsert))
      case (s, UpdateItem(row, key, value, colTransition)) =>
        val rowtoUpdate = row.getOrElse(s.rowActive)
        val curItem = s.items.getOrElse(rowtoUpdate, Map.empty[String, JsonValue])  //Map.empty[String, Any] )
        //val newItem: (Int, Map[String, Any]) = (rowtoUpdate -> (curItem + (key -> value)))
        val newItem: (Int, Map[String, JsonValue]) = ( rowtoUpdate -> ( curItem + ( key -> value ) ) )
        val state2 = colTransition match {
          case Some ( col ) => s.copy ( transition = ( Row(s.rowActive), Column(col) ) )//s.copy ( transition = ( 0, 0 ), colActive = col )
          case None => s
        }
        state2.copy ( items = s.items + newItem )
      case (s, DeleteItem(r)) =>
        val curItem = s.items.getOrElse(r, Map.empty[String, Any])
        val transition = (Row(s.transition._1.r - 1), s.transition._2)

        s.copy(items = (s.items - r), rowActive = (r - 1) )
      case (s, UpdateTransitionColumn(col)) => s.copy(transition = (Row(s.rowActive), Column(col)))
      case (s, otros) =>
        println ( s"¡&#%! Algo malo paso y no sabemos que ¡&#%! action: $otros" )
        s
   }

}
