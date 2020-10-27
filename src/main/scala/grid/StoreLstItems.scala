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

trait StoreLstItems {

    sealed trait ActionsStore
    case object Clean extends ActionsStore
    case class InsertItem(row: Int) extends ActionsStore
    case class DeleteItem(row: Int) extends ActionsStore

    case class UpdateItem(row: Option[Int], key: String, value: String, colTransition: Option[Int]) extends ActionsStore

    case class UpdateLstItems(l: Map[Int, Map[String, String]]) extends ActionsStore
    case class SetDefaultValues(items: Map[String, String]) extends ActionsStore
    case class UpdateRowActive(row: Int) extends ActionsStore
    case class ItemNew(row: Int)

    case class UpdateTransitionColumn(col: Int) extends ActionsStore

    case class State( items: Map[Int, Map[String, Any]],
                      defaultValues: Option[Map[String, String]],
                      rowActive: Int,
                      itemNew: ItemNew,
                      transition: (Int, Int) = (0,0) )

  val reduce: Reducer[ActionsStore, State] = Reducer {
      case (s, Clean) => s.copy(items = Map.empty[Int, Map[String, Any]])
      case (s, SetDefaultValues(defaultValues)) =>
        s.copy(defaultValues = Some(defaultValues))

      case (s, UpdateLstItems(lst)) => //lst.size start on 1 but, others types start on 0.

            val r = lst.size
            val lstPlusNew = s.defaultValues match {
                  case Some(value) =>
                        val newVal = (r -> (Map("renglon" -> (r + 1).toString()) ++ value))
                        lst + newVal
                  case None => lst + ( r -> Map("renglon" -> (r + 1).toString() ) )
            }

            s.copy(items = lstPlusNew, rowActive = r)

      case (s, UpdateRowActive(r)) => s.copy(rowActive = r)
      case (s, InsertItem(r)) =>
        val newItem = ( r -> s.defaultValues.getOrElse(Map.empty[String, Any]))
        val itemsUpdated = s.items + newItem
        s.copy( items = itemsUpdated, rowActive = (r + 1), itemNew = s.itemNew.copy(row = r))
      case (s, UpdateItem(row, key, value, colTransition)) =>
        val rowtoUpdate = row.getOrElse(s.rowActive)
        val curItem = s.items.getOrElse(rowtoUpdate, Map.empty[String, Any])
        val newItem: (Int, Map[String, Any]) = (rowtoUpdate -> (curItem + (key -> value)))

        val state2 = colTransition match {
          case Some(col) =>
            s.copy(transition = ( s.rowActive, col))
          case None => s
        }

        state2.copy ( items = s.items + newItem)
      case (s, DeleteItem(r)) =>
        val curItem = s.items.getOrElse(r, Map.empty[String, Any])
        s.copy(items = (s.items - r), rowActive = (r - 1))
      case (s, UpdateTransitionColumn(col)) => s.copy(transition = (s.rowActive, col))
      case (s, otros) =>
        println ( s"¡&#%! Algo malo paso y no sabemos que ¡&#%! action: $otros" )
        s
   }

}