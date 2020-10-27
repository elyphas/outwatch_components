package outwatch_components

case class Fechas(fecha: String)
import java.util.Date

import shapeless._
import shapeless.record._
import scala.scalajs.js

//private[outwatch_components] 
trait TransformData[A] {
    def transform(a: A): Map[String, String]
}

//private[outwatch_components] 
object TransformData {

  implicit def genericTransform[A, ARepr <: HList](implicit gen: LabelledGeneric.Aux[A, ARepr],
                                                   toMap: ops.record.ToMap[ARepr]
                                                  ): TransformData[A] = 
        new TransformData[A] {
              override def transform(rpt: A): Map[String, String] = {
                  val genRecord = gen.to(rpt)
                  toMap(genRecord).map { case (k: Symbol, value) =>
                        val valor = value match {
                          case date: Date =>
                              val jsDate = new js.Date(date.getTime().toDouble)
                              (jsDate.getDay()+1) + "/" + (jsDate.getMonth()+1) + "/" + jsDate.getFullYear()
                          case Fechas(f) => f
                          case Some(v) => v
                          case v => if ( v == None) "" else v
                        }
                        k.name.toString -> valor.toString
                  }
              }
        }
  def apply[A](rpt: A)(implicit transformer: TransformData[A]): Map[String, String] = transformer.transform(rpt)
}
