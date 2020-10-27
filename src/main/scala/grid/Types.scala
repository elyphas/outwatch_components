package outwatch_components

import shapeless._

//import outwatch.dsl._
import outwatch._
import encoder_json.types._

final case class ColG (   
    field: String, 
    title: Option[String] = None, 
    styleTitle: Option[VDomModifier] = None, 
    styleCell: Option[VDomModifier] = None,
    typeNumber: String = "",
    inputType: String = "input",
    edit: Boolean = false 
)

final case class ColG_Json (
    field: (String, JsonValue),
    title: Option[String] = None,
    styleTitle: Option[VDomModifier] = None,
    styleCell: Option[VDomModifier] = None,
    typeNumber: String = "",
    inputType: String = "input",
    edit: Boolean = false
)

/*case class ColG_Witness (
                  field: String,
                  fieldWitness: Symbol,
                  title: Option[String] = None,
                  styleTitle: Option[VDomModifier] = None,
                  styleCell: Option[VDomModifier] = None,
                  typeNumber: String = "",
                  inputType: String = "input",
                  edit: Boolean = false
                )*/

final case class Column(c: Int) 
final case class Row(r: Int)
