package outwatch_components

trait FormatNumber {
  def addComa(str: String): String = {
    val partEntera = str.substring(0, str.length - 3)
    val partDecimal = str.substring(str.length - 3, str.length)

    val miles: String = if (partEntera.length > 6) {
      val len = partEntera.length
      partEntera.substring(0, len - 6) + "," + partEntera.substring(len - 6, (len - 3)) + "," + partEntera.substring(len - 3, len)
    } else if (partEntera.length > 3) {
      partEntera.substring(0, partEntera.length - 3) + "," + partEntera.substring(partEntera.length - 3, partEntera.length)
    } else partEntera
    miles + partDecimal
  }

  def fmtMiles(valor: BigDecimal, dec: Int = 2): String = {
    import scala.scalajs.js.JSNumberOps._
    val newValor = if (dec > 0) valor.toDouble.toFixed(dec) else valor.toDouble.toFixed(0)
    addComa(newValor)
  }
}
