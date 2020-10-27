package outwatch_components

import org.scalajs.dom

class FileSlicer(file: dom.File) {

  val sliceSize: Int = 1024 * 40

  val valor = file.size / sliceSize
  val slices = math.ceil(valor).toInt
  var currentSlice = 0

  def getNextSlice() = {
    var start = currentSlice * sliceSize
    var end = Math.min((currentSlice + 1) * sliceSize, file.size)
    currentSlice = currentSlice + 1
    file.slice(start, end)
  }

}
