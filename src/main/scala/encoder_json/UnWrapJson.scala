package unwrapjson

import encoder_json.types._

trait UnWrapJson[ +A ] {
    type Out
    def apply[A]( a: A ): Out
}

object UnWrapJson {
    type Aux[A, B] = UnWrapJson[A] { type Out = B}
    implicit def unWrapJsonString: UnWrapJson.Aux[JsonString, String] = new UnWrapJson [JsonString] {
        type Out = String
        def apply[JsonString](json: JsonString) = json.toString
    }

    implicit def unWrapJsonInt: UnWrapJson.Aux[JsonInt, String] = new UnWrapJson [JsonInt] {
        type Out = String
        def apply[JsonInt](json: JsonInt) = json.toString
    }

}