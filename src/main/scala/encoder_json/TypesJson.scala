package encoder_json.types

sealed trait JsonValue
case class JsonObject(fields: List[ ( String, JsonValue ) ] ) extends JsonValue
case class JsonString(value: String) extends JsonValue
case class JsonDouble(value: Double) extends JsonValue
case class JsonInt(value: Int) extends JsonValue
case class JsonBoolean(value: Boolean) extends JsonValue
case class JsonArray(items: List[JsonValue]) extends JsonValue
case object JsonNull extends JsonValue