package course.cohort4

// - personal projects
// - books (red book)
// - teaching
// - open source
sealed trait Json {
  override def toString: String =
    this match {
      case Json.JsonString(value)  => s"\"$value\""
      case Json.JsonNumber(value)  => value.toString
      case Json.JsonBoolean(value) => value.toString
      case Json.JsonNull           => "null"
      case Json.JsonArray(value)   => value.mkString("[", ",", "]")
      case Json.JsonObject(value)  => value.map { case (k, v) => s""""$k":$v""" }.mkString("{", ",", "}")
    }
}

object Json {
  final case class JsonString(string: String)         extends Json
  final case class JsonNumber(number: Double)         extends Json
  final case class JsonBoolean(boolean: Boolean)      extends Json
  case object JsonNull                                extends Json
  final case class JsonArray(array: Vector[Json])     extends Json
  final case class JsonObject(map: Map[String, Json]) extends Json
}

trait KeyEncoder[A] {
  def toJsonKey(a: A): String
}

object KeyEncoder {
  implicit val stringKeyCodec: KeyEncoder[String] =
    new KeyEncoder[String] {
      override def toJsonKey(string: String): String = string
    }
}

// A => B  B => C
trait Encoder[A] { self =>
  def toJson(a: A): Json

  def contramap[A0](f: A0 => A): Encoder[A0] =
    new Encoder[A0] {
      override def toJson(a0: A0): Json =
        self.toJson(f(a0))
    }
}

object Encoder {
  import Json._

  def apply[A](implicit jsonCodec: Encoder[A]): Encoder[A] = jsonCodec

  implicit val stringCodec: Encoder[String] = new Encoder[String] {
    override def toJson(string: String): Json = JsonString(string)
  }

  implicit val intCodec: Encoder[Int] = new Encoder[Int] {
    override def toJson(int: Int): Json = JsonNumber(int.toDouble)
  }

  implicit val doubleCodec: Encoder[Double] = new Encoder[Double] {
    override def toJson(double: Double): Json = JsonNumber(double)
  }

  implicit def listCodec[A](implicit aCodec: Encoder[A]): Encoder[List[A]] = new Encoder[List[A]] {
    // Container[A] => Container[B]
    // A => Json
    // Vector[A] => Vector[Json]
    override def toJson(as: List[A]): Json =
      JsonArray(as.toVector.map(aCodec.toJson))
  }

  implicit def mapCodec[K, V](implicit
      kKeyCodec: KeyEncoder[K],
      vCodec: Encoder[V]
  ): Encoder[Map[K, V]] = new Encoder[Map[K, V]] {
    // Map[K, V] => Map[String, Json]
    override def toJson(map: Map[K, V]): Json = {
      val newMap: Map[String, Json] = map.map { case (k, v) =>
        (kKeyCodec.toJsonKey(k), vCodec.toJson(v))
      }
      JsonObject(newMap)
    }
  }
}

case class ModelName(name: String) extends AnyVal

object ModelName {
  implicit val encoder: Encoder[ModelName] =
    Encoder[String].contramap(_.name)
}

case class RunId(int: Int) extends AnyVal

object RunId {
  implicit val encoder: Encoder[RunId] =
    Encoder[Int].contramap[RunId](_.int)
}

sealed trait Category

object Category {
  case object Food   extends Category
  case object Drink  extends Category
  case object Snacks extends Category

  implicit val keyEncoder: KeyEncoder[Category] = new KeyEncoder[Category] {
    override def toJsonKey(category: Category): String =
      category match {
        case Food   => "FOOD"
        case Drink  => "DRINK"
        case Snacks => "SNACKS"
      }

  }
}

case class ModelData(
    name: String,
    score: Double,
    errorMargin: Int
)

object ModelData {
  implicit val encoder: Encoder[ModelData] =
//    DeriveEncoder.gen[ModelData]
    new Encoder[ModelData] {
      override def toJson(a: ModelData): Json =
        Json.JsonObject(
          Map(
            "name"        -> Encoder[String].toJson(a.name),
            "score"       -> Encoder[Double].toJson(a.score),
            "errorMargin" -> Encoder[Int].toJson(a.errorMargin)
          )
        )
    }
}

// A x B x C
// json(A) x json(B) x json(C)
// typeclass(case class)
// typeclass(modelName) x typeclass(runId) x typeclass(modelData)
case class PreferenceScores(
    modelName: ModelName,
    data: Map[Category, ModelData],
    history: List[RunId]
)

object PreferenceScores {
  implicit val codec: Encoder[PreferenceScores] =
    new Encoder[PreferenceScores] {
      override def toJson(scores: PreferenceScores): Json =
        Json.JsonObject(
          Map(
            "modelName" -> Encoder[ModelName].toJson(scores.modelName),
            "data"      -> Encoder[Map[Category, ModelData]].toJson(scores.data),
            "history"   -> Encoder[List[RunId]].toJson(scores.history)
          )
        )
    }
}

object Examples extends App {
  val exampleScore1 =
    PreferenceScores(
      ModelName("model1"),
      Map(
        Category.Food   -> ModelData("hamburger", 0.5, 1),
        Category.Drink  -> ModelData("coca cola", 0.5, 1),
        Category.Snacks -> ModelData("reese's pieces", 0.5, 1)
      ),
      List(RunId(1), RunId(2), RunId(3))
    )

  val exampleScore2 =
    PreferenceScores(
      ModelName("model2"),
      Map(
        Category.Food   -> ModelData("hamburger", 0.5, 1),
        Category.Drink  -> ModelData("coca cola", 0.5, 1),
        Category.Snacks -> ModelData("reese's pieces", 0.5, 1)
      ),
      List(RunId(1), RunId(2), RunId(3))
    )

  val exampleScores: List[PreferenceScores] =
    List(exampleScore1, exampleScore2)

  ExternalApi.send(exampleScores.toJson)

  implicit class JsonOps[A](val self: A) {
    def toJson(implicit encoder: Encoder[A]): Json =
      encoder.toJson(self)
  }
}
// json encoders and decoders
// custom domain model

object ExternalApi {
  def send(json: Json): Unit =
    println(s"Sending ${json.toString} to my mysterious backend service")
}

object DeriveEncoder {
  def gen[A]: Encoder[A] = ???
}
