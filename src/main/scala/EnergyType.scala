sealed trait EnergyType {
  def name: String
  def datasetId: Int
  def intervalMin: Int
}
object EnergyType {
  case object Wind  extends EnergyType { val name = "Wind";  val datasetId = 181; val intervalMin = 3  }
  case object Hydro extends EnergyType { val name = "Hydro"; val datasetId = 191; val intervalMin = 3  }
  case object Solar extends EnergyType { val name = "Solar"; val datasetId = 248; val intervalMin = 15 }

  val all: Seq[EnergyType] = Seq(Wind, Hydro, Solar)

  def fromSelection(sel: String): EnergyType = sel match {
    case "1" => Wind
    case "2" => Hydro
    case "3" => Solar
    case _   => Wind
  }

  def fromName(name: String): Option[EnergyType] =
    all.find(_.name.equalsIgnoreCase(name))
}
