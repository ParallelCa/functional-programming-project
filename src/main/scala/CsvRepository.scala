import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths, StandardOpenOption}
import java.time.ZonedDateTime
import scala.io.{Codec, Source}
import scala.util.Try

object CsvRepository {
  private val csvFileName = "energy_data.csv"

  // CSV field escaping
  def escapeCsv(field: String): String = {
    if (field.contains(",") || field.contains("\"") || field.contains("\n"))
      s"\"${field.replace("\"", "\"\"")}\""
    else
      field
  }

  // Save full CSV content (including header)
  def saveCsv(content: String): Unit = {
    Files.write(
      Paths.get(csvFileName),
      content.getBytes(StandardCharsets.UTF_8),
      StandardOpenOption.CREATE,
      StandardOpenOption.TRUNCATE_EXISTING
    )
  }

  // Split a CSV line into fields, supporting quoted fields
  private def splitCsvLine(line: String): Seq[String] =
    line.split(",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)", -1)
      .map(_.stripPrefix("\"").stripSuffix("\""))

  // Generic: load any type of data from CSV by providing a parser from fields to T
  def loadFromCsv[T](parser: Seq[String] => Option[T]): Seq[T] = {
    val path = Paths.get(csvFileName)
    if (!Files.exists(path)) {
      println(s"File $csvFileName does not exist; please run the Data Collection feature first.")
      Seq.empty
    } else {
      val source = Source.fromFile(csvFileName)(Codec.UTF8)
      try {
        source
          .getLines()
          .drop(1)
          .flatMap { line =>
            val cols = splitCsvLine(line)
            parser(cols)
          }
          .toSeq
      } finally {
        source.close()
      }
    }
  }

  // DataRecord represents an energy data record
  case class DataRecord(
                         datasetId: Int,
                         energyType: String,
                         startTime: ZonedDateTime,
                         endTime: ZonedDateTime,
                         value: Double
                       )

  // Parse a row of fields into a DataRecord
  def parseRecord(fields: Seq[String]): Option[DataRecord] = for {
    dsId  <- Try(fields(0).toInt).toOption
    etype <- Try(fields(1)).toOption
    st    <- Try(ZonedDateTime.parse(fields(2))).toOption
    et    <- Try(ZonedDateTime.parse(fields(3))).toOption
    v     <- Try(fields(4).toDouble).toOption
  } yield DataRecord(dsId, etype, st, et, v)

  def loadRecordsFromCsv(): Seq[DataRecord] =
    loadFromCsv(parseRecord)
}
