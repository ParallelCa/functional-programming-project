import java.time.{DayOfWeek, ZonedDateTime}
import java.time.format.DateTimeFormatter
import scala.io.StdIn
import scala.util.Try
import ujson._

object FingridApiClient {
  private val dateTimeFormatter = FingridService.dateTimeFormatter
  private val csvFileName       = "energy_data.csv"
  private val apiKey            = "ebb78b9ad058437ab531d570d40a4746"

  // General higher-order function: retry mechanism
  private def retry[A](n: Int)(block: => Either[String, A]): Either[String, A] = {
    def attempt(remaining: Int): Either[String, A] = block match {
      case success @ Right(_) => success
      case Left(err) if remaining > 1 => attempt(remaining - 1)
      case failure @ Left(_) => failure
    }
    attempt(n)
  }

  // General higher-order function: log the result and return it
  private def logEither[A](either: Either[String, A], onSuccess: A => String): Either[String, A] = {
    either.fold(
      err => { println(err); Left(err) },
      v   => { println(onSuccess(v)); Right(v) }
    )
  }

  // Unified user input reading + parsing, curried
  private def readPrompt[A](msg: String)
                           (parse: String => Option[A]): Either[String, A] = {
    val input = StdIn.readLine(msg)
    parse(input).toRight(s"Invalid input: $input")
  }

  // Returns a new JSON AST where the "value" field of the first element in the "data" array is replaced with newVal, without modifying the original JSON object.
  private def replaceValueInData(json: Value, newVal: Double): Value = json match {
    case obj: Obj =>
      val fieldsMap = obj.value.toMap
      val updatedFieldsMap = fieldsMap.map {
        case ("data", Arr(arr)) if arr.nonEmpty =>
          val updatedArr = arr.head match {
            case hObj: Obj =>
              val updatedHeadFields = hObj.value.toMap.map {
                case (k, _) if k == "value" => k -> Num(newVal)
                case other                   => other
              }.toSeq
              val newHead = if (updatedHeadFields.nonEmpty)
                Obj(updatedHeadFields.head, updatedHeadFields.tail: _*)
              else Obj()
              newHead +: arr.tail.toSeq
            case other => other +: arr.tail.toSeq
          }
          "data" -> Arr(updatedArr: _*)
        case other => other
      }
      val entries = updatedFieldsMap.toSeq
      if (entries.nonEmpty) Obj(entries.head, entries.tail: _*) else Obj()

    case other => other
  }

  // Core functionality: monitoring and adjusting
  def monitorAndAdjustOperation(): Unit = {
    println("[Monitoring and Adjustment Feature]")
    println("Select the energy type to monitor:")
    println("1: Wind (datasetId = 181)")
    println("2: Hydro (datasetId = 191)")
    println("3: Solar (datasetId = 248)")
    val datasetId = readPrompt("Enter your choice (1/2/3): ") {
      case "1" => Some(181)
      case "2" => Some(191)
      case "3" => Some(248)
      case _   => None
    }.fold(
      _ => { println("Invalid choice, defaulting to Wind (datasetId = 181)"); 181 },
      v => v
    )

    val endTime   = ZonedDateTime.now
    val startTime = endTime.minusMinutes(30)
    println(s"Fetching current data for time range: ${startTime.format(dateTimeFormatter)} to ${endTime.format(dateTimeFormatter)}")

    val queryParams = QueryParameters(
      datasetId           = datasetId,
      startTime           = Some(startTime),
      endTime             = Some(endTime),
      format              = "json",
      oneRowPerTimePeriod = false,
      page                = 1,
      pageSize            = 10,
      locale              = "en",
      sortBy              = Some("startTime"),
      sortOrder           = "desc"
    )

    val rawDataEither = retry(3) {
      FingridService.getDatasetData(queryParams, apiKey)
        .left.map(err => s"API call failed: $err")
        .flatMap(data => if (data.nonEmpty) Right(data) else Left("Received empty data."))
    }
    val jsonEither = rawDataEither.flatMap(data =>
      Try(read(data)).toEither.left.map(e => s"Error parsing JSON data: ${e.getMessage}")
    )
    val valueEither = for {
      json    <- jsonEither
      dataArr <- json.obj.get("data").toRight("Data format unexpected; cannot extract value.")
      head    <- dataArr.arr.headOption.toRight("Data format unexpected; cannot extract value.")
      value   <- Try(head("value").num).toEither.left.map(e => s"Error extracting value: ${e.getMessage}")
    } yield value

    valueEither.fold(
      err => println(err),
      v   => println(s"Current retrieved value: $v")
    )

    val modify = readPrompt("Do you want to modify the data value? (y/n): ")(s =>
      Some(s.trim.equalsIgnoreCase("y"))
    ).getOrElse(false)

    if (modify) {
      val editEither = for {
        json    <- jsonEither
        dataArr <- json.obj.get("data").toRight("Data format unexpected; cannot modify.")
        _       <- if (dataArr.arr.nonEmpty) Right(()) else Left("Data format unexpected; cannot modify.")
        newVal  <- readPrompt("Enter new value: ")(s => Try(s.toDouble).toOption)
      } yield {
        replaceValueInData(json, newVal)
        newVal
      }

      editEither.fold(
        err  => println(err),
        newV => println(s"Data updated to: $newV")
      )
    } else {
      println("Data was not modified.")
    }
  }

  // Data collection
  def dataCollectionOperation(): Unit = {
    println("[Data Collection Feature]")
    println("Enter the query time range:")

    val startTimeOpt = readPrompt("Enter the start time (e.g., 2025-04-01T00:00:00Z): ") { s =>
      Try(ZonedDateTime.parse(s)).toOption
    }.toOption

    val endTimeOpt = readPrompt("Enter the end time   (e.g., 2025-04-01T23:59:59Z): ") { s =>
      Try(ZonedDateTime.parse(s)).toOption
    }.toOption

    (startTimeOpt, endTimeOpt) match {
      case (Some(startTime), Some(endTime)) =>
        val energyDatasets = Seq(("Wind", 181), ("Hydro", 191), ("Solar", 248))
        val delayMillis    = 3000

        val allRecords = energyDatasets.foldLeft(Seq.empty[String]) { case (acc, (energyType, datasetId)) =>
          println(s"Fetching $energyType data ...")
          val qp = QueryParameters(
            datasetId           = datasetId,
            startTime           = Some(startTime),
            endTime             = Some(endTime),
            format              = "json",
            oneRowPerTimePeriod = false,
            page                = 1,
            pageSize            = 20000,
            locale              = "en",
            sortBy              = Some("startTime"),
            sortOrder           = "asc"
          )

          val recordsForTypeEither = retry(3) {
            for {
              data <- FingridService.getDatasetData(qp, apiKey)
                .left.map(err => s"API call to fetch $energyType data failed: $err")
                .flatMap(d => if (d.nonEmpty) Right(d) else Left(s"$energyType data returned empty."))
              json <- Try(read(data)).toEither.left.map(e => s"Error parsing $energyType data: ${e.getMessage}")
              arr  <- json.obj.get("data").toRight(s"$energyType data is empty or not in expected format.")
            } yield arr.arr.map { record =>
              Seq(
                record("datasetId").num.toInt.toString,
                energyType,
                record("startTime").str,
                record("endTime").str,
                record("value").num.toString
              ).map(CsvRepository.escapeCsv).mkString(",")
            }.toSeq
          }

          val recordsForType = recordsForTypeEither.fold(err => { println(err); Seq.empty }, identity)
          Thread.sleep(delayMillis)
          acc ++ recordsForType
        }

        if (allRecords.nonEmpty) {
          val header     = Seq("datasetId","energyType","startTime","endTime","value").map(CsvRepository.escapeCsv).mkString(",")
          val csvContent = (header +: allRecords).mkString("\n")
          Try(CsvRepository.saveCsv(csvContent)).fold(
            err => println(s"Error saving CSV file: ${err.getMessage}"),
            _   => println(s"All data has been successfully saved to $csvFileName")
          )
        } else {
          println("No data retrieved; CSV file not created.")
        }

      case _ =>
        println("Invalid time format. Please use RFC3339 (e.g., 2025-04-01T00:00:00Z).")
    }
  }

  // View saved data
  def viewSavedDataOperation(): Unit = {
    println("--- View Energy Generation and Storage Data ---")
    val records = CsvRepository.loadFromCsv(CsvRepository.parseRecord)
    if (records.isEmpty) return

    val energyByType = records.groupBy(_.energyType).view.mapValues { recs =>
      val intervalMin = if (recs.head.energyType == "Solar") 15 else 3
      recs.map(_.value * intervalMin / 60.0).sum
    }.toMap

    val overall = energyByType.values.sum

    println("%-6s  %-18s  %s".format("Source","Total Energy (kWh)","Percentage"))
    println("-" * 50)
    Seq("Solar","Hydro","Wind").foreach { etype =>
      energyByType.get(etype).foreach { tot =>
        val pct = tot / overall * 100
        println(f"$etype%-6s  $tot%14.2f kWh  $pct%8.2f%%")
      }
    }
    println("-" * 50)
    println(f"Total      ${overall}%14.2f kWh\n")

    println("Source | Energy Bar (scaled to percentage)")
    println("-" * 50)
    val barWidth = 30
    Seq("Solar","Hydro","Wind").foreach { etype =>
      energyByType.get(etype).foreach { tot =>
        val pct    = tot / overall * 100
        val hashes = "#" * ((pct / 100 * barWidth).round.toInt)
        println(f"$etype%-6s | ${hashes}%-30s ${pct}%5.2f%% (${tot}%10.2f kWh)")
      }
    }
  }

  // Statistical calculations
  def computeStats(values: Seq[Double]): (Double, Double, Seq[Double], Double, Double) = {
    val sorted   = values.sorted
    val n        = sorted.length
    val mean     = if (n > 0) sorted.sum / n else Double.NaN
    val median   = if (n % 2 == 1) sorted(n / 2) else (sorted(n/2 -1) + sorted(n/2)) / 2
    val freq     = sorted.groupBy(identity).view.mapValues(_.size)
    val maxCount = if (freq.nonEmpty) freq.values.max else 0
    val mode     = freq.filter(_._2 == maxCount).keys.toSeq
    val range    = if (n > 0) sorted.last - sorted.head else Double.NaN
    val mid      = if (n > 0) (sorted.head + sorted.last) / 2 else Double.NaN
    (mean, median, mode, range, mid)
  }

  // Data analysis
  def analysisOperation(): Unit = {
    println("--- Data Analysis Feature ---")
    val records = CsvRepository.loadFromCsv(CsvRepository.parseRecord)
    if (records.isEmpty) return

    // 1. Read optional start time
    val startStr = StdIn.readLine(
      "Please enter the start time for analysis (e.g., 2025-04-01T00:00:00Z), leave blank for no limit:"
    ).trim
    val startFilter = if (startStr.isEmpty) None
    else Try(ZonedDateTime.parse(startStr)).toOption

    // 2. Read optional end time
    val endStr = StdIn.readLine(
      "Please enter the end time   (e.g., 2025-04-02T00:00:00Z), leave blank for no limit:"
    ).trim
    val endFilter = if (endStr.isEmpty) None
    else Try(ZonedDateTime.parse(endStr)).toOption

    // Original type filter input
    val typesInput = StdIn.readLine(
      "Optional energy types (Wind, Hydro, Solar), comma-separated, leave blank for all:"
    ).split(",").map(_.trim).filter(_.nonEmpty)

    val filtered = records.filter { r =>
      startFilter.forall(!r.startTime.isBefore(_)) &&
        endFilter.forall(!r.startTime.isAfter(_)) &&
        (if (typesInput.nonEmpty) typesInput.contains(r.energyType) else true)
    }

    if (filtered.isEmpty) {
      println("No data matches the criteria.")
      return
    }

    StdIn.readLine("Select grouping granularity: 1-Hour, 2-Day, 3-Week, 4-Month, 5-No grouping:").trim match {
      case "5" =>
        val (mean, median, mode, range, mid) = computeStats(filtered.map(_.value))
        println(f"Overall Data -> Mean: $mean%.2f, Median: $median%.2f, Mode: ${mode.mkString("|")}, Range: $range%.2f, Midrange: $mid%.2f")
      case gran =>
        val groups = filtered.groupBy { r =>
          gran match {
            case "1" => r.startTime.format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:00"))
            case "2" => r.startTime.toLocalDate.toString
            case "3" => r.startTime.toLocalDate.`with`(DayOfWeek.MONDAY).toString + " week"
            case "4" => s"${r.startTime.getYear}-${f"${r.startTime.getMonthValue}%02d"}"
            case _   => "Unknown"
          }
        }
        println("%-20s %-8s %-8s %-12s %-8s %-8s".format("Group","Mean","Median","Mode","Range","Midrange"))
        groups.toSeq.sortBy(_._1).foreach { case (grp, recs) =>
          val (mean, median, mode, range, mid) = computeStats(recs.map(_.value))
          println(f"$grp%-20s $mean%8.2f $median%8.2f ${mode.mkString("|")}%12s $range%8.2f $mid%8.2f")
        }
    }
  }

  // Detect and handle issues
  def detectAndHandleIssues(): Unit = {
    println("--- Detect and Handle Issues Feature ---")

    // 1. Read optional start time
    val startStr = StdIn.readLine(
      "Enter the start time (e.g., 2025-04-01T00:00:00Z), leave blank for no limit:"
    ).trim
    val startOpt = if (startStr.isEmpty) None
    else Try(ZonedDateTime.parse(startStr)).toOption

    // 2. Read optional end time
    val endStr = StdIn.readLine(
      "Enter the end time   (e.g., 2025-04-02T00:00:00Z), leave blank for no limit:"
    ).trim
    val endOpt = if (endStr.isEmpty) None
    else Try(ZonedDateTime.parse(endStr)).toOption

    val typeInput = StdIn.readLine("Select energy type (Wind, Hydro, Solar), leave blank for all:").trim
    val types     = if (Set("Wind","Hydro","Solar")(typeInput)) Set(typeInput) else Set("Wind","Hydro","Solar")

    val thresholdEither = readPrompt("Enter the minimum energy output threshold:")(s => Try(s.toDouble).toOption)
    val threshold = thresholdEither.fold(
      _ => { println("Invalid numeric input."); return },
      v => v
    )

    val filtered = CsvRepository.loadFromCsv(CsvRepository.parseRecord).filter { r =>
      startOpt.forall(!r.startTime.isBefore(_)) &&
        endOpt.forall(!r.endTime.isAfter(_))   &&
        types.contains(r.energyType)            &&
        r.value < threshold
    }

    if (filtered.isEmpty) {
      println("No alerts.")
    } else {
      println("Alerts!")
      println("%-10s %-6s %-25s %-25s %s".format("datasetId","Type","StartTime","EndTime","Value"))
      filtered.foreach { r =>
        println(f"${r.datasetId}%-10d ${r.energyType}%-6s ${r.startTime.format(dateTimeFormatter)}%-25s ${r.endTime.format(dateTimeFormatter)}%-25s ${r.value}%.2f")
      }
    }
  }
  @annotation.tailrec
  private def loop(): Unit = {
    println("Select an operation mode:")
    println("1: Monitoring and Adjustment")
    println("2: Data Collection")
    println("3: View Saved Data")
    println("4: Data Analysis")
    println("5: Detect and Handle Issues")
    println("6: Exit")
    StdIn.readLine("Enter your choice (1/2/3/4/5/6): ") match {
      case "1" => monitorAndAdjustOperation()
      case "2" => dataCollectionOperation()
      case "3" => viewSavedDataOperation()
      case "4" => analysisOperation()
      case "5" => detectAndHandleIssues()
      case "6" => println("Exiting."); return
      case _   => println("Invalid choice, please try again.")
    }
    println()
    loop()
  }

  def main(args: Array[String]): Unit = loop()
}
