import java.time.ZonedDateTime

// Parameter object for building API requests
case class QueryParameters(
                            datasetId: Int,
                            startTime: Option[ZonedDateTime] = None,
                            endTime: Option[ZonedDateTime] = None,
                            format: String = "json",
                            oneRowPerTimePeriod: Boolean = false,
                            page: Int = 1,
                            pageSize: Int = 10,
                            locale: String = "en",
                            sortBy: Option[String] = None,
                            sortOrder: String = "asc"
                          )

// Domain model representing records converted from CSV or API responses
case class DataRecord(
                       datasetId: Int,
                       energyType: String,
                       startTime: ZonedDateTime,
                       endTime: ZonedDateTime,
                       value: Double
                     )
