import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.net.{URI, URLEncoder}
import java.nio.charset.StandardCharsets
import java.time.format.DateTimeFormatter

object FingridService {
  // RFC3339 format
  val dateTimeFormatter: DateTimeFormatter =
    DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ssXXX")

  // Build URL query string
  private def buildQueryParams(params: QueryParameters): String = {
    val baseParams = Seq(
      "format"             -> params.format,
      "oneRowPerTimePeriod"-> params.oneRowPerTimePeriod.toString,
      "page"               -> params.page.toString,
      "pageSize"           -> params.pageSize.toString,
      "locale"             -> params.locale,
      "sortOrder"          -> params.sortOrder
    )
    val optionalParams = Seq(
      params.startTime.map(t => "startTime" -> t.format(dateTimeFormatter)),
      params.endTime  .map(t => "endTime"   -> t.format(dateTimeFormatter)),
      params.sortBy   .map(sb => "sortBy"    -> sb)
    ).flatten

    (baseParams ++ optionalParams).map { case (k, v) =>
      s"${URLEncoder.encode(k, StandardCharsets.UTF_8)}=${URLEncoder.encode(v, StandardCharsets.UTF_8)}"
    }.mkString("&")
  }

  // Call Fingrid API to get raw JSON string
  def getDatasetData(params: QueryParameters, apiKey: String): Either[String, String] = {
    val baseUrl     = s"https://data.fingrid.fi/api/datasets/${params.datasetId}/data"
    val queryString = buildQueryParams(params)
    val fullUrl     = s"$baseUrl?$queryString"

    try {
      val client  = HttpClient.newHttpClient()
      val uri     = new URI(fullUrl)
      val request = HttpRequest.newBuilder(uri)
        .header("x-api-key", apiKey)
        .header("Cache-Control", "no-cache")
        .GET()
        .build()
      val response = client.send(request, HttpResponse.BodyHandlers.ofString())
      if (response.statusCode >= 200 && response.statusCode < 300)
        Right(response.body())
      else
        Left(s"Error: Received HTTP ${response.statusCode}")
    } catch {
      case e: Exception =>
        Left(s"Exception occurred: ${e.getMessage}")
    }
  }
}
