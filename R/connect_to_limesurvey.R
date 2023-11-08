#' @title connect_to_limesurvey
#'
#' @description Connect to `LimeSurvey` instance via the RPC and a direct MySQL
#' connection.
#' Store the RPC session key in `options('limesurvey_session_key')`.
#' Store the MySQL connection object in
#' `options('limesurvey_mysql_connection')`.
#' Store the RPC URL in `options('limesurvey_api_url')`.
#'
#' @param api_url URL to the `LimeSurvey` RPC, e.g.
#' 'http://localhost/index.php/admin/remotecontrol'
#' @param limesurvey_username Username for the `LimeSurvey` API
#' @param limesurvey_password Password for the `LimeSurvey` API
#' @param mysql_host Hostname of the MySQL server used by `LimeSurvey`
#' @param mysql_port Port on which the MySQL server listens for connections
#' @param mysql_dbname Name of the database on the MySQL server which is used by
#' `LimeSurvey`
#' @param mysql_username Username for the MySQL server
#' @param mysql_password Password for the MySQL server
#'
#' @return No return value, called for side effects
#'
#' @importFrom DBI dbConnect dbGetQuery
#' @importFrom RMySQL MySQL
#'
#' @examples
#' # This example assumes a locally hosted `LimeSurvey` instance using a locally
#' # hosted MySQL server
#' \dontrun{
#' connect_to_limesurvey(
#'   api_url = 'https://localhost/index.php/admin/remotecontrol',
#'   limesurvey_username = 'admin',
#'   limesurvey_password = '1234admin',
#'   mysql_host = '127.0.0.1',
#'   mysql_port = 3306,
#'   mysql_dbname = 'limesurvey',
#'   mysql_username = 'lime',
#'   mysql_password = '1234lime'
#' )
#' }
#'
#' @export

connect_to_limesurvey <- function(
    api_url,
    limesurvey_username,
    limesurvey_password,
    mysql_host,
    mysql_port,
    mysql_dbname,
    mysql_username,
    mysql_password
) {
  # SQL ------------------------------------------
  ipanema_cache$mysql_username <- mysql_username
  ipanema_cache$mysql_password <- mysql_password
  ipanema_cache$mysql_dbname <- mysql_dbname
  ipanema_cache$mysql_host <- mysql_host
  ipanema_cache$mysql_port <- mysql_port

  # LimeSurvey -----------------------------------
  ipanema_cache$limesurvey_api_url <- api_url

  body.json = list(
    method = 'get_session_key',
    id = 1,
    params = c(
      username = limesurvey_username,
      password = limesurvey_password
    )
  )

  res <- POST(
    api_url,
    content_type_json(),
    body = jsonlite::toJSON(
      body.json,
      auto_unbox = TRUE
    )
  )

  session_key <- as.character(
    jsonlite::fromJSON(
      content(
        res,
        encoding = 'utf-8'
      )
    )$result
  )

  ipanema_cache$limesurvey_session_key <- session_key
}

ipanema_cache <- new.env(parent = emptyenv())
