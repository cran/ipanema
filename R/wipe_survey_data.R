#' @title wipe_survey_data
#'
#' @description Delete all data collected by this survey.
#'
#' @param survey_id ID of the survey from which the collected data shall be
#' deleted.
#' 6-digit integer.
#'
#' @return Nothing. Function is called for side effects on SQL table.
#'
#' @examples
#' # This example assumes a locally hosted `LimeSurvey` instance using a locally
#' # hosted MySQL server.
#' # On this `LimeSurvey` instance, there is a survey with the ID 123456.
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
#'
#' wipe_survey_data(123456)
#' }
#'
#' @export

wipe_survey_data <- function(
    survey_id
) {
  if (!exists('limesurvey_session_key', envir = ipanema_cache)) {
    stop(paste0(
      'You need to call `connect_to_limesurvey()` before calling any other ',
      'ipanema functions.'
    ))
  }

  conn <- dbConnect(
    MySQL(),
    user = ipanema_cache$mysql_username,
    password = ipanema_cache$mysql_password,
    dbname = ipanema_cache$mysql_dbname,
    host = ipanema_cache$mysql_host,
    port = ipanema_cache$mysql_port
  )

  dbGetQuery(
    conn,
    paste0(
      'DELETE FROM ',
      ipanema_cache$mysql_dbname,
      '.survey_',
      survey_id
    )
  )

  dbDisconnect(conn)
}
