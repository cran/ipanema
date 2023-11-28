#' @title get_survey_id
#'
#' @description Get numerical LimeSurvey ID of the survey with the given title.
#'
#' @param survey_title TItle of the survey. String.
#'
#' @return An integer Survey ID which can be used as a parameter in
#' `get_survey_data()`
#'
#' @examples
#' # This example assumes a locally hosted `LimeSurvey` instance using a locally
#' # hosted MySQL server.
#' # On this `LimeSurvey` instance, there is a survey with the title 'mysurvey'.
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
#' survey_id <- get_survey_id('mysurvey')
#' df_data <- get_survey_data(survey_id)
#' }
#'
#' @export

get_survey_id <- function(
    survey_title
) {
  print
  if (!exists('limesurvey_session_key', envir = ipanema_cache)) {
    stop(paste0(
        'You need to call `connect_to_limesurvey()` before calling any other ',
        'ipanema functions.'
    ))
  }

  # Connect to MySQL
  conn <- dbConnect(
    MySQL(),
    user = ipanema_cache$mysql_username,
    password = ipanema_cache$mysql_password,
    dbname = ipanema_cache$mysql_dbname,
    host = ipanema_cache$mysql_host,
    port = ipanema_cache$mysql_port
  )

  survey_id <- dbGetQuery(
    conn,
    paste0(
      'SELECT surveyls_survey_id ',
      'FROM surveys_languagesettings ',
      "WHERE surveyls_title = '", survey_title, "'"
    )
  )$surveyls_survey_id[1]

  dbDisconnect(conn)
  return(survey_id)
}
