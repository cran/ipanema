#' @title get_survey_data
#'
#' @description Get collected data from a specific survey on the connected
#' `LimeSurvey` instance.
#' Fails horribly saying
#' ```
#' Error in make.names(col.names, unique = TRUE) :
#' invalid multibyte string 1
#' ```
#' if data is empty.
#'
#' @param survey_id ID of the survey from which the collected data shall be
#' extracted.
#' 6-digit integer.
#'
#' @return A `data.frame` object containing the survey data.
#' Column names follow a dot-based naming scheme:
#' <group title>.<subquestion title>.
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
#' df_data <- get_survey_data(123456)
#' }
#'
#' @export

get_survey_data <- function(
    survey_id
) {
  print
  if (!exists('limesurvey_session_key', envir = ipanema_cache)) {
    stop(paste0(
        'You need to call `connect_to_limesurvey()` before calling any other ',
        'ipanema functions.'
    ))
  }

  params <- list(
    iSurveyID = survey_id,
    sDocumentType = 'csv',
    sLanguageCode = NULL,
    sCompletionStatus = 'complete',
    sHeadingType = 'code',
    sResponseType = 'short'
  )

  results <- limesurvey_api_call(
    method = 'export_responses',
    params = params
  )
  df_raw <- base64_to_df(
    unlist(
      results
    )
  )

  # Remove trailing dots from item columns
  names(df_raw) <- names(df_raw) %>%
    sapply(function(colname) {
      if(endsWith(colname, '.')) {
        substring(colname, 1, nchar(colname) - 1)
      } else {
        colname
      }
    })

  df_tidy <- fix_column_data_types(df_raw)

  return (df_tidy)
}
