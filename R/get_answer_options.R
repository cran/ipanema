#' @title get_answer_options
#'
#' @description Get the answer options to a question with pre-defined answer
#' options (e.g. a multiple choice question).
#'
#' @param question_code Code by which to identify the question.
#' Follows a dot-based naming scheme:
#' <group title>.<subquestion title>.
#'
#' @return `data.frame` object with the columns `code` and `answer` in which
#' each row represents one answer option where `code` is the encoded value (as
#' found in datasets exported by `get_survey_data()` and `answer` is the answer
#' option text as seen by survey users).
#'
#' @examples
#' # This example assumes a locally hosted `LimeSurvey` instance using a locally
#' # hosted MySQL server.
#' # On this `LimeSurvey` instance, there is a survey with the ID 123456.
#' # In this survey, a multiple-choice question identified by the code "bdi.01"
#' # is used.
#' # For this question, this example retrieves the possible answer options.
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
#' answer_options <- get_answer_options("bdi.01")
#' }
#'
#' @importFrom DBI dbConnect dbDisconnect dbGetQuery
#'
#' @export

get_answer_options <- function(question_code) {
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

  if (grepl("\\.", question_code)) {
    # Der `question_code` schaut ja z.B. so aus: "sedi.SQ001".
    # Das will ich zuerst in die linke Hälfte (question group) und die rechte
    # Hälfte (question) zerlegen:
    dot_index <- which(strsplit(question_code, '')[[1]] == '.')
    group <- substring(
      question_code,
      first = 1,
      last = dot_index - 1
    )
    question <- substring(
      question_code,
      first = dot_index + 1
    )

    # ID der group bestimmen
    parent_qid <- dbGetQuery(
      conn,
      paste0(
        'SELECT qid ',
        'FROM questions ',
        "WHERE type = 'F' AND ",
        'title = ', "'", group, "'"
      )
    )$qid

    df_answers <- dbGetQuery(
      conn,
      paste0(
        'SELECT code, answer ',
        'FROM answers ',
        'WHERE qid = ', parent_qid
      )
    )
  } else{
    # ID der question bestimmen
    question_qid <- dbGetQuery(
      conn,
      paste0(
        'SELECT qid ',
        'FROM questions ',
        "WHERE type <> 'F' AND ",
        'title = ', "'", question_code, "'"
      )
    )$qid

    df_answers <- dbGetQuery(
      conn,
      paste0(
        'SELECT code, answer ',
        'FROM answers ',
        'WHERE qid = ', question_qid
      )
    )
  }

  dbDisconnect(conn)

  return(df_answers)
}
