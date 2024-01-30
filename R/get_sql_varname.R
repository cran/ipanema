#' @title get_sql_varname
#'
#' @description Get the internal SQL field name (e.g. "697929X4X21") to a
#' question from a specific survey in the dataset.
#'
#' @param question_code Code by which to identify the question.
#' Follows a dot-based naming scheme:
#' <group title>.<subquestion title>.
#'
#' @param survey_id Survey-ID of the survey from which to select the question.
#'
#' @return `character` object containing the field name
#'
#' @examples
#' # This example assumes a locally hosted `LimeSurvey` instance using a locally
#' # hosted MySQL server.
#' # On this `LimeSurvey` instance, there is a survey with the ID 123456.
#' # In this survey, a multiple-choice question identified by the code "bdi.01"
#' # is used.
#' # For this question, this example retrieves name of the SQL table field in
#' # which `LimeSurvey` internally stores the responses to this question.
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
#' q_varname <- get_sql_varname("bdi.01", 123456)
#' }
#'
#' @import magrittr
#' @importFrom DBI dbConnect dbDisconnect dbGetQuery
#' @importFrom dplyr filter
#'
#' @export

get_sql_varname <- function(
    question_code,
    survey_id
) {
  if (!exists('limesurvey_session_key', envir = ipanema_cache)) {
    stop(paste0(
      'You need to call `connect_to_limesurvey()` before calling any other ',
      'ipanema functions.'
    ))
  }

  # SQL connect
  conn <- dbConnect(
    MySQL(),
    user = ipanema_cache$mysql_username,
    password = ipanema_cache$mysql_password,
    dbname = ipanema_cache$mysql_dbname,
    host = ipanema_cache$mysql_host,
    port = ipanema_cache$mysql_port
  )

  if (grepl("\\.", question_code)) {
    # Der `question_code` schaut ja z.B. so aus: "sedi.01".
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

    # Eigenschaften der group bestimmen
    parent_ids <- dbGetQuery(
      conn,
      paste0(
        'SELECT gid, qid ',
        'FROM ', ipanema_cache$mysql_dbname, '.questions ',
        "WHERE type IN ('F', 'K') AND ",
        'title = ', "'", group, "' AND ",
        "sid = ", survey_id
      )
    )
    parent_gid <- parent_ids[1, 'gid']
    parent_qid <- parent_ids[1, 'qid']

    if (
      is.na(parent_gid) ||
      is.na(parent_qid)
    ) {
      dbDisconnect(conn)
      return(NA)
    }

    # Eigenschaften der question bestimmen
    df_questions <- dbGetQuery(
      conn,
      paste0(
        'SELECT qid, sid, gid, title ',
        'FROM ', ipanema_cache$mysql_dbname, '.questions ',
        "WHERE type NOT IN ('F', 'K') AND ",
        "parent_qid = ", parent_qid, " ",
        "ORDER BY qid"
      )
    )
    this_question <-
      df_questions %>%
      dplyr::filter(title == question)

    question_index <-
      match(
        question,
        df_questions$title
      )
    question_index_str <-
      sprintf('%02d', question_index)

    if (
      is.na(this_question$sid) ||
      is.na(this_question$qid)
    ) {
      dbDisconnect(conn)
      return(NA)
    }

    out <- paste0(
      this_question$sid,
      'X',
      this_question$gid,
      'X',
      parent_qid,
      question_index_str
    )
  } else {
    # Text dieser spezifischen Question
    question_ids <- dbGetQuery(
      conn,
      paste0(
        'SELECT gid, qid, sid ',
        'FROM ', ipanema_cache$mysql_dbname, '.questions ',
        "WHERE title = '", question_code, "' AND ",
        "parent_qid = 0 AND ",
        "sid = ", survey_id
      )
    )
    question_gid <- question_ids[1, 'gid']
    question_qid <- question_ids[1, 'qid']
    question_sid <- question_ids[1, 'sid']

    if (
      is.na(question_sid) ||
      is.na(question_gid) ||
      is.na(question_qid)
    ) {
      dbDisconnect(conn)
      return(NA)
    }

    out <- paste0(
      c(
        question_sid,
        question_gid,
        question_qid
      ),
      collapse = 'X'
    )
  }

  dbDisconnect(conn)
  return(out)
}
