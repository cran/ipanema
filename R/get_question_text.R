#' @title get_question_text
#'
#' @description Get the question text (e.g. "How have you been feeling?") to a
#' question in the dataset.
#'
#' @param question_code Code by which to identify the question.
#' Follows a dot-based naming scheme:
#' <group title>.<subquestion title>.
#'
#' @return `character` object containing the question text
#'
#' @examples
#' # This example assumes a locally hosted `LimeSurvey` instance using a locally
#' # hosted MySQL server.
#' # On this `LimeSurvey` instance, there is a survey with the ID 123456.
#' # In this survey, a multiple-choice question identified by the code "bdi.01"
#' # is used.
#' # For this question, this example retrieves the question text which was shown
#' # to the user when answering the questionnaire.
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
#' q_text <- get_question_text("bdi.01")
#' }
#'
#' @importFrom DBI dbGetQuery
#'
#' @export

get_question_text <- function(question_code) {
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
    getOption('limesurvey_mysql_connection'),
    paste0(
      'SELECT qid ',
      'FROM limesurvey.questions ',
      "WHERE type = 'F' AND ",
      'title = ', "'", group, "'"
    )
  )$qid

  # Text dieser spezifischen Question
  question_text <- dbGetQuery(
    getOption('limesurvey_mysql_connection'),
    paste0(
      'SELECT question ',
      'FROM limesurvey.questions ',
      "WHERE type = 'T' AND ",
      "title = '", question, "' AND ",
      "parent_qid = ", parent_qid
    )
  )$question

  return(question_text)
}
