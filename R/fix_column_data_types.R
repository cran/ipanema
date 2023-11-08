#' @title fix_column_data_types
#'
#' @description Freshly exported data has all item-data columns as type
#' "character".
#' This function converts these columns to ideal types (e.g. integer).
#' Currently simply converts all multiple-choice columns to integer.
#' Future task: Add conversion to other data types as needed.
#'
#' @param df_in The `data.frame` object to fix.
#'
#' @return A `data.frame` object containing the data from `df_in` but with
#' fixed column data types.
#'
#' @importFrom DBI dbGetQuery
#' @importFrom dplyr mutate mutate_at rowwise ungroup vars
#' @import magrittr

fix_column_data_types <- function(
    df_in
) {
  conn <- DBI::dbConnect(
    RMySQL::MySQL(),
    user = ipanema_cache$mysql_username,
    password = ipanema_cache$mysql_password,
    dbname = ipanema_cache$mysql_dbname,
    host = ipanema_cache$mysql_host,
    port = ipanema_cache$mysql_port
  )

  df_questions <- dbGetQuery(
    conn,
    paste0(
      'SELECT parent_qid, title ',
      'FROM questions ',
      'WHERE parent_qid ',
      'IN ( ',
      '   SELECT qid ',
      '   FROM answers ',
      ')'
    )
  )

  df_parents <- dbGetQuery(
    conn,
    paste0(
      'SELECT qid, title ',
      'FROM questions ',
      'WHERE qid ',
      'IN (',
      '    SELECT qid ',
      '    FROM answers ',
      ')'
    )
  )

  # New column: Dot-format question codes for all existing questions
  df_questions <- df_questions %>%
    rowwise() %>%
    mutate(questioncode = paste0(
      df_parents[parent_qid, 'title'],
      '.',
      title
    )) %>%
    ungroup()

  columns_to_fix <- base::intersect(
    df_questions$questioncode,
    names(df_in)
  )

  df_out <- df_in %>%
    dplyr::mutate_at(
      .vars = dplyr::vars(columns_to_fix),
      .funs = as.integer
    )

  DBI::dbDisconnect(conn)

  return(df_out)
}

utils::globalVariables(c(
  'parent_qid',
  'title'
))
