#' @title base64_to_df
#'
#' @description Convert a base64 representation of a CSV table into a
#' `data.frame` object.
#'
#' @param x The base64-encoded CSV string
#'
#' @return A `data.frame` object containing the data from `x`.
#'
#' @importFrom base64enc base64decode
#' @importFrom utils read.csv

base64_to_df <- function(x) {
  raw_csv <- rawToChar(
    base64decode(x)
  )

  return(
    read.csv(
      textConnection(raw_csv),
      stringsAsFactors = FALSE,
      sep = ";"
    )
  )
}
