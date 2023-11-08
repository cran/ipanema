#' @title limesurvey_api_call
#'
#' @description Perform a call to the `LimeSurvey` RPC API.
#'
#' @param method Name of the API method to call.
#' A complete list of methods can be found here:
#' https://api.limesurvey.org/classes/remotecontrol_handle.html
#' @param params Parameters to pass to the API
#' @param ... Additional parameters passed from above
#'
#' @return A list containing the de-serialized response.
#'
#' @importFrom httr content content_type_json POST
#' @importFrom jsonlite fromJSON toJSON

limesurvey_api_call <- function(method, params = list(), ...) {
  if (!is.list(params)) {
    stop('params must be a list.')
  }

  if (!exists('limesurvey_session_key', envir = ipanema_cache)) {
    stop(paste0(
      'You need to call `connect_to_limesurvey()` before calling any other ',
      'ipanema functions.'
    ))
  }

  key.list <- list(
    sSessionKey = ipanema_cache$limesurvey_session_key
  )
  params.full <- c(
    key.list,
    params
  )

  body.json <- list(
    method = method,
    id = ' ',
    params = params.full
  )

  res <- POST(
    ipanema_cache$limesurvey_api_url,
    content_type_json(),
    body = toJSON(
      body.json,
      auto_unbox = TRUE
    ),
    ...
  )

  return(
    fromJSON(
      content(
        res,
        as = 'text',
        encoding = 'utf-8'
      )
    )$result
  )
}
